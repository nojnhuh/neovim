local M = {}

local protocol = require('vim.lsp.protocol')
local uv = vim.loop

---@private
---Parses the raw pattern into a number of Lua-native patterns.
---
---@param pattern string The raw glob pattern
---@return table A list of Lua patterns. A match with any of them matches the input glob pattern.
local function parse(pattern)
  local patterns = { '' }

  local path_sep = '[/\\]'
  local non_path_sep = '[^/\\]'

  local function append(chunks)
    local new_patterns = {}
    for _, p in ipairs(patterns) do
      for _, chunk in ipairs(chunks) do
        table.insert(new_patterns, p .. chunk)
      end
    end
    patterns = new_patterns
  end

  local function split(s, sep)
    local segments = {}
    local segment = ''
    local in_braces = false
    local in_brackets = false
    for i = 1, #s do
      local c = string.sub(s, i, i)
      if c == sep and not in_braces and not in_brackets then
        table.insert(segments, segment)
        segment = ''
      else
        if c == '{' then
          in_braces = true
        elseif c == '}' then
          in_braces = false
        elseif c == '[' then
          in_brackets = true
        elseif c == ']' then
          in_brackets = false
        end
        segment = segment .. c
      end
    end
    if segment ~= '' then
      table.insert(segments, segment)
    end
    return segments
  end

  local function escape(c)
    if
      c == '?'
      or c == '.'
      or c == '('
      or c == ')'
      or c == '%'
      or c == '['
      or c == ']'
      or c == '*'
      or c == '+'
      or c == '-'
    then
      return '%' .. c
    end
    return c
  end

  local segments = split(pattern, '/')
  for i, segment in ipairs(segments) do
    local last_seg = i == #segments
    if segment == '**' then
      local chunks = {
        path_sep .. '-',
        '.-' .. path_sep,
      }
      if last_seg then
        chunks = { '.-' }
      end
      append(chunks)
    else
      local in_braces = false
      local brace_val = ''
      local in_brackets = false
      local bracket_val = ''
      for j = 1, #segment do
        local char = string.sub(segment, j, j)
        if char ~= '}' and in_braces then
          brace_val = brace_val .. char
        else
          if in_brackets and (char ~= ']' or bracket_val == '') then
            local res
            if char == '-' then
              res = char
            elseif bracket_val == '' and char == '!' then
              res = '^'
            elseif char == '/' then
              res = ''
            else
              res = escape(char)
            end
            bracket_val = bracket_val .. res
          else
            if char == '{' then
              in_braces = true
            elseif char == '[' then
              in_brackets = true
            elseif char == '}' then
              local choices = split(brace_val, ',')
              local parsed_choices = {}
              for _, choice in ipairs(choices) do
                table.insert(parsed_choices, parse(choice))
              end
              append(vim.tbl_flatten(parsed_choices))
              in_braces = false
              brace_val = ''
            elseif char == ']' then
              append({ '[' .. bracket_val .. ']' })
              in_brackets = false
              bracket_val = ''
            elseif char == '?' then
              append({ non_path_sep })
            elseif char == '*' then
              append({ non_path_sep .. '-' })
            else
              append({ escape(char) })
            end
          end
        end
      end

      if not last_seg and (segments[i + 1] ~= '**' or i + 1 < #segments) then
        append({ path_sep })
      end
    end
  end

  return patterns
end

---@private
--- Implementation of LSP 3.17.0's pattern matching: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#pattern
--- Modeled after VSCode's implementation: https://github.com/microsoft/vscode/blob/0319eed971719ad48e9093daba9d65a5013ec5ab/src/vs/base/common/glob.ts#L509
---
---@param pattern string|table The glob pattern (raw or parsed) to match.
---@param s string The string to match against pattern.
---@return bool Whether or not pattern matches s.
function M._match(pattern, s)
  if type(pattern) == 'string' then
    pattern = parse(pattern)
  end
  -- Since Lua's built-in string pattern matching does not have an alternate
  -- operator like '|', `parse` will construct one pattern for each possible
  -- alternative. Any pattern that matches thus matches the glob.
  for _, p in ipairs(pattern) do
    if s:match('^' .. p .. '$') then
      return true
    end
  end
  return false
end

---@private
--- Joins filepath elements by platform-specific separator.
---
---@param args ... The path elements. The first element must be absolute.
---@return string The joined path.
local function filepath_join(...)
  local dir = ...
  return table.concat({ ... }, dir:match('^([a-zA-Z]:)(.*)') and '\\' or '/')
end

-- Cache of libuv handles per directory, per LSP client, per registration ID.
local watched_dirs = {}

local change_queue = {}
local change_cache = {}
local queue_timers = {}

-- Recursive flag for libuv watcher not implemented on linux
local recursive_watch = vim.fn.has('mac') == 1 or vim.fn.has('win32') == 1

-- kqueue requires one watch for every file
local watch_each_file = vim.fn.has('bsd') == 1

---@private
--- Creates callbacks invoked on watched file events.
---
---@param dir string Absolute path to the watched directory.
---@param client_id number The LSP client's ID.
---@param reg_id string The ID used to register the request.
---@return function The callback invoked on watched file events.
local function get_callback(dir, client_id, reg_id)
  return function(filename, events)
    local path = filepath_join(dir, filename)

    local type = events.change and protocol.FileChangeType.Changed or 0
    if events.rename then
      local _, err, errname = uv.fs_stat(path)
      if errname == 'ENOENT' then
        type = protocol.FileChangeType.Deleted
      else
        assert(not err, err)
        type = protocol.FileChangeType.Created
      end
    end

    local matches_filter = watch_each_file
    local filters = watched_dirs[path].callbacks[client_id][reg_id].filters
    for _, filter in ipairs(filters) do
      if M._match(filter.pattern, path) and math.floor(filter.kind / (2 ^ (type - 1))) % 2 == 1 then
        matches_filter = true
        break
      end
    end
    if not matches_filter then
      return
    end

    local change = {
      uri = vim.uri_from_fname(path),
      type = type,
    }
    change_cache[client_id] = change_cache[client_id] or {}
    local last_type = change_cache[client_id][change.uri]
    if not last_type or last_type ~= change.type then
      change_queue[client_id] = change_queue[client_id] or {}
      table.insert(change_queue[client_id], change)
      change_cache[client_id][change.uri] = change.type
    end
    if not queue_timers[client_id] then
      local t = uv.new_timer()
      t:start(100, 0, function()
        t:close()
        vim.lsp.get_client_by_id(client_id).notify('workspace/didChangeWatchedFiles', {
          changes = change_queue[client_id],
        })
        change_queue[client_id] = nil
        change_cache[client_id] = nil
        queue_timers[client_id] = nil
      end)
      queue_timers[client_id] = t
    end
  end
end

-- Never create watchers for directories matching these patterns. Similar
-- to vscode's files.watcherExclude setting.
local excludes = {
  parse('**/.git/objects/**'),
  parse('**/.git/subtree-cache/**'),
  parse('**/node_modules/*/**'),
  parse('**/.hg/store/**'),
}

---@private
--- Creates libuv fs_events handles.
---
---@param path string Absolute path to watch.
---@param pattern string|table The LSP glob pattern (raw or parsed) to match against.
---@param kind number The LSP WatchKind value.
---@param client_id number The LSP client's ID.
---@param reg_id string The ID used to register the request.
local function fsevent_ensure_recursive(path, pattern, kind, client_id, reg_id)
  if watch_each_file and not M._match(pattern, path) then
    if not watched_dirs[path] then
      local fsevent, fserr = uv.new_fs_event()
      assert(not fserr, fserr)
      watched_dirs[path] = {
        fsevent = fsevent,
        callbacks = {},
      }
      fsevent:start(path, { recursive = recursive_watch }, function(err, filename, events)
        assert(not err, err)
        for _, reg in pairs(watched_dirs[path].callbacks[client_id]) do
          reg.callback(filename, events)
        end
      end)
    end

    watched_dirs[path].callbacks[client_id] = watched_dirs[path].callbacks[client_id] or {}
    watched_dirs[path].callbacks[client_id][reg_id] = watched_dirs[path].callbacks[client_id][reg_id]
    or {}
    watched_dirs[path].callbacks[client_id][reg_id].callback = watched_dirs[path].callbacks[client_id][reg_id].callback
    or get_callback(path, client_id, reg_id)
    watched_dirs[path].callbacks[client_id][reg_id].filters = watched_dirs[path].callbacks[client_id][reg_id].filters
    or {}
    table.insert(
    watched_dirs[path].callbacks[client_id][reg_id].filters,
    { pattern = pattern, kind = kind }
    )
  end

  if recursive_watch then
    return
  end

  local scan, err, errname = uv.fs_scandir(path)
  if errname == "ENOTDIR" then
    return
  end
  assert(not err, err)
  while true do
    local ret = { uv.fs_scandir_next(scan) }
    if #ret == 0 then
      break
    end
    if #ret == 3 then
      assert(not ret[2], ret[2])
    end -- error check
    local name, type = ret[1], ret[2]
    if watch_each_file or type == 'directory' then
      local subpath = filepath_join(path, name)
      local include = true
      for _, exclude in ipairs(excludes) do
        if M._match(exclude, subpath) then
          include = false
          break
        end
      end
      if not include then
        break
      end
      fsevent_ensure_recursive(subpath, pattern, kind, client_id, reg_id)
    end
  end
end

--- Registers the workspace/didChangeWatchedFiles capability dynamically.
---
---@param reg table LSP Registration object.
---@param ctx table Context from the |lsp-handler|.
function M.register(reg, ctx)
  local client = vim.lsp.get_client_by_id(ctx.client_id)
  for _, w in ipairs(reg.registerOptions.watchers) do
    local glob_patterns = {}
    if type(w.globPattern) == 'string' then
      for _, folder in ipairs(client.workspace_folders) do
        table.insert(glob_patterns, { baseUri = folder.uri, pattern = w.globPattern })
      end
    else
      table.insert(glob_patterns, w.globPattern)
    end
    for _, glob_pattern in ipairs(glob_patterns) do
      local pattern = parse(glob_pattern.pattern)
      local base_dir = nil
      if type(glob_pattern.baseUri) == 'string' then
        base_dir = glob_pattern.baseUri
      elseif type(glob_pattern.baseUri) == 'table' then
        base_dir = glob_pattern.baseUri.uri
      end
      assert(base_dir, "couldn't identify root of watch")
      base_dir = vim.uri_to_fname(base_dir)
      local kind = w.kind
        or protocol.WatchKind.Create + protocol.WatchKind.Change + protocol.WatchKind.Delete

      fsevent_ensure_recursive(base_dir, pattern, kind, ctx.client_id, reg.id)
    end
  end
end

--- Unregisters the workspace/didChangeWatchedFiles capability dynamically.
---
---@param unreg table LSP Unregistration object.
---@param ctx table Context from the |lsp-handler|.
function M.unregister(unreg, ctx)
  for dir, w in pairs(watched_dirs) do
    w.callbacks[ctx.client_id][unreg.id] = nil
    if not next(w.callbacks[ctx.client_id]) then
      w.callbacks[ctx.client_id] = nil
      if not next(w.callbacks) then
        local _, err = w.fsevent:stop()
        assert(not err, err)
        watched_dirs[dir] = nil
      end
    end
  end
end

return M
