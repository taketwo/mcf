--- Lualine component that shows a spinner when CodeCompanion is processing a request

local M = require('lualine.component'):extend()

--- Spinner animation frames
---@type string[]
local spinner_symbols = {
  '⠋',
  '⠙',
  '⠹',
  '⠸',
  '⠼',
  '⠴',
  '⠦',
  '⠧',
  '⠇',
  '⠏',
}
local spinner_symbols_len = #spinner_symbols

--- Flag indicating whether CodeCompanion is currently processing a request
---@type boolean
local is_processing = false

--- Current index in the spinner animation sequence
---@type number
local spinner_index = 1

--- Flag to prevent setting up autocmds multiple times
---@type boolean
local autocmd_setup = false

---@param options table Component configuration options
function M:init(options)
  M.super.init(self, options)

  -- Setup autocmds to track CodeCompanion processing state
  if not autocmd_setup then
    local group = vim.api.nvim_create_augroup('CodeCompanionLualineHooks', {})
    vim.api.nvim_create_autocmd({ 'User' }, {
      pattern = 'CodeCompanionRequest*',
      group = group,
      callback = function(request)
        if request.match == 'CodeCompanionRequestStarted' then
          is_processing = true
        elseif request.match == 'CodeCompanionRequestFinished' then
          is_processing = false
        end
      end,
    })
    autocmd_setup = true
  end
end

--- Returns the current spinner frame or nil if not processing
---@return string|nil Current spinner character or nil
function M:update_status()
  if is_processing then
    spinner_index = (spinner_index % spinner_symbols_len) + 1
    return spinner_symbols[spinner_index]
  else
    return nil
  end
end

return M
