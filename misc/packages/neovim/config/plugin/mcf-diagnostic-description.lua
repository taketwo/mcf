function _G.show_diagnostic_description()
  local diagnostics = vim.diagnostic.get(0, { lnum = vim.fn.line('.') - 1 })
  if #diagnostics == 0 then return end
  for _, v in ipairs(diagnostics) do
    local description = 'no description available'
    if v.user_data.lsp.codeDescription then description = v.user_data.lsp.codeDescription.href end
    print(string.format('Diagnostic description: %s', description))
  end
end

vim.cmd('command! DiagnosticDescription lua _G.show_diagnostic_description()')
