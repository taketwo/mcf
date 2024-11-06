function _G.show_diagnostic_description()
  local diagnostics = vim.diagnostic.get(0, { lnum = vim.fn.line('.') - 1 })
  if #diagnostics == 0 then return end
  for _, v in ipairs(diagnostics) do
    print(string.format('Diagnostic description: %s', v.user_data.lsp.codeDescription.href))
  end
end

vim.cmd('command! DiagnosticDescription lua _G.show_diagnostic_description()')
