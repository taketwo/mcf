function _G.show_diagnostic_description()
  local diagnostics = vim.lsp.diagnostic.get_line_diagnostics()
  if #diagnostics == 0 then return end
  for _, v in ipairs(diagnostics) do
    print(string.format('Diagnostic description: %s', v.codeDescription.href))
  end
end

vim.cmd('command! DiagnosticDescription lua _G.show_diagnostic_description()')
