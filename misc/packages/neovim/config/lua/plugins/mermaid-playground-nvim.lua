return {
  'selimacerbas/mermaid-playground.nvim',
  dependencies = {
    {
      'barrett-ruth/live-server.nvim',
      cmd = { 'LiveServerStart', 'LiveServerStop' },
      build = 'fnm exec --using=node-nvim npm install --location=global live-server',
      config = true,
    },
  },
  cmd = { 'MermaidPreviewStart' },
  config = function()
    require('mermaid_playground').setup({
      workspace_dir = nil,
      index_name = 'index.html',
      diagram_name = 'diagram.mmd',
      overwrite_index_on_start = false,
      auto_refresh = true,
      auto_refresh_events = { 'InsertLeave', 'TextChanged', 'TextChangedI', 'BufWritePost' },
      debounce_ms = 450,
      notify_on_refresh = false,
    })
  end,
}
