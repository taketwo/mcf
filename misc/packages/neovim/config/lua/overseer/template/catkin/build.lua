return {
  name = 'Catkin build',
  builder = function(params)
    return {
      cmd = { 'catkin' },
      args = { 'build', '--no-status', vim.b.ros_package_name },
      components = {
        {
          'on_output_parse',
          parser = {
            diagnostics = {
              'sequence',
              { 'extract', { append = false }, 'ld: error: (.*)', 'text' },
              { 'extract', '>>> referenced by .+%((.+):(%d+)%)$', 'filename', 'lnum' },
            },
          },
        },
        'on_result_diagnostics',
        'on_result_diagnostics_trouble',
        'on_exit_set_status',
      },
    }
  end,
  desc = 'package of the active file',
  tags = { require('overseer').TAG.BUILD },
  condition = {
    callback = function() return vim.b.ros_package_name ~= nil end,
  },
}
