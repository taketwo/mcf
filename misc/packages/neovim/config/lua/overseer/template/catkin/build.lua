return {
  name = 'Catkin build',
  builder = function(params)
    return {
      cmd = { 'catkin' },
      args = { 'build', '--no-status', '--this' },
      -- name = 'build current catkin package',
      -- cwd = '/tmp',
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
  desc = 'Build catkin workspace',
  tags = { require('overseer').TAG.BUILD },
}
