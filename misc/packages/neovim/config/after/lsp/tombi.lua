return {
  -- List of supported settings: https://tombi-toml.github.io/tombi/docs/configuration
  -- Lowest-priority fallback: any project's own tombi.toml takes precedence.
  settings = {
    tombi = {
      format = {
        rules = {
          ['trailing-comment-alignment'] = true,
        },
      },
    },
  },
}
