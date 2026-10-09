# Copy the command line Herdr asks for with prefix+y.
# herdr-copy injects Ctrl-x Ctrl-y; this widget copies the line bash is editing
# and leaves the buffer in place. HERDR_ENV is set only inside a Herdr pane.
#
# OSC 52 is how the text reaches a Herdr client attached from outside the
# machine that runs the server. xsel covers a server on this desktop.

if [[ -n ${HERDR_ENV:-} ]]; then
  __herdr_copy_line() {
    local payload
    payload=$(printf '%s' "$READLINE_LINE" | base64 -w0 2>/dev/null ||
      printf '%s' "$READLINE_LINE" | base64 | tr -d '\n')
    printf '\033]52;c;%s\a' "$payload"
    if command -v xsel >/dev/null 2>&1; then
      printf '%s' "$READLINE_LINE" | xsel --clipboard --input 2>/dev/null || true
    fi
  }
  bind -x '"\C-x\C-y": __herdr_copy_line'
fi
