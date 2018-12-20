#!/bin/bash

# Relaunch the last command in gdb
# In case of multiple commands joined by && will launch the very last one
gdb-last-command ()
{
  cmd=$(fc -ln -2 -2 | sed -e 's/^[[:space:]]*//')
  cmd=$(echo "${cmd// && /$'\n'}" | tail -1)
  bin=$(cut -d ' ' -f 1 <<< "$cmd")
  arg=$(cut -d ' ' -f 2- <<< "$cmd")
  gdb "$bin" -ex "run $arg"
}

gdb-backtrace ()
{
  # Run a program in gdb and send detailed backtrace to gdb.bt file
  # See: http://blog.cryptomilk.org/2010/12/23/gdb-backtrace-to-file
  echo 0 | gdb -batch-silent -ex "run" -ex "set logging overwrite on" -ex "set logging file gdb.bt" -ex "set logging on" -ex "set pagination off" -ex "handle SIG33 pass nostop noprint" -ex "echo backtrace:\\n" -ex "backtrace full" -ex "echo \\n\\nregisters:\\n" -ex "info registers" -ex "echo \\n\\ncurrent instructions:\\n" -ex "x/16i \$pc" -ex "echo \\n\\nthreads backtrace:\\n" -ex "thread apply all backtrace" -ex "set logging off" -ex "quit" --args "$@"
}
