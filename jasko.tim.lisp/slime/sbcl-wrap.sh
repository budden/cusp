#! /bin/bash

# Silly wrapper script that unequivocally kills SBCL if pipes are closed
# or this script is terminated with SIGHUP.
#
# Sbcl's default behaviour in such circumstance is to spin endlessly, which
# isn't quite desirable when developing over remote SSH connection.

COMMAND=/usr/local/bin/sbcl

SSHD_PID=$PPID

($COMMAND $*) 0<&0 1>&1 2>&2 &

SBCL_PID=`jobs -p`

(while kill -0 $SSHD_PID && kill -0 $SBCL_PID; do sleep 1 ; done ; kill -9 $SBCL_PID ;  kill -9 $$ ) 1>/dev/null 2>/dev/null &
#WATCHER_PID=`jobs -p | tail -1`

trap "kill -9 $SBCL_PID " 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32

wait
