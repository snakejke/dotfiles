#!/bin/sh

if [ ! -e "/tmp/dbus-$USER-env" ]; then
       echo "Creating new dbus session on /tmp/dbus-$USER-env"
       export $(dbus-launch)
       echo "${DBUS_SESSION_BUS_ADDRESS}" > /tmp/dbus-$USER-env
       echo "Dbus session address is: ${DBUS_SESSION_BUS_ADDRESS}"
else
       echo "Using dbus session address from /tmp/dbus-$USER-env"
       export DBUS_SESSION_BUS_ADDRESS="$(cat /tmp/dbus-$USER-env)"
       echo "Dbus session address is: ${DBUS_SESSION_BUS_ADDRESS}"
fi
if [ -n "$1" ]; then
       $@
fi
