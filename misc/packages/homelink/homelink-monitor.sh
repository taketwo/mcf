#!/usr/bin/env bash

INTERFACE="wg0"
HOME_TARGET="192.168.178.1"
LOG_TAG="homelink-monitor"

# Check if interface exists and is up
if ! ip link show $INTERFACE &>/dev/null; then
  logger -t $LOG_TAG "Interface $INTERFACE not found, attempting restart"
  systemctl restart homelink.service
  exit 1
fi

# Check if we can reach the home network
if ! ping -c 1 -W 5 $HOME_TARGET &>/dev/null; then
  # Check if general internet is working
  if ping -c 1 -W 5 1.1.1.1 &>/dev/null; then
    logger -t $LOG_TAG "Internet up but can't reach home network, restarting HomeLink"
    systemctl restart homelink.service
  else
    logger -t $LOG_TAG "No internet connectivity, skipping restart"
  fi
fi
