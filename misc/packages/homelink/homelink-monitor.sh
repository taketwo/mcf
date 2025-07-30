#!/bin/bash

INTERFACE="wg0"
HOME_ROUTER="${HOME_ROUTER:-192.168.178.1}"
LOG_TAG="homelink-monitor"

# Test direct connectivity (bypassing any WG routes)
# Get default route via physical interface (exclude virtual interfaces)
PHYS_ROUTE=$(ip route | grep '^default' | grep -vE 'dev (wg|tun|tap|veth|docker|br-|lo)' | head -1)

if [ -n "$PHYS_ROUTE" ]; then
  PHYS_DEV=$(echo "$PHYS_ROUTE" | awk '{print $5}')
  if timeout 3 ping -c 1 -I "$PHYS_DEV" "$HOME_ROUTER" >/dev/null 2>&1; then
    AT_HOME=true
  else
    AT_HOME=false
  fi
else
  AT_HOME=false
fi

if [ "$AT_HOME" = true ]; then
  if ip link show $INTERFACE &>/dev/null; then
    logger -t $LOG_TAG "At home, bringing down HomeLink"
    systemctl stop homelink.service
  else
    logger -t $LOG_TAG "At home and HomeLink already down; no action needed"
  fi
  exit 0
fi

if ! ip link show $INTERFACE &>/dev/null; then
  logger -t $LOG_TAG "Remote and no HomeLink interface, starting service"
  systemctl start homelink.service
  exit 0
fi

if ! ping -c 1 -W 5 "$HOME_ROUTER" &>/dev/null; then
  if ping -c 1 -W 5 8.8.8.8 &>/dev/null; then
    logger -t $LOG_TAG "Remote but HomeLink tunnel broken, restarting"
    systemctl restart homelink.service
  else
    logger -t $LOG_TAG "Remote and HomeLink up, but no internet connectivity; skipping restart"
  fi
else
  logger -t $LOG_TAG "Remote and HomeLink already up; no action needed"
fi
