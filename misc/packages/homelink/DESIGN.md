# HomeLink Monitor - Design Document

## Overview

HomeLink is an intelligent WireGuard VPN daemon that automatically manages VPN connectivity based on location detection and network conditions, preventing unnecessary VPN overhead when at home.

## Architecture

HomeLink operates as a daemon with configurable monitoring intervals, providing:
- **Fast response times**: Immediate detection of network changes
- **Hysteresis logic**: Sliding window of connectivity checks prevents flapping
- **Resume detection**: Automatic stabilization delay after system wake from suspend
- **Real-time status**: Unix socket API for Polybar/desktop integration
- **Structured logging**: SystemD journal integration for monitoring

## Location Detection

HomeLink determines location by checking default gateway routes on physical network interfaces:

```bash
ip route show default dev wlp2s0
# At home: "default via 192.168.114.1 dev wlp2s0"
# Remote:  "default via 172.20.0.1 dev wlp2s0"
```

This approach is VPN-independent and relies on local DHCP-assigned gateways for reliable detection.

## Physical Interface Discovery

HomeLink dynamically discovers physical network interfaces by filtering out virtual interfaces:

```python
virtual_prefixes = ("wg", "tun", "tap", "veth", "docker", "br-", "lo", "virbr", "vmbr")
if not iface_name.startswith(virtual_prefixes) and iface_name != configured_interface:
    interfaces.append(iface_name)
```

This ensures compatibility across different systems without hardcoding interface names.

## Internet Connectivity Monitoring

Internet connectivity is assessed using consensus logic to prevent flapping:

```python
class ConsensusTracker:
    def add(self, current_value: Any) -> None:
        """Add current value to the history."""
        self.history.append(current_value)
    
    def get(self) -> Any | None:
        """Get current consensus if all values agree, None otherwise."""
        if len(self.history) < self.history.maxlen:
            return None
        first_value = self.history[0]
        if all(val == first_value for val in self.history):
            return first_value
        return None

# Usage
internet_tracker.add(internet_working)
consensus = internet_tracker.get()  # True/False/None
```

The system requires all values in the sliding window to be identical before declaring consensus. Connectivity tests ping multiple DNS servers (8.8.8.8, 1.1.1.1) with configurable timeout.

## VPN Tunnel Health Detection

VPN status is determined through multi-tier health checks:

1. **Interface check**: Verify WireGuard interface exists and is up
2. **Handshake verification**: Check for recent WireGuard handshake (within 180 seconds)
3. **Ping test**: Fallback to ping home router if handshake check unavailable

Handshake timestamp is the primary health indicator because it directly verifies cryptographic tunnel establishment. A fresh handshake (within 3x the rekey interval) guarantees the tunnel is operational, even if the home router blocks ping.

If interface is up but no recent handshake and ping fails, status is marked as "degraded".

## Configuration

### Environment Variables

- `HOMELINK_INTERFACE` - VPN interface name (default: wg0)
- `HOMELINK_HOME_ROUTER` - Home router IP (default: 192.168.114.1)
- `HOMELINK_CHECK_INTERVAL` - Check frequency seconds (default: 10)
- `HOMELINK_INTERNET_HISTORY_SIZE` - Sliding window size (default: 3)
- `HOMELINK_PING_TIMEOUT` - Ping timeout seconds (default: 3)
- `HOMELINK_LOG_LEVEL` - Logging verbosity (default: INFO)

## Core State Model

The system operates on **3 independent state variables**:

1. **`location`**: `"home" | "remote"`
   - Determined by default gateway detection via physical interfaces

2. **`internet`**: `"down" | "uncertain" | "up"`
   - `"down"` = consensus that internet is not working (all recent checks failed)
   - `"uncertain"` = mixed results, no consensus yet
   - `"up"` = consensus that internet is working (all recent checks succeeded)

3. **`vpn`**: `"down" | "degraded" | "up"`
   - `"down"` = WireGuard interface down
   - `"degraded"` = interface up but tunnel broken
   - `"up"` = interface up and tunnel working

## Control Logic

The system determines VPN actions based on the current state variables:

```python
internet_consensus = internet_tracker.get()  # True/False/None

if location == "home":
    return "down" if vpn != "down" else "noop"    # Disable VPN at home
if internet_consensus is False:
    return "down" if vpn != "down" else "noop"    # Save resources when no internet
if internet_consensus is None:
    return "noop"                                 # Uncertain internet, maintain state
# internet_consensus is True (working internet)
if vpn == "down":
    return "up"                                   # Start VPN when remote with internet
if vpn == "degraded":
    return "restart"                              # Fix broken tunnel
return "noop"                                     # VPN working correctly
```

**Control Signals:**
- `"down"` - Stop VPN service
- `"up"` - Start VPN service
- `"restart"` - Restart VPN service
- `"noop"` - No action needed

## Socket API

HomeLink provides real-time status through a Unix domain socket at `/run/homelink/monitor.sock` for integration with Polybar, debugging, and monitoring.

**Status Response:**
```json
{
  "location": "remote",
  "internet": "up",
  "vpn": "up",
  "vpn_control_signal": "noop",
  "next_check_at": "2025-01-15T10:31:00+00:00"
}
```

## System Requirements

### Root Privileges

HomeLink requires root privileges to manage systemd services (`systemctl start/stop/restart`). The daemon runs as a systemd service with security hardening to minimize attack surface.

### Network Topology

The system assumes the home router is the default gateway when at home. Complex multi-gateway setups may require configuring `HOMELINK_HOME_ROUTER` to specify the primary gateway.

### Timing Considerations

All network checks are performed atomically within a single method to prevent race conditions. Hysteresis logic requires multiple failed checks before triggering state changes.

## Resume Detection

HomeLink detects system resume from suspend using timing gap analysis and resets consensus state to prevent VPN flapping during network interface stabilization.

**Detection Logic:**
```python
if next_check_at and now > next_check_at + timedelta(seconds=check_interval * 2):
    # Resume detected - reset consensus tracker
    internet_tracker.reset()
```

**How It Works:**
1. **Gap Detection**: If current time exceeds expected check time by more than 2x check interval, assume system resumed
2. **State Reset**: Clear consensus tracker history, forcing "uncertain" state
3. **Natural Stabilization**: Normal consensus-building process provides stabilization delay
4. **No VPN Actions**: Control logic maintains current state until consensus is reached

This approach is self-consistent with the consensus model and naturally prevents premature VPN decisions during network stabilization.

---

## Appendix: Home Detection Lessons Learned

Home detection in the presence of VPN routing presents significant challenges. The following approaches were evaluated:

### Failed Approaches

- **Ping-based detection**: VPN routes make home router reachable remotely
- **Subnet IP detection**: VPN tunnel gets home subnet IP (192.168.114.x)
- **ARP table inspection**: VPN routing interferes with ARP resolution
- **Physical interface ping**: `-I interface` still uses VPN routes when available

### Working Solution: Default Gateway Detection

The implemented solution checks default gateway routes on physical interfaces:

```bash
ip route show default dev wlp2s0
# At home: "default via 192.168.114.1 dev wlp2s0"
# Remote:  "default via 172.20.0.1 dev wlp2s0"
```

**Why This Works:**
- ✅ VPN-independent: Only checks physical interface routing
- ✅ Reliable: Default gateway is assigned by local DHCP
- ✅ Fast: Single route table lookup, no network tests
- ✅ Simple: No complex logic that VPN can interfere with

### Key Implementation Details

**Physical Interface Filtering**: Dynamic discovery with virtual interface exclusion prevents interference from VPN and container networking:

```python
virtual_prefixes = ("wg", "tun", "tap", "veth", "docker", "br-", "lo", "virbr", "vmbr")
if not iface_name.startswith(virtual_prefixes) and iface_name != self.interface:
    interfaces.append(iface_name)
```

**VPN Route Pollution**: WireGuard creates routes that can interfere with detection. The solution explicitly excludes the configured VPN interface from all physical interface logic.

**Robust Tunnel Health**: Home routers often don't respond to ping (disabled/firewalled), requiring multi-tier health detection combining ping tests, route verification, and IP assignment checks.

---

## Appendix: WireGuard IPv6 Handshake Issues

**Problem Observed:** WireGuard tunnel failed to establish handshakes when connecting from a FritzBox-managed network to a home FritzBox server via DynDNS hostname, despite working reliably on other networks.

**Symptoms:**
- WireGuard interface up with correct IP and routes
- Endpoint reachable via both IPv4 and IPv6
- Client sending handshake packets (transfer shows KiB sent, 0 B received)
- No "latest handshake" timestamp in `wg show` output
- Ping to home router through tunnel fails

**Root Cause:** FritzBox IPv6 firewall blocking incoming WireGuard handshake response packets. When the DynDNS hostname resolved to both IPv4 and IPv6 addresses, WireGuard preferred IPv6. The local FritzBox's stateful packet inspection dropped incoming UDP responses on high ports as "unsolicited" traffic.

**Solution:** Force IPv4 endpoint by specifying IP address directly instead of hostname in WireGuard configuration:
```
Endpoint = <ipv4-address>:<port>
```

**Key Diagnostic:** Check `wg show` for:
- Presence of "latest handshake" timestamp
- Bidirectional transfer (both sent and received > 0)
- Endpoint IP version being used

**Detection Improvement:** The health check now verifies handshake freshness as the primary indicator, which immediately detects this failure mode (interface up with routes/IP configured, but no successful handshakes).
