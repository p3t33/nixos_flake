# I have a dual monitor setup with a desktop and a laptop that is connected to a docking station.
# In order to make things smoother when switching between the two I have added a KVM switch
# that emulates EDID.
#
# EDID emulation makes the OS think the monitors are always connected so it keeps the desktop
# layout, without this the OS will consider every KVM switch as you pulling the monitors out.
#
# This script is the butter that makes the switch from my PC to the laptop smooth.
#
# In order to understand what this script does it is necessary to understand what constitutes
# a "pipeline", what are the different components and how they interact.
#
# There are 3 "pipeline" configurations:
# 1. PC GPU -> KVM -> monitors
# 2. laptop GPU -> Dock(via MST) -> monitors (typical office setup)
# 3. laptop GPU -> Dock(via MST) -> KVM -> monitors (my home setup)
#
# The MST (Multi-Stream Transport) is a DisplayPort feature that sends multiple independent
# video streams over a single cable. Which means that my laptop does not see the actual
# monitors but instead sees the internals of the Dock and how it represents them.
#
# Why pipeline 1 works without this script:
# The GPU is directly connected to the KVM. When the KVM switches, the GPU detects the
# link change via the DisplayPort AUX channel and automatically retrains the signal.
# The monitors just work.
#
# Why pipeline 2 works without this script:
# There is no KVM, so there is no EDID emulation. The dock sees real monitors and reports
# their true state to the GPU. xrandr shows "connected" or "disconnected" based on reality.
# A simple xrandr command is all that's needed.
#
# Why pipeline 3 is problematic:
# The dock insulates the GPU from the KVM switch. The GPU only talks to the dock, and
# the dock never disconnects. The KVM switch happens downstream of the dock, but the dock
# doesn't report it because the KVM's EDID emulation makes it look like nothing changed.
# As a result:
# - No one reinitiates DisplayPort link training, so the monitors can't decode the video
#   stream and stay black.
# - xrandr always reports the monitors as "connected" due to EDID emulation, so it is
#   impossible to tell if the monitors are real or just EDID ghosts.
#
# How this script solves it:
# It addresses two separate problems with two different tools:
# - xrandr --auto reprobes the outputs and forces downstream link retraining through the
#   dock. This fixes the pipeline so the monitors CAN receive the signal. Note: we must
#   never use xrandr --output --off on MST outputs as it tears down the MST topology.
# - ddcutil detect verifies if the monitors ACTUALLY responded. DDC/CI is a two-way
#   protocol that requires a real monitor to reply, unlike EDID which the KVM can fake.
#   This prevents a black screen when the KVM is switched to the other machine.
{ config, lib, pkgs, ... }:
let
  cfg = config.custom.scripts.i3Monitor;

  # Re-probes display outputs and verifies with ddcutil (DDC/CI) whether external
  # monitors are real. Works around KVM EDID emulation where xrandr always sees
  # monitors as "connected" even when KVM is switched to another machine.
  # IMPORTANT: Do NOT use xrandr --output --off on MST (dock) outputs, as this
  # tears down the MST topology and the outputs become "disconnected."
  i3-monitor = pkgs.writeShellScriptBin "i3-monitor" ''
    export PATH="${lib.makeBinPath [ pkgs.ddcutil pkgs.xorg.xrandr pkgs.coreutils pkgs.gnugrep ]}:$PATH"

    # Auto-detect the internal panel (eDP-1, eDP-1-1, eDP, etc.)
    builtin_display=$(xrandr | grep -oP '^eDP[^\s]*(?=\s+connected)' | head -n1)
    if [ -z "$builtin_display" ]; then
      echo "No internal eDP panel detected, skipping laptop screen management"
    fi

    # Re-probe all outputs and re-establish MST links through the dock.
    # We intentionally do NOT --off the laptop screen here. The laptop panel
    # may briefly light up, but that's preferable to going completely dark
    # while waiting for the dock/KVM to finish link training.
    xrandr --auto

    # Poll xrandr until external outputs appear with valid modes (max 5s)
    xrandr_external=""
    for attempt in $(seq 1 10); do
      xrandr_external=$(xrandr | grep " connected" | grep -v "$builtin_display" | cut -d " " -f1 | grep -E '^(DP|HDMI|DVI|VGA)-')
      [ -n "$xrandr_external" ] && break
      sleep 0.5
    done

    if [ -z "$xrandr_external" ]; then
      [ -n "$builtin_display" ] && xrandr --output "$builtin_display" --auto
      exit 0
    fi

    # Explicitly enable external monitors
    for monitor in $xrandr_external; do
      xrandr --output "$monitor" --auto
    done

    # Poll ddcutil until monitors respond or timeout (max 5s, 3 attempts).
    # Monitors sometimes need a moment after link training to answer DDC/CI.
    # We distinguish three cases:
    #   1. ddcutil succeeded + found displays   -> monitors are real
    #   2. ddcutil succeeded + no displays       -> KVM on other machine
    #   3. ddcutil failed (permissions, i2c, etc) -> unknown, don't change state
    ddc_stderr=$(mktemp)
    ddc_rc=0
    ddc_external=""
    for attempt in $(seq 1 3); do
      ddc_rc=0
      ddc_output=$(ddcutil detect 2>"$ddc_stderr") || ddc_rc=$?
      [ "$ddc_rc" -ne 0 ] && break
      ddc_external=$(echo "$ddc_output" | grep "Display [0-9]")
      [ -n "$ddc_external" ] && break
      sleep 1.5
    done

    if [ "$ddc_rc" -ne 0 ]; then
      echo "ddcutil failed (rc=$ddc_rc): $(cat "$ddc_stderr")"
      echo "Cannot determine monitor state, leaving display config unchanged"
      rm -f "$ddc_stderr"
      exit 1
    fi
    rm -f "$ddc_stderr"

    if [ -n "$ddc_external" ]; then
      # Monitors are real - only now disable laptop screen
      [ -n "$builtin_display" ] && xrandr --output "$builtin_display" --off
    else
      # Monitors didn't respond - KVM is on another machine, fall back to laptop
      [ -n "$builtin_display" ] && xrandr --output "$builtin_display" --auto
    fi
  '';
in
{
  options.custom.scripts.i3Monitor.enable = lib.mkEnableOption "Enable i3-monitor script";

  config = lib.mkIf cfg.enable {
    home.packages = [ i3-monitor ];
  };
}
