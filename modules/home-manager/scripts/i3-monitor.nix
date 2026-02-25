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

    builtin_display="eDP-1"

    # Re-probe all outputs and re-establish MST links through the dock
    # Combine with --output eDP-1 --off to avoid briefly enabling the laptop screen
    xrandr --auto --output "$builtin_display" --off
    sleep 2

    # Get external monitor names from xrandr
    xrandr_external=$(xrandr | grep " connected" | grep -v "$builtin_display" | cut -d " " -f1)

    if [ -z "$xrandr_external" ]; then
      xrandr --output "$builtin_display" --auto
      exit 0
    fi

    # Explicitly enable external monitors
    for monitor in $xrandr_external; do
      xrandr --output "$monitor" --auto
    done
    sleep 3

    # Verify with ddcutil if the monitors actually responded over DDC/CI
    ddc_output=$(ddcutil detect 2>/dev/null || true)
    ddc_external=$(echo "$ddc_output" | grep "Display [0-9]")

    if [ -n "$ddc_external" ]; then
      # Monitors are real - disable laptop screen
      xrandr --output "$builtin_display" --off
    else
      # Monitors didn't respond - KVM is on another machine, fall back to laptop
      xrandr --output "$builtin_display" --auto
    fi
  '';
in
{
  options.custom.scripts.i3Monitor.enable = lib.mkEnableOption "Enable i3-monitor script";

  config = lib.mkIf cfg.enable {
    home.packages = [ i3-monitor ];
  };
}
