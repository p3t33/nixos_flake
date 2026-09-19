# Display policy and emergency recovery for the work laptop's dock -> MST ->
# EDID-emulating KVM display path. DDC distinguishes reachable monitors from
# the KVM's persistent EDID, while Sway power commands preserve MST topology.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.custom.scripts.displayRecovery;
  awk = lib.getExe pkgs.gawk;
  ddcutil = lib.getExe pkgs.ddcutil;
  jq = lib.getExe pkgs.jq;
  sleep = lib.getExe' pkgs.coreutils "sleep";
  swaymsg = lib.getExe' pkgs.sway "swaymsg";

  displayControl = pkgs.writeShellScript "display-control" ''
    set -euo pipefail

    action=''${1:-}
    builtin_display=${lib.escapeShellArg cfg.builtInOutput}
    expected_external_displays=${toString cfg.expectedExternalDisplays}
    external_power_cycle_armed=false
    external_outputs=()

    sway_command() {
      local response
      response=$(${swaymsg} "$@") || return
      printf '%s\n' "$response" | ${jq} --exit-status 'all(.success == true)' >/dev/null
    }

    detect_responsive_external_displays() {
      local connector ddc_output output
      local -a ddc_connectors=()
      local -A responsive_outputs=()

      ddc_output=$(${ddcutil} detect --verbose 2>/dev/null || true)
      mapfile -t ddc_connectors < <(
        printf '%s\n' "$ddc_output" | ${awk} '
          /^Display [0-9]+$/ {
            valid = 1
            next
          }
          /^(Invalid|Phantom) display/ {
            valid = 0
            next
          }
          valid && /^[[:space:]]*DRM[_ ]connector:/ {
            connector = $0
            sub(/^[[:space:]]*DRM[_ ]connector:[[:space:]]*/, "", connector)
            sub(/^card[0-9]+-/, "", connector)
            print connector
            valid = 0
          }
        '
      )

      for connector in "''${ddc_connectors[@]}"; do
        for output in "''${external_outputs[@]}"; do
          if [ "$connector" = "$output" ]; then
            responsive_outputs["$output"]=1
            break
          fi
        done
      done
      printf '%s\n' "''${#responsive_outputs[@]}"
    }

    apply_panel_policy() {
      local detected=''${1:-}

      if [ -z "$detected" ]; then
        detected=$(detect_responsive_external_displays)
      fi

      if [ "$detected" -gt 0 ]; then
        if [ "$builtin_present" = true ]; then
          sway_command output "$builtin_display" disable
        fi
        echo "$detected responsive external display(s); $builtin_display disabled."
        return
      fi

      if [ "$builtin_present" != true ]; then
        echo "No responsive external displays and fallback output $builtin_display is unavailable." >&2
        return 1
      fi

      sway_command output "$builtin_display" enable
      echo "No responsive external displays; $builtin_display enabled."
    }

    # shellcheck disable=SC2329
    restore_external_power() {
      local output

      if [ "$external_power_cycle_armed" != true ]; then
        return
      fi

      for output in "''${external_outputs[@]}"; do
        ${swaymsg} output "$output" power on >/dev/null 2>&1 || true
      done
    }
    trap restore_external_power EXIT

    recover_displays() {
      local detected output

      if [ "$external_count" -eq 0 ]; then
        apply_panel_policy 0
        echo "No external Sway outputs found." >&2
        return 1
      fi

      detected=$(detect_responsive_external_displays)
      if [ "$external_count" -ge "$expected_external_displays" ] && [ "$detected" -eq "$external_count" ]; then
        apply_panel_policy "$detected"
        echo "All $detected external displays already respond over DDC; no recovery needed."
        return
      fi

      if [ "$builtin_present" != true ]; then
        echo "Cannot recover safely without fallback output $builtin_display." >&2
        return 1
      fi

      echo "Only $detected of $external_count external displays respond over DDC; recovering links."
      sway_command output "$builtin_display" enable

      external_power_cycle_armed=true
      for output in "''${external_outputs[@]}"; do
        sway_command output "$output" power off
      done
      ${sleep} 2

      for output in "''${external_outputs[@]}"; do
        sway_command output "$output" power on
      done
      external_power_cycle_armed=false
      ${sleep} 3

      detected=$(detect_responsive_external_displays)
      if [ "$external_count" -ge "$expected_external_displays" ] && [ "$detected" -eq "$external_count" ]; then
        apply_panel_policy "$detected"
        echo "Recovered all $detected external displays."
        return
      fi

      echo "Recovery failed: only $detected of $external_count external displays respond over DDC (minimum expected: $expected_external_displays); keeping $builtin_display enabled." >&2
      return 1
    }

    if [ -z "''${SWAYSOCK:-}" ]; then
      echo "Display control requires an active Sway session." >&2
      exit 1
    fi

    outputs=$(${swaymsg} --raw --type get_outputs)
    builtin_present=$(printf '%s\n' "$outputs" | ${jq} \
      --arg builtin "$builtin_display" 'any(.name == $builtin)')
    mapfile -t external_outputs < <(
      printf '%s\n' "$outputs" |
        ${jq} --raw-output --arg builtin "$builtin_display" \
          '.[] | select(.active == true and .name != $builtin and (.name | startswith("HEADLESS-") | not)) | .name'
    )
    external_count=''${#external_outputs[@]}

    case "$action" in
      policy) apply_panel_policy ;;
      recover) recover_displays ;;
      *)
        echo "Usage: display-control {policy|recover}" >&2
        exit 2
        ;;
    esac
  '';

  applyDisplayPolicy = pkgs.writeShellScriptBin "apply-display-policy" ''
    exec ${displayControl} policy
  '';

  recoverDisplays = pkgs.writeShellScriptBin "recover-displays" ''
    exec ${displayControl} recover
  '';
in
{
  options.custom.scripts.displayRecovery = {
    enable = lib.mkEnableOption "Sway display policy and recovery commands";
    builtInOutput = lib.mkOption {
      type = lib.types.str;
      default = "eDP-1";
      description = "Name of the built-in output used as a recovery fallback.";
    };
    expectedExternalDisplays = lib.mkOption {
      type = lib.types.ints.positive;
      default = 1;
      description = "Minimum number of external displays required for successful link recovery.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      applyDisplayPolicy
      recoverDisplays
    ];

    wayland.windowManager.sway.config = {
      keybindings = lib.mkOptionDefault {
        "Mod4+q" = "reload; exec ${lib.getExe applyDisplayPolicy}";
      };

      startup = [
        {
          command = lib.getExe applyDisplayPolicy;
          always = false;
        }
      ];
    };
  };
}
