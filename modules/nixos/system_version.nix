{ config, lib, ... }:
{
  # Wraps system.stateVersion so every machine only sets a value while
  # the warning comment and wiring live here once, not copy-pasted everywhere.
  options.custom.systemStateVersion = lib.mkOption {
    type = lib.types.str;
    description = "NixOS stateVersion for the machine — set once at install time, rarely change";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  config = {
    system.stateVersion = config.custom.systemStateVersion; # Did you read the comment?
  };
}
