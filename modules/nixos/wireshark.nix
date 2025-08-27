{ pkgs, lib, config, hostSpecific, ... }:

let
  cfg = config.custom.apps.wireshark;
in
{
  options.custom.apps.wireshark.enable = lib.mkEnableOption "Enable wireshark and its Dependencies";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      # presentation
      wireshark
      iw
      aircrack-ng
      wirelesstools # install iwconfig among other things
    ];

    users.groups.wireshark = {
      members = [ hostSpecific.primeUsername ];
    };
  };
}
