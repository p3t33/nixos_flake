{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.sshClient;
in
{
  options.customOptions.enableModule.sshClient = lib.mkEnableOption "Enable SSH configuration with extraConfig pointing to a SOPS-managed include file";

  config = lib.mkIf cfg {
    programs.ssh = {
      enable = true;
      extraConfig = "
        # This file will be generated with sops and if sops fails to generate
        # it this directive will be skipped.
        Include ${config.sops.secrets.extra_hosts.path}
        ";
    };
  };
}
