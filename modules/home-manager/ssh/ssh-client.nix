{ config, lib, ... }:
{
  config = lib.mkIf config.programs.ssh.enable {
    programs.ssh = {
      extraConfig = "
        # This file will be generated with sops and if sops fails to generate
        # it this directive will be skipped.
        Include ${config.sops.secrets.extra_hosts.path}
        ";
    };
  };
}
