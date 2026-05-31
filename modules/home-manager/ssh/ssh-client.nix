{ config, lib, ... }:
{
  config = lib.mkIf config.programs.ssh.enable {
    programs.ssh = {
      # In near future home-manger will not generate defaults and when upgrading
      # to 25.11 it was recomanded to explicitly set this option to false.
      enableDefaultConfig = false;
      settings."*" = {
        ForwardAgent = false; # safer (prevents credential theft)
        Compression = false; # fine for LANs / fast links
        HashKnownHosts = true; # protects privacy; hides hostnames/IPs

        ServerAliveInterval = 30; # keep connection alive every 30s
        ServerAliveCountMax = 3; # fail after ~90s of silence

        AddKeysToAgent = "no"; # don't auto-add keys (safer)

        # SSH multiplexing
        ControlMaster = "auto";
        ControlPath = "~/.ssh/master-%r@%n:%p";
        ControlPersist = "5m";
      };

      extraConfig = "
        # This file will be generated with sops and if sops fails to generate
        # it this directive will be skipped.
        Include ${config.sops.secrets.extra_hosts.path}
        ";
    };
  };
}
