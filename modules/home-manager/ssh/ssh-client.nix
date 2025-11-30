{ config, lib, ... }:
{
  config = lib.mkIf config.programs.ssh.enable {
    programs.ssh = {
      # In near future home-manger will not generate defaults and when upgrading
      # to 25.11 it was recomanded to explicitly set this option to false.
      enableDefaultConfig = false;
      matchBlocks."*" = {
        # Built-in HM options (typed)
        forwardAgent = false;  # safer (prevents credential theft)
        compression = false; # # fine for LANs / fast links
        hashKnownHosts = true; # protects privacy; hides hostnames/IPs

        serverAliveInterval = 30; # keep connection alive every 30s
        serverAliveCountMax = 3; # fail after ~90s of silence

        # Raw ssh_config options that HM does not expose
        extraOptions = {
          AddKeysToAgent = "no"; # don't auto-add keys (safer)

          # SSH multiplexing (raw options)
          ControlMaster = "auto";
          ControlPath = "~/.ssh/master-%r@%n:%p";
          ControlPersist = "5m";
        };
      };

      extraConfig = "
        # This file will be generated with sops and if sops fails to generate
        # it this directive will be skipped.
        Include ${config.sops.secrets.extra_hosts.path}
        ";
    };
  };
}
