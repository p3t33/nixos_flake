{ ... }:
{

  programs.ssh = {
    enable = true;
    extraConfig = "
      # This file wil be generated with sops and if sops fails to generate
      # it this directive will be skipped.
      Include ~/.ssh/extra_hosts
       ";
  };
}
