{ config, pkgs, ... }:

{

  # workaround for one of the tests failng.
  nixpkgs.overlays = [
  (final: prev: {
    jackett = prev.jackett.overrideAttrs { doCheck = false; };
  })
];

  services.jackett = {
    enable = true; # Starts the Jackett service
    openFirewall = true; # Opens firewall for external access (optional) # 9117
    port = config.userDefinedGlobalVariables.servicePort.jackett;
  };
}
