{ config, ... }:
{
  # Enable the Sonarr service(as of now there is no config for default sonarr port)
  services.sonarr = {
    enable = true;
    openFirewall = true; # Opens Sonarr's port on the firewall (default 8989)
    user = "sonarr";
    group = "${config.userDefinedGlobalVariables.mediaGroup}";
  };

  # sonarr is using end of life dotnet, which was marked as insecure in NixOS 24.11
  # Untill soanrr moves to newer version this workaound is requried.
  nixpkgs.config.permittedInsecurePackages = [
    "aspnetcore-runtime-6.0.36"
    "aspnetcore-runtime-wrapped-6.0.36"
    "dotnet-sdk-6.0.428"
    "dotnet-sdk-wrapped-6.0.428"
  ];
}
