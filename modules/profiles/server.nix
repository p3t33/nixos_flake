{ ... }:
{
  flake.modules.nixos.profile-system = { config, lib, pkgs, ... }:
    let g = config.custom.profiles.system; in
    {
      config = lib.mkIf g.server.enable {
        virtualisation.docker.enable = true;
        custom.motd.enable = true; # moto of the day.
        environment.systemPackages = [ pkgs.ghostty.terminfo ];
      };
    };

  flake.modules.nixos.profile-system-services = { config, lib, ... }:
    let g = config.custom.profiles.systemServices; in
    {
      config = lib.mkIf g.server.enable {
        services.openssh.enable  = true; # sshd
        services.fail2ban.enable = true;
      };
    };
}
