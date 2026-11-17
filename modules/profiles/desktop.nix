{ ... }:
{
  flake.modules.nixos.profile-system = { config, lib, ... }:
    let g = config.custom.profiles.system; in
    {
      config = lib.mkIf g.desktop.enable {
        custom.apps.desktopEnvironment.enable = true;
        custom.apps.chrome.enable = true;
        custom.apps.gui.enable = true;
        programs.dconf.enable = true;
      };
    };

  flake.modules.nixos.profile-system-services = { config, lib, ... }:
    let g = config.custom.profiles.systemServices; in
    {
      config = lib.mkMerge [
        (lib.mkIf g.desktop.common.enable {
          custom.services.tmuxd.startWithGraphicalSession = true;
          services.avahi.enable = true;
          custom.services.printing.client.enable = true;
          services.udisks2.enable = true; # mount and unmout USB drives
          services.pipewire.enable = true; # sound
          services.gnome.gnome-keyring.enable = true;
          services.upower.enable = true;
          custom.services.moolticuted.enable = true;
        })

        (lib.mkIf g.desktop.wayland.enable {
          programs.sway.enable = true;
        })
      ];
    };

  flake.modules.homeManager.profile-home-manager = { config, lib, ... }:
    let g = config.custom.profiles.homeManager; in
    {
      config = lib.mkMerge [
        (lib.mkIf g.desktop.common.enable {
          programs.alacritty.enable = true;
          programs.ssh.enable = true;
          programs.rofi.enable = true;
          programs.taskwarrior.enable = true;
          custom.desktop.wallpaper.enable = true;
          xdg.mimeApps.enable = true;
          programs.emacs.enable = true;
          gtk.enable = true;
          qt.enable = true;
          services.dunst.enable = true;
          programs.firefox.enable = true;
          programs.ghostty.enable = true;
          programs.zathura.enable = true;
          custom.programs.moolticute.enable = true;
          custom.programs.whatsapp.enable = true;
        })

        (lib.mkIf g.desktop.wayland.enable {
          wayland.windowManager.sway.enable = true;
          programs.waybar.enable = true;
          custom.services.cliphist.enable = true;
          services.udiskie.enable = true;
          services.flameshot.enable = true;
          services.wlsunset.enable = true;
          services.swayidle.enable = true;
        })
      ];
    };
}
