{ config, ... }:
{
  # Global settings for displaymanger.
  # can be used to define the display manger itslef(E.g ssdm, lt) but not all
  # of them are available via this configuration(only ssdm, and lt) at the
  # moment. This is why I provide the selection of the actual displaymanger via
  # xserver in a seperate file.
  services.displayManager = {
    # The "none", Refers to a session type that skips starting a full desktop
    # environment (like GNOME, KDE, or XFCE).
    #
    # +i3: Specifies that i3 (a standalone tiling window manager) should be launched as the session.
    #
    # Together, "none+i3" means:
    # - Start a minimal X11 session with no desktop environment.
    # - Launch i3wm as the window manager within that session
    defaultSession = "none+i3";
    autoLogin = {
      enable = true;
      user = config.userDefinedGlobalVariables.primeUsername;
    };
  };
}
