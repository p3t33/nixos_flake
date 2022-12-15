{ ... }:
{

  programs.gpg = {
    enable = true;
  };

  services.gpg-agent = {
    enableSshSupport = true;
    enable = true;

    # Requires pinentry_qt to be installed.
    # When no gui is available(when connecting via
    # ssh or on a server this option will default
    # into "curses" even if it is set to "qt".
    #
    # "corses" caveat:
    # A minimum window size is required for it to work
    # inside a terminal.
    pinentryFlavor = "qt";
  };

}
