{ ... }:
let
  # 12 hours in seconds
  timeout = 43200;
in
{

  programs.gpg = {
    enable = true;
  };

  services.gpg-agent = {
    enableSshSupport = true;

    # this is the overall timeout it overides any other
    # set values. By default it is set to 2 hours. 
    # Which means that even if defaultCacheTtlSsh is set 
    # for more then 2 hours it will be ignored by the 
    # value in maxCachedTtl
    maxCacheTtl = timeout;
    defaultCacheTtl = timeout;
    defaultCacheTtlSsh = timeout;
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
