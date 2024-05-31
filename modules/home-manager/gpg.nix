{ pkgs, ... }:
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

    # maxCacheTtl* is the overall cache timeout,
    # it overrides any other. By default it is set
    # to 2 hours. Which means that even if
    # defaultCacheTtlSsh is set for more then 2 hours
    # it will be ignored by the
    # value in maxCachedTtl*
    maxCacheTtl = timeout;
    maxCacheTtlSsh = timeout;

    # default values
    # --------------
    # Each time a  cache  entry  is  accessed,  the
    # entry's timer is rest to the default set value.
    # this reset will go as long as the overall time
    # for the cache doesn't exceed the max cache time.
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
    pinentryPackage = pkgs.pinentry-qt;
  };

}
