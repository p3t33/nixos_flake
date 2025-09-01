{
  # Workaround for systemd-vconsole-setup.service failing during early boot.
  #
  # After upgrade to NixOS 25.05 I notced, as part of journalctl -p err -b
  # The service would report "No such file or directory" when trying
  # to use gzip to decompress keymap files (e.g., us.map.gz from /nix/store),
  # leading to a failure in loading the console keyboard layout.
  # Delaying the service to run `After = "local-fs.target";` ensures that
  # local filesystems (including the Nix store) are fully mounted and accessible,
  # resolving this timing-related issue.
  # See NixOS/nixpkgs issue #312452 for similar reports.
  systemd.services.systemd-vconsole-setup = {
    unitConfig = {
      After = "local-fs.target";
    };
  };

  #   Set your time zone.
  time.timeZone = "Asia/Jerusalem";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "he_IL.UTF-8";
    LC_IDENTIFICATION = "he_IL.UTF-8";
    LC_MEASUREMENT = "he_IL.UTF-8";
    LC_MONETARY = "he_IL.UTF-8";
    LC_NAME = "he_IL.UTF-8";
    LC_NUMERIC = "he_IL.UTF-8";
    LC_PAPER = "he_IL.UTF-8";
    LC_TELEPHONE = "he_IL.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };
}
