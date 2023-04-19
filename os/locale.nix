{ pkgs,  ... }:
{
#   Set your time zone.
    time.timeZone = "Asia/Jerusalem";

    # Select internationalisation properties.
    i18n.defaultLocale = "en_IL";

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
