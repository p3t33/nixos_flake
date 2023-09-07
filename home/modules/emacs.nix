{ pkgs, ... }:
{
    programs.emacs = {
        enable = true;
        package = pkgs.emacs;  # replace with pkgs.emacs-gtk, or a version provided by the community overlay if desired.

        extraPackages = epkgs: [
            epkgs.nord-theme

        ];
        extraConfig = ''
            ;; disabling the default built int plugin manager
            ;; in context of nix these settings are essentially ensuring a
            ;; clean slate" in terms of package management behavior in Emacs
            ;; They ensure that certain built-in package manager features that
            ;; might conflict with an external management tool are turned
            ;; off or are in their most permissive settings.
            ;;
            ;; This should be always at the top of the file before any
            ;; other settings have the chance to run.
            (setq package-quickstart nil)
            (setq package-menu-async nil)
            (setq package-load-list '(all))

            (setq standard-indent 2)
            (load-theme 'nord t)
            (message "Loading init.el...")


        '';
    };

    services.emacs = {
        enable = true;
        package = pkgs.emacs; # replace with emacs-gtk, or a version provided by the community overlay if desired.
    };

}

