{ config, pkgs, ... }:

{
    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

    home = {
        # Home Manager needs a bit of information about you and the
        # paths it should manage.
        username = config.userDefinedGlobalVariables.primeUsername;
        homeDirectory = config.userDefinedGlobalVariables.primeUserHomeDirectory;

        # This value determines the Home Manager release that your
        # configuration is compatible with. This helps avoid breakage
        # when a new Home Manager release introduces backwards
        # incompatible changes.
        #
        # You can update Home Manager without changing this value. See
        # the Home Manager release notes for a list of state version
        # changes in each release.
        stateVersion = config.userDefinedGlobalVariables.homeManagerStateVersion;


        # Variables that will be set "system wide" in the context of the user.
        # E.g, by setting MANPAGER it will be available to bash, zsh, fish with
        # the alternative limiting the scope and setting this variables in a file
        # such as .bashrc using "export".
        sessionVariables = {
            EDITOR = config.userDefinedGlobalVariables.editor;
            SUDO_EDITOR = config.userDefinedGlobalVariables.editor;
            MANPAGER = config.userDefinedGlobalVariables.manPager;
        };
    };
}

