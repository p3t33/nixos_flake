{config, pkgs, lib, machineName, ...}:

{

  options = with lib; with types; {

    userDefinedGlobalVariables = {

    systemStateVersion = mkOption {
      default = "";
      type = str;
      description = "Defines the realease version when OS was first installed";
    };

    homeManagerStateVersion = mkOption {
      # Helps to avoid brakeage and as a best practice shoud be the same version
      # as the reset of the system.
      default = "23.11";
      type = str;
      description = "Defines the realease that Home Manager has backwards compatibility with";
    };

    pathToFlakeDirectory = mkOption {
      default = "${config.home.homeDirectory}/projects/nixos_flake";
      type = str;
      description = "used to define host hostname";
    };

    flakeRepositoryUrl = mkOption {
      default = "https://github.com/p3t33/nixos_flake.git";
      type = str;
      description = "used to define host hostname";
    };

    hostname = mkOption {
      default = "";
      type = str;
      description = "used to define host hostname";
    };

    username = mkOption {
        type = str;
        default = "kmedrish";
    };

    editor = mkOption {
        type = str;
        default = "nvim";
    };

    manPager = mkOption {
        type = str;
        default = "nvim +Man!";
    };

    hostTag = mkOption {
        type = str;
        default = "used as an argument for flake update";
    };

    homeDirectory = mkOption {
        type = str;
        default = "/home/${config.userDefinedGlobalVariables.username}";
    };

    syncthingConfigDirectory = mkOption {
        type = str;
        default = "${config.userDefinedGlobalVariables.homeDirectory}/.config/syncthing";
    };
    syncthingUser = mkOption {
        type = str;
        default = "${config.userDefinedGlobalVariables.username}";
    };

    initialHashedPassword = mkOption {
        type = str;
        default = "6$1R0D6ghbKpPOPY6I$UoGLPJbvOvE4ZEPs/Qq/JG9WLMUU9H8lTgIEi4vIwV1fdNfgdX1uYOZNGWYQmlDo84vUmEILEqc.BaRzvYXyB1";
    };

    wallpaperName = mkOption {
        default = "mountain.jpg";
        type = str;
        description = "the name of the wallpaper file";
    };

    wallpaperOut = mkOption {
        default = "wallpaper/${config.userDefinedGlobalVariables.wallpaperName}";
        type = str;
        description = "the relative path to $HOME/.config/wallpaper/ where wallpaper will be located";
    };

    wallpaperIn = mkOption {
        default = ../wallpaper/${config.userDefinedGlobalVariables.wallpaperName};
        type = path;
        description = "the relative path inside the repository of the wallpaper file and the .nix file that will be sourcing it";
    };

    home_manger_import_path = mkOption {
        default = ../hosts/${config.userDefinedGlobalVariables.hostTag}/home.nix;
        type = path;
        description = "the relative path inside the repository of the wallpaper file and the .nix file that will be sourcing it";
    };

    NixOSSecretsPath = mkOption {
        default = ../hosts/${config.userDefinedGlobalVariables.hostTag}/secrets.yaml;
        type = path;
        description = "the relative path inside the repository of the wallpaper file and the .nix file that will be sourcing it";
    };


    font = mkOption {
        type = attrsOf str;
        default = {
            mono = "JetBrainsMono Nerd Font";
            serif = "DejaVu Serif";
            sansSerif = "DejaVu Sans";
        };
        description = "Font to be used on the system, depended on NerdFonts to be installed";
    };

    colors = mkOption {
        type = attrsOf str;
        default = {
            background = "#312f2f";
            background-alt = "#3b4354";
            foreground = "#F1FAEE";
            primary = "#08D9D6";
            secondary = "#047672";
            alert = "#ff2e63";
            disabled = "#707880";
            bg = "#2f343f";
            inactive-bg = "#2f343f";
            text = "#f3f4f5";
            inactive-text = "#676E70";
            urgent-bg = "#E53935";
      };
        example = {
            background = "#312f2f";
            background-alt = "#3b4354";
            foreground = "#F1FAEE";
            primary = "#08D9D6";
            secondary = "#047672";
            alert = "#ff2e63";
            disabled = "#707880";
            bg = "#2f343f";
            inactive-bg = "#2f343f";
            text = "#f3f4f5";
            inactive-text = "#676E70";
            urgent-bg = "#E53935";
        };
      description = "The color palette for the user interface.";
    };

    workspaces_icons = mkOption {
        type = attrsOf str;
        default = {
            firefox = "";
            vscode = "";
            cherrytree = "";
            chrome = "";
            terminal = "";
            buildserver = "";
            vm = "";
        };
        description = "Icon definitions";
    };

    #"${config.userDefinedGlobalVariables.workspaces.ws9}"
    workspaces = mkOption {
        type = attrsOf str;
        default = {
            ws1 = "1: ${config.userDefinedGlobalVariables.workspaces_icons.firefox} Firefox";
            ws2 = "2: ${config.userDefinedGlobalVariables.workspaces_icons.vscode} VSCode";
            ws3 = "3: ${config.userDefinedGlobalVariables.workspaces_icons.cherrytree} Cherrytree";
            ws4 = "4: ${config.userDefinedGlobalVariables.workspaces_icons.chrome} Chrome";
            ws6 = "5: ${config.userDefinedGlobalVariables.workspaces_icons.terminal} terminal";
            ws5 = "6: ${config.userDefinedGlobalVariables.workspaces_icons.buildserver} BuildServer";
            ws7 = "7";
            ws8 = "8: ${config.userDefinedGlobalVariables.workspaces_icons.vm} VM";
            ws9 = "9: VPN";
            ws10 = "10";

      };
        example = {
            ws1 = "1:  Firefox";
            ws2 = "2:  VSCode";
            ws3 = "3:  Cherrytree";
            ws4 = "4:  Chrome";
            ws6 = "5:  terminal";
            ws5 = "6:  BuildServer";
            ws7 = "7";
            ws8 = "8:  VM";
            ws9 = "9: VPN";
            ws10 = "10";
        };
      description = "i3wm and i3wm related strings";
    };

    };
  };

  config = lib.mkMerge [
     (lib.mkIf (config.userDefinedGlobalVariables.hostname == "HP-Zbook"){
          userDefinedGlobalVariables.username = "kmedrish";
          userDefinedGlobalVariables.hostTag = "work_pc";
          userDefinedGlobalVariables.systemStateVersion = "23.05";
      })

     (lib.mkIf (machineName  == "home-desktop"){
          userDefinedGlobalVariables.username = "kmedrish";
          userDefinedGlobalVariables.hostTag = "home_desktop";
          userDefinedGlobalVariables.wallpaperName = "crane_at_night.png";
          userDefinedGlobalVariables.systemStateVersion = "23.11";
      })

     (lib.mkIf (config.userDefinedGlobalVariables.hostname == "kvm-nixos-gui"){
          userDefinedGlobalVariables.username = "kmedrish";
          userDefinedGlobalVariables.hostTag = "vm_gui";
          userDefinedGlobalVariables.initialHashedPassword = "$6$8jJvz/BcLMaK4yEN$tZ.bzF13N9i9deD8GkfROIkJ874.w2GPKN0xBeQ5RlZ40XpVPiUIi85Z/mkcq97y9qKnwyfujPZuxFhaDZTid0";
          userDefinedGlobalVariables.systemStateVersion = "23.05";
      })

     (lib.mkIf (config.userDefinedGlobalVariables.hostname == "kvm-nixos-server"){
          userDefinedGlobalVariables.username = "drone";
          userDefinedGlobalVariables.hostTag = "vm_server";
          userDefinedGlobalVariables.initialHashedPassword = "$6$8jJvz/BcLMaK4yEN$tZ.bzF13N9i9deD8GkfROIkJ874.w2GPKN0xBeQ5RlZ40XpVPiUIi85Z/mkcq97y9qKnwyfujPZuxFhaDZTid0";
          userDefinedGlobalVariables.systemStateVersion = "23.05";
      })
     (lib.mkIf (config.userDefinedGlobalVariables.hostname == "homelab"){
          userDefinedGlobalVariables.username = "kmedrish";
          userDefinedGlobalVariables.hostTag = "homelab";
          userDefinedGlobalVariables.systemStateVersion = "23.11";
          userDefinedGlobalVariables.syncthingConfigDirectory = "/var/lib/syncthing/.config/syncthing";
          userDefinedGlobalVariables.syncthingUser = "syncthing";
      })
   ];
}
