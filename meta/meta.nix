{config, pkgs,lib,  ...}:

{

  options = with lib; with types; {

    userDefinedGlobalVariables = {

    homeManagerStateVersion = mkOption {
      # Helps to avied brakeage and as a best practice shoud be the same version
      # as the reset of the system.
      default = "23.05";
      type = str;
      description = "Defines the realease that Home Manager has backwards compatibility with";
    };

    pathToFlakeDirectory = mkOption {
      default = "${config.home.homeDirectory}/projects/nixos_flake";
      type = str;
      description = "used to define host hostname";
    };

    flakeRepositoryUrl = mkOption {
      default = "git@github.com:p3t33/nixos_flake.git";
      type = str;
      description = "used to define host hostname";
    };

    font = mkOption {
      default =  "JetBrainsMono Nerd Font";
      type = str;
      description = "Font to be used on the system, depended on NerdFonts to be installed";
    };
    hostname = mkOption {
      default = "";
      type = str;
      description = "used to define host hostname";
    };

    email = mkOption {
        type = str;
        default = "kobi.medrish@gmail.com";
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

    gitUser = mkOption {
        type =  str;
        default = "Kobi Medrish";
    };

    hostTag = mkOption {
        type = str;
        default = "used as an argument for flake update";
    };

    homeDirectory = mkOption {
        type = str;
        default = "/home/${config.userDefinedGlobalVariables.username}";
    };

    initialPassword = mkOption {
        type = str;
        default = "changeme";
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
          userDefinedGlobalVariables.email = "kobi.medrish@motorolasolutions.com";
          userDefinedGlobalVariables.username = "kmedrish";
          userDefinedGlobalVariables.hostTag = "work_pc";
      })

     (lib.mkIf (config.userDefinedGlobalVariables.hostname == "home-desktop"){
          userDefinedGlobalVariables.username = "kmedrish";
          userDefinedGlobalVariables.hostTag = "home_desktop";
          userDefinedGlobalVariables.wallpaperName = "crane_at_night.png";
      })

     (lib.mkIf (config.userDefinedGlobalVariables.hostname == "kvm-nixos-gui"){
          userDefinedGlobalVariables.username = "kmedrish";
          userDefinedGlobalVariables.hostTag = "vm_gui";
          userDefinedGlobalVariables.initialPassword = "q";
      })

     (lib.mkIf (config.userDefinedGlobalVariables.hostname == "kvm-nixos-server"){
          userDefinedGlobalVariables.username = "drone";
          userDefinedGlobalVariables.hostTag = "vm_server";
          userDefinedGlobalVariables.initialPassword = "q";
      })
   ];
}

