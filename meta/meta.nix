{config, pkgs,lib,  ...}:

{

  options = with lib; with types; {

    userDefinedGlobalVariables = {
    enable = mkOption {
      default = true;
      type = bool;
      description = "enables user defined global variables";
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
        default = "";
    };

    initialPassword = mkOption {
        type = str;
        default = "changeme";
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

    };
  };

  config = lib.mkIf config.userDefinedGlobalVariables.enable (lib.mkMerge [
     (lib.mkIf (config.userDefinedGlobalVariables.hostname == "HP-Zbook"){
          userDefinedGlobalVariables.email = "kobi.medrish@motorolasolutions.com";
          userDefinedGlobalVariables.username = "kmedrish";
          userDefinedGlobalVariables.hostTag = "work_pc";
          userDefinedGlobalVariables.homeDirectory = "/home/${config.userDefinedGlobalVariables.username}";
      })

     (lib.mkIf (config.userDefinedGlobalVariables.hostname == "home-desktop"){
          userDefinedGlobalVariables.username = "kmedrish";
          userDefinedGlobalVariables.hostTag = "home_desktop";
          userDefinedGlobalVariables.homeDirectory = "/home/${config.userDefinedGlobalVariables.username}";
      })

     (lib.mkIf (config.userDefinedGlobalVariables.hostname == "kvm-nixos-gui"){
          userDefinedGlobalVariables.username = "kmedrish";
          userDefinedGlobalVariables.hostTag = "vm_gui";
          userDefinedGlobalVariables.homeDirectory = "/home/${config.userDefinedGlobalVariables.username}";
          userDefinedGlobalVariables.initialPassword = "q";
      })

     (lib.mkIf (config.userDefinedGlobalVariables.hostname == "kvm-nixos-server"){
          userDefinedGlobalVariables.username = "drone";
          userDefinedGlobalVariables.hostTag = "vm_server";
          userDefinedGlobalVariables.homeDirectory = "/home/${config.userDefinedGlobalVariables.username}";
          userDefinedGlobalVariables.initialPassword = "q";

      })

   ]);
}

