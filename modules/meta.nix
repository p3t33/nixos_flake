# The meta.nix was created in order to encapsulate variables with same values
# that are being set across multiple files. Some of the values are
# evaluated "dynamically" via an if statement based on the value that machines
# sets for its hostname.
{
  config,
  pkgs,
  lib,
  machineName,
  ...
}:

{

  options =
    with lib;
    with types;
    {

      userDefinedGlobalVariables = {

        fontPackages = mkOption {
          default = with pkgs; [
            nerdfonts
            powerline-fonts
            font-awesome
          ];
          type = listOf package;
          description = "List of font packages to be used";
        };

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

        mediaGroup = mkOption {
          # Helps to avoid brakeage and as a best practice shoud be the same version
          # as the reset of the system.
          default = "media";
          type = str;
          description = "defies the media group";
        };

        dataGroup = mkOption {
          # Helps to avoid brakeage and as a best practice shoud be the same version
          # as the reset of the system.
          default = "data";
          type = str;
          description = "defies the data group";
        };

        homeLabSubnetPrefix = mkOption {
          # Helps to avoid brakeage and as a best practice shoud be the same version
          # as the reset of the system.
          default = "10.100.102.";
          type = str;
          description = "defines the static ip used by homelab machine";
        };

        homeLabIP = mkOption {
          # Helps to avoid brakeage and as a best practice shoud be the same version
          # as the reset of the system.
          default = "${config.userDefinedGlobalVariables.homeLabSubnetPrefix}73";
          type = str;
          description = "defines the static ip used by homelab machine";
        };

        homeLabGateway = mkOption {
          # Helps to avoid brakeage and as a best practice shoud be the same version
          # as the reset of the system.
          default = "${config.userDefinedGlobalVariables.homeLabSubnetPrefix}1";
          type = str;
          description = "defines the static ip used by homelab machine";
        };


        pathToDataDirectory = mkOption {
          default = "/mnt/data";
          type = str;
          description = "path of data directoy on homelab host";
        };

        pathToMediaDirectory = mkOption {
          default = "/mnt/media";
          type = str;
          description = "path of media directoy on homelab host";
        };

        pathToFlakeDirectory = mkOption {
          default = "${config.home.homeDirectory}/projects/nixos_flake";
          type = str;
          description = "used to define host hostname";
        };

        # Created for code reuse for flakeRepositoryUrl and system.autoUpgrade.
        githubFlakeRepositoryName = mkOption {
          default = "p3t33/nixos_flake";
          type = str;
          description = "name of the flake repository on github";
        };
        flakeRepositoryUrl = mkOption {
          default = "https://github.com/${config.userDefinedGlobalVariables.githubFlakeRepositoryName}.git";
          type = str;
          description = "Used as part of the zsh update/upgrade aliases.";
        };

        primeUsername = mkOption {
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

        hostConfigurationName = mkOption {
          type = str;
          default = "used as an argument for flake configuration.";
        };

        primeUserHomeDirectory = mkOption {
          type = str;
          default = "/home/${config.userDefinedGlobalVariables.primeUsername}";
        };

        syncthingConfigDirectory = mkOption {
          type = str;
          default = "${config.userDefinedGlobalVariables.primeUserHomeDirectory}/.config/syncthing";
        };
        syncthingUser = mkOption {
          type = str;
          default = "${config.userDefinedGlobalVariables.primeUsername}";
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
          default = ../machines/${config.userDefinedGlobalVariables.hostConfigurationName}/home.nix;
          type = path;
          description = "the relative path inside the repository of the wallpaper file and the .nix file that will be sourcing it";
        };

        NixOSSecretsPath = mkOption {
          default = ../machines/${config.userDefinedGlobalVariables.hostConfigurationName}/secrets.yaml;
          type = path;
          description = "the relative path inside the repository of the wallpaper file and the .nix file that will be sourcing it";
        };

        sopsKeyPath = mkOption {
          default = "/keys.txt";
          type = str;
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

        nvidiaHybridWithIntel = mkOption {
          type = attrsOf str;
          default = {
            nvidiaBusId = "";
            intelBusId = "";
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
            code = "";
            cherrytree = "";
            chrome = "";
            terminal = "";
            buildserver = "";
            vm = "";
          };
          description = "Icon definitions";
        };

        servicePort = mkOption {
          type = attrsOf int;
          default = {
            deglue = 8112;
            adguard = 3000;
            syncthing = 8384;
          };
          description = "default ports used by services";
        };

        #"${config.userDefinedGlobalVariables.workspaces.ws9}"
        workspaces = mkOption {
          type = attrsOf str;
          default = {
            ws1 = "1: ${config.userDefinedGlobalVariables.workspaces_icons.firefox} Firefox";
            ws2 = "2: ${config.userDefinedGlobalVariables.workspaces_icons.code} Code";
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
    (lib.mkIf (machineName == "HP-Zbook") {
      userDefinedGlobalVariables.hostConfigurationName = "work_pc";
      userDefinedGlobalVariables.systemStateVersion = "23.05";
      userDefinedGlobalVariables.nvidiaHybridWithIntel.nvidiaBusId = "PCI:01:00:0";
      userDefinedGlobalVariables.nvidiaHybridWithIntel.intelBusId = "PCI:00:02:0";
    })

    (lib.mkIf (machineName == "home-desktop") {
      userDefinedGlobalVariables.hostConfigurationName = "home_desktop";
      userDefinedGlobalVariables.wallpaperName = "crane_at_night.png";
      userDefinedGlobalVariables.systemStateVersion = "23.11";
    })
    (lib.mkIf (machineName == "kvm-nixos-server") {
      userDefinedGlobalVariables.primeUsername = "drone";
      userDefinedGlobalVariables.hostConfigurationName = "vm_server";
      userDefinedGlobalVariables.systemStateVersion = "24.05";
    })
    (lib.mkIf (machineName == "homelab") {
      userDefinedGlobalVariables.hostConfigurationName = "homelab";
      userDefinedGlobalVariables.systemStateVersion = "24.05";
      userDefinedGlobalVariables.syncthingConfigDirectory = "/var/lib/syncthing/.config/syncthing";
      userDefinedGlobalVariables.syncthingUser = "syncthing";
    })
    (lib.mkIf (machineName == "generic_linux_distro") {
      userDefinedGlobalVariables.hostConfigurationName = "generic_linux_distro";
      userDefinedGlobalVariables.systemStateVersion = "23.11";
      userDefinedGlobalVariables.sopsKeyPath = "${config.userDefinedGlobalVariables.primeUserHomeDirectory}/.config/sops/age/keys.txt";
    })
  ];
}
