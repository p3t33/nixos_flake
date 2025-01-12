# The meta.nix was created in order to encapsulate variables with same values
# that are being set across multiple files. Some of the values are
# evaluated "dynamically" via an if statement based on the value that machines
# sets for its hostname.
# The meta.nix was created to encapsulate variables with the same values
# that are being set across multiple files.
{
  config,
  pkgs,
  lib,
  machineName,
  ...
}:

{
  options = {
    userDefinedGlobalVariables = {
      fontPackages = lib.mkOption {
        default = [
          pkgs.nerdfonts
          pkgs.powerline-fonts
          pkgs.font-awesome
        ];
        type = lib.types.listOf lib.types.package;
        description = "List of font packages to be used";
      };

      systemStateVersion = lib.mkOption {
        default = "";
        type = lib.types.str;
        description = "Defines the release version when OS was first installed";
      };

      neovimCustomDictionaryPathrelativeToHomeDirectory = lib.mkOption {
        default = ".config/nvim/spell/nixen.utf-8";
        type = lib.types.str;
        description = "Defines the relative path to dictionary";
      };

      homeManagerStateVersion = lib.mkOption {
        default = "24.05";
        type = lib.types.str;
        description = "Defines the release that Home Manager has backwards compatibility with";
      };

      mediaGroup = lib.mkOption {
        default = "media";
        type = lib.types.str;
        description = "Defines the media group";
      };

      dataGroup = lib.mkOption {
        default = "data";
        type = lib.types.str;
        description = "Defines the data group";
      };

      homeLabSubnetPrefix = lib.mkOption {
        default = "10.100.102.";
        type = lib.types.str;
        description = "Defines the static IP subnet prefix used by homelab machines";
      };

      homeLabIP = lib.mkOption {
        default = "${config.userDefinedGlobalVariables.homeLabSubnetPrefix}73";
        type = lib.types.str;
        description = "Defines the static IP used by the homelab machine";
      };

      routerIP = lib.mkOption {
        default = "${config.userDefinedGlobalVariables.homeLabSubnetPrefix}1";
        type = lib.types.str;
        description = "Defines the static IP used by the homelab machine";
      };

      proxmoxIP = lib.mkOption {
        default = "${config.userDefinedGlobalVariables.homeLabSubnetPrefix}74";
        type = lib.types.str;
        description = "Defines the static IP used by the homelab machine";
      };

      homeLabGateway = lib.mkOption {
        default = "${config.userDefinedGlobalVariables.homeLabSubnetPrefix}1";
        type = lib.types.str;
        description = "Defines the gateway IP for the homelab machine";
      };

      pathToDataDirectory = lib.mkOption {
        default = "/mnt/data";
        type = lib.types.str;
        description = "Path to the data directory on the homelab host";
      };

      pathToMediaDirectory = lib.mkOption {
        default = "/mnt/media";
        type = lib.types.str;
        description = "Path to the media directory on the homelab host";
      };

      pathToFlakeDirectory = lib.mkOption {
        default = "${config.home.homeDirectory}/projects/nixos_flake";
        type = lib.types.str;
        description = "Defines the path to the flake directory";
      };

      githubFlakeRepositoryName = lib.mkOption {
        default = "p3t33/nixos_flake";
        type = lib.types.str;
        description = "Name of the flake repository on GitHub";
      };

      flakeRepositoryUrl = lib.mkOption {
        default = "https://github.com/${config.userDefinedGlobalVariables.githubFlakeRepositoryName}.git";
        type = lib.types.str;
        description = "URL of the flake repository used in Zsh update/upgrade aliases";
      };

      primeUsername = lib.mkOption {
        default = "kmedrish";
        type = lib.types.str;
        description = "Defines the primary username";
      };

      editor = lib.mkOption {
        default = "nvim";
        type = lib.types.str;
        description = "Defines the default editor";
      };

      manPager = lib.mkOption {
        default = "nvim +Man!";
        type = lib.types.str;
        description = "Defines the man pager command";
      };

      hostConfigurationName = lib.mkOption {
        default = "used as an argument for flake configuration.";
        type = lib.types.str;
        description = "Defines the host configuration name";
      };

      primeUserHomeDirectory = lib.mkOption {
        default = "/home/${config.userDefinedGlobalVariables.primeUsername}";
        type = lib.types.str;
        description = "Defines the primary user's home directory";
      };

      syncthingDataDirectory = lib.mkOption {
        default = "/var/lib/syncthing/";
        type = lib.types.str;
        description = "Defines the Syncthing configuration directory";
      };

      syncthingSyncDir = lib.mkOption {
        default = "${config.userDefinedGlobalVariables.primeUserHomeDirectory}/Sync";
        type = lib.types.str;
        description = "Defines the Syncthing configuration directory";
      };

      syncthingConfigDirectory = lib.mkOption {
        default = "${config.userDefinedGlobalVariables.primeUserHomeDirectory}/.config/syncthing";
        type = lib.types.str;
        description = "Defines the Syncthing configuration directory";
      };

      syncthingUser = lib.mkOption {
        default = "${config.userDefinedGlobalVariables.primeUsername}";
        type = lib.types.str;
        description = "Defines the Syncthing user";
      };

      wallpaperName = lib.mkOption {
        default = "mountain.jpg";
        type = lib.types.str;
        description = "The name of the wallpaper file";
      };

      wallpaperOut = lib.mkOption {
        default = "wallpaper/${config.userDefinedGlobalVariables.wallpaperName}";
        type = lib.types.str;
        description = "Defines the path where the wallpaper will be located";
      };

      wallpaperIn = lib.mkOption {
        default = ../wallpaper/${config.userDefinedGlobalVariables.wallpaperName};
        type = lib.types.path;
        description = "The relative path to the wallpaper file inside the repository";
      };

      home_manger_import_path = lib.mkOption {
        default = ../machines/${config.userDefinedGlobalVariables.hostConfigurationName}/home.nix;
        type = lib.types.path;
        description = "The relative path inside the repository to the home configuration file";
      };

      secretsPath = lib.mkOption {
        default = ../machines/${config.userDefinedGlobalVariables.hostConfigurationName}/secrets;
        type = lib.types.path;
        description = "the relative path inside the repository of the wallpaper file and the .nix file that will be sourcing it";
      };

      NixOSDefaultSecretsPath = lib.mkOption {
        default = ../machines/${config.userDefinedGlobalVariables.hostConfigurationName}/secrets/nixos/secrets.yaml;
        type = lib.types.path;
        description = "The relative path to the NixOS secrets file";
      };

      homeManagerAsNixOSModuleDefaultSecretsPath = lib.mkOption {
        default = ../machines/${config.userDefinedGlobalVariables.hostConfigurationName}/secrets/home-manager/secrets.yaml;
        type = lib.types.path;
        description = "The relative path to the NixOS secrets file";
      };

      calibreAsNixOSModuleSecretsPath = lib.mkOption {
        default = ../machines/${config.userDefinedGlobalVariables.hostConfigurationName}/calibre.yaml;
        type = lib.types.path;
        description = "The relative path to the NixOS secrets file";
      };

      databaseSecret = lib.mkOption {
        default = ../machines/${config.userDefinedGlobalVariables.hostConfigurationName}/prowlarr.db;
        type = lib.types.path;
        description = "the relative path inside the repository of the wallpaper file and the .nix file that will be sourcing it";
      };

      sopsKeyPath = lib.mkOption {
        default = "/keys.txt";
        type = lib.types.str;
        description = "The relative path to the SOPS key file";
      };

      font = lib.mkOption {
        default = {
          mono = "JetBrainsMono Nerd Font";
          serif = "DejaVu Serif";
          sansSerif = "DejaVu Sans";
        };
        type = lib.types.attrsOf lib.types.str;
        description = "Fonts to be used on the system";
      };

      nvidiaHybridWithIntel = lib.mkOption {
        default = {
          nvidiaBusId = "";
          intelBusId = "";
        };
        type = lib.types.attrsOf lib.types.str;
        description = "Bus IDs for Nvidia Hybrid with Intel setup";
      };

      colors = lib.mkOption {
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
        type = lib.types.attrsOf lib.types.str;
        description = "Defines the color palette for the user interface";
      };

      workspaces_icons = lib.mkOption {
        default = {
          firefox = "";
          code = "";
          cherrytree = "";
          chrome = "";
          terminal = "";
          buildserver = "";
          vm = "";
        };
        type = lib.types.attrsOf lib.types.str;
        description = "Icon definitions for workspace labels";
      };

      servicePort = lib.mkOption {
        default = {
          deglue = 8112;
          adguard = 3000;
          syncthing = 8384;
          sonarr = 8989;
          radarr = 7878;
          prowlarr = 9696;
          jackett = 9117;
          homepageDashboard = 8082;
          jellyfin = 8096;
        };
        type = lib.types.attrsOf lib.types.int;
        description = "Default ports used by various services";
      };

      workspaces = lib.mkOption {
        default = {
          ws1 = "1: ${config.userDefinedGlobalVariables.workspaces_icons.firefox} Firefox";
          ws2 = "2: ${config.userDefinedGlobalVariables.workspaces_icons.code} Code";
          ws3 = "3: ${config.userDefinedGlobalVariables.workspaces_icons.cherrytree} Cherrytree";
          ws4 = "4: ${config.userDefinedGlobalVariables.workspaces_icons.chrome} Chrome";
          ws5 = "5: ${config.userDefinedGlobalVariables.workspaces_icons.buildserver} BuildServer";
          ws6 = "6: ${config.userDefinedGlobalVariables.workspaces_icons.terminal} Terminal";
          ws7 = "7";
          ws8 = "8: ${config.userDefinedGlobalVariables.workspaces_icons.vm} VM";
          ws9 = "9: VPN";
          ws10 = "10";
        };
        type = lib.types.attrsOf lib.types.str;
        description = "Workspace definitions for i3wm";
      };

      machines = lib.mkOption {
        default = {
          kvm-nixos-server = "kvm-nixos-server";
          work-pc = "work-pc";
          home-desktop = "home-desktop";
          homelab = "homelab";
          generic-linux-distro = "generic-linux-distro";
        };

        type = lib.types.attrsOf lib.types.str;
        description = "avalible machines";
      };

      devicesToShareTaskWarriorFolderWith = lib.mkOption {
        default = [ ];
        type = lib.types.listOf lib.types.str;
        description = "List of devices to use for folder synchronization.";
      };

      devicesToShareDevResourcesFolderWith = lib.mkOption {
        default = [ ];
        type = lib.types.listOf lib.types.str;
        description = "List of devices to use for folder synchronization.";
      };

    };
  };

  config = lib.mkMerge [
    # Machine-specific configurations
    (lib.mkIf (machineName == "${config.userDefinedGlobalVariables.machines.work-pc}") {
      userDefinedGlobalVariables.hostConfigurationName = "${config.userDefinedGlobalVariables.machines.work-pc
      }";
      userDefinedGlobalVariables.systemStateVersion = "24.05";
      userDefinedGlobalVariables.nvidiaHybridWithIntel.nvidiaBusId = "PCI:01:00:0";
      userDefinedGlobalVariables.nvidiaHybridWithIntel.intelBusId = "PCI:00:02:0";
      userDefinedGlobalVariables.devicesToShareTaskWarriorFolderWith = [
        "${config.userDefinedGlobalVariables.machines.homelab}"
        "${config.userDefinedGlobalVariables.machines.home-desktop}"
      ];
      userDefinedGlobalVariables.devicesToShareDevResourcesFolderWith = [
        "${config.userDefinedGlobalVariables.machines.homelab}"
        "${config.userDefinedGlobalVariables.machines.home-desktop}"
      ];
    })

    (lib.mkIf (machineName == "${config.userDefinedGlobalVariables.machines.home-desktop}") {
      userDefinedGlobalVariables.hostConfigurationName = "${config.userDefinedGlobalVariables.machines.home-desktop
      }";
      userDefinedGlobalVariables.wallpaperName = "crane_at_night.png";
      userDefinedGlobalVariables.systemStateVersion = "24.05";
      userDefinedGlobalVariables.devicesToShareTaskWarriorFolderWith = [
        "${config.userDefinedGlobalVariables.machines.homelab}"
        "${config.userDefinedGlobalVariables.machines.work-pc}"
      ];
      userDefinedGlobalVariables.devicesToShareDevResourcesFolderWith = [
        "${config.userDefinedGlobalVariables.machines.homelab}"
        "${config.userDefinedGlobalVariables.machines.work-pc}"
      ];
    })

    (lib.mkIf (machineName == "${config.userDefinedGlobalVariables.machines.kvm-nixos-server}") {
      userDefinedGlobalVariables.primeUsername = "drone";
      userDefinedGlobalVariables.hostConfigurationName = "${config.userDefinedGlobalVariables.machines.kvm-nixos-server
      }";
      userDefinedGlobalVariables.systemStateVersion = "24.11";
    })

    (lib.mkIf (machineName == "${config.userDefinedGlobalVariables.machines.homelab}") {
      userDefinedGlobalVariables.hostConfigurationName = "${config.userDefinedGlobalVariables.machines.homelab
      }";
      userDefinedGlobalVariables.systemStateVersion = "24.05";
      userDefinedGlobalVariables.syncthingConfigDirectory = "/var/lib/syncthing/.config/syncthing";
      userDefinedGlobalVariables.syncthingSyncDir = "/mnt/data/Sync";
      userDefinedGlobalVariables.syncthingUser = "syncthing";
      userDefinedGlobalVariables.devicesToShareTaskWarriorFolderWith = [
        "${config.userDefinedGlobalVariables.machines.work-pc}"
        "${config.userDefinedGlobalVariables.machines.home-desktop}"
      ];
      userDefinedGlobalVariables.devicesToShareDevResourcesFolderWith = [
        "${config.userDefinedGlobalVariables.machines.work-pc}"
        "${config.userDefinedGlobalVariables.machines.home-desktop}"
      ];
    })

    (lib.mkIf (machineName == "generic_linux_distro") {
      userDefinedGlobalVariables.hostConfigurationName = "generic_linux_distro";
      userDefinedGlobalVariables.systemStateVersion = "23.11";
      userDefinedGlobalVariables.sopsKeyPath = "${config.userDefinedGlobalVariables.primeUserHomeDirectory}/.config/sops/age/keys.txt";
    })
  ];
}
