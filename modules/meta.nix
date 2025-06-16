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
  hostSpecific,
  ...
}:

{
  options = {
    userDefinedGlobalVariables = {
      fontPackages = lib.mkOption {
        default = [
          pkgs.powerline-fonts
          pkgs.font-awesome
        ] ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);
        type = lib.types.listOf lib.types.package;
        description = "List of font packages to be used";
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

      localHostIPv4 = lib.mkOption {
        default = "127.0.0.1";
        type = lib.types.str;
        description = "Defines the static IP used by the homelab machine";
      };

      anyIPv4 = lib.mkOption {
          default = "0.0.0.0";
          type = lib.types.str;
          description = "Defines an IPv4 address that binds to all available network interfaces.";
      };

      timeZone = lib.mkOption {
          default = "Asia/Jerusalem";
          type = lib.types.str;
          description = "Defines an IPv4 address that binds to all available network interfaces.";
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

      pathToUsenetDirectory = lib.mkOption {
        default = "${config.userDefinedGlobalVariables.pathToMediaDirectory}/usenet";
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

      # shared between home-manger and nixos.
      primeUserHomeDirectory = lib.mkOption {
        default = "/home/${hostSpecific.primeUsername}";
        type = lib.types.str;
        description = "Defines the primary user's home directory";
      };

      secretsPath = lib.mkOption {
        default = ../machines/${hostSpecific.hostName}/secrets;
        type = lib.types.path;
        description = "the relative path inside the repository of the wallpaper file and the .nix file that will be sourcing it";
      };

      NixOSDefaultSecretsPath = lib.mkOption {
        default = ../machines/${hostSpecific.hostName}/secrets/nixos/secrets.yaml;
        type = lib.types.path;
        description = "The relative path to the NixOS secrets file";
      };

      homeManagerAsNixOSModuleDefaultSecretsPath = lib.mkOption {
        default = ../machines/${hostSpecific.hostName}/secrets/home-manager/secrets.yaml;
        type = lib.types.path;
        description = "The relative path to the NixOS secrets file";
      };

      calibreAsNixOSModuleSecretsPath = lib.mkOption {
        default = ../machines/${hostSpecific.hostName}/calibre.yaml;
        type = lib.types.path;
        description = "The relative path to the NixOS secrets file";
      };

      databaseSecret = lib.mkOption {
        default = ../machines/${hostSpecific.hostName}/prowlarr.db;
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

      bridgedNetwork = lib.mkOption {
        default = {
          bridgeName = "br0";
          physicalInterface = "enp0s13f0u3";
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

      wireguard = lib.mkOption {
        default = let
          base = "10.100.0";
        in {
          networkName = "wg0";
          externalInterface = "eno1";
          baseSubnet = base;
          gateway = "${base}.1";
          network = "${base}.0/24";
        };
        type = lib.types.attrsOf lib.types.str;
        description = "Default values used by wireguard";
      };

      servicePort = lib.mkOption {
        default = {
          jellyfin = 8096;
          wireguard = 51820;
          homeAssistant = 8123;
          mosquitto = 1883;
          zigbee2mqttFrontend = 8124;
          sabnzbd = 8080;
        };

        type = lib.types.attrsOf (lib.types.oneOf [ lib.types.int (lib.types.attrsOf lib.types.int) ]);
        description = "Default ports used by various services (single port or multiple ports per service)";
      };


      sshPublicKeys = lib.mkOption {
          default = {
              work-pc = {
                  key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFMZ7lwYos3kwgGNff76kAjUchcSAT2yjKWwf0dZKtsY openpgp:0xEDC5F14F";
              };

              home-desktop = {
                  key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPvNm5eCx93uwyiJUIy/scH8UMOUfifzw6PrYLLbBnV+ openpgp:0x6799A2F2";
              };
          };

          type = lib.types.attrsOf (lib.types.oneOf [
                  lib.types.str
                  (lib.types.attrsOf lib.types.str)
          ]);

          description = "Default ports used by various services (single port or multiple ports per service, as strings)";
      };

      sshPublicKey = lib.mkOption {
        default = "";
        type = lib.types.str;
        description = "The relative path to the SOPS key file";
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
          home-assistant = "home-assistant";
          work-pc = "work-pc";
          home-desktop = "home-desktop";
          homelab = "homelab";
          generic-linux-distro = "generic-linux-distro";
        };

        type = lib.types.attrsOf lib.types.str;
        description = "avalible machines";
      };

      # shared between home-manger and nixos.
      syncthing = lib.mkOption {
        type = lib.types.submodule {
          options = {

            syncDir = lib.mkOption {
              default = "${config.userDefinedGlobalVariables.primeUserHomeDirectory}/Sync";
              type = lib.types.str;
              description = "Defines the Syncthing sync directory";
            };
          };
        };
        default = {};
        description = "Syncthing related configuration";
        };
    };
  };
}
