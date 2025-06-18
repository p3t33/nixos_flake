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
  hostSpecific,
  ...
}:
{
  options = {
    customGlobalOptions = {
      fontPackages = lib.mkOption {
        default = [
          pkgs.powerline-fonts
          pkgs.font-awesome
        ] ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);
        type = lib.types.listOf lib.types.package;
        description = "List of font packages to be used";
      };

      # global
      mediaGroup = lib.mkOption {
        default = "media";
        type = lib.types.str;
        description = "Defines the media group";
      };

      # global
      dataGroup = lib.mkOption {
        default = "data";
        type = lib.types.str;
        description = "Defines the data group";
      };

      # global
      githubFlakeRepositoryName = lib.mkOption {
        default = "p3t33/nixos_flake";
        type = lib.types.str;
        description = "Name of the flake repository on GitHub";
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

      # shared between home-manger and nixos and multiple hosts
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

      # shared between home-manger and nixos.
      syncthing = lib.mkOption {
        type = lib.types.submodule {
          options = {

            syncDir = lib.mkOption {
              default = "${config.customGlobalOptions.primeUserHomeDirectory}/Sync";
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
