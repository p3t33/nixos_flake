# Shared options consumed by multiple configuration files.
# Some values are derived from the machine's hostname.
{
  config,
  pkgs,
  lib,
  hostSpecific,
  ...
}:
{
  config.assertions =
    let
      categoryValues = builtins.attrValues config.custom.media.downloadCategories;
    in
    [
      {
        assertion = (builtins.length (lib.unique categoryValues)) == (builtins.length categoryValues);
        message = "custom.media.downloadCategories values must be unique.";
      }
    ];

  options.custom = {
    media.downloadCategories = lib.mkOption {
      type = lib.types.submodule {
        freeformType = lib.types.attrsOf lib.types.str;
        options = {
          default = lib.mkOption {
            default = "default";
            type = lib.types.str;
            description = "Default download category name.";
          };

          movies = lib.mkOption {
            default = "movies";
            type = lib.types.str;
            description = "Movie download category name.";
          };

          tv = lib.mkOption {
            default = "tv";
            type = lib.types.str;
            description = "TV download category name.";
          };

          ebooks = lib.mkOption {
            default = "ebooks";
            type = lib.types.str;
            description = "Ebook download category name.";
          };

          audiobooks = lib.mkOption {
            default = "audiobooks";
            type = lib.types.str;
            description = "Audiobook download category name.";
          };

          audio = lib.mkOption {
            default = "audio";
            type = lib.types.str;
            description = "Usenet audio download category name.";
          };

          software = lib.mkOption {
            default = "software";
            type = lib.types.str;
            description = "Usenet software download category name.";
          };

          books = lib.mkOption {
            default = "books";
            type = lib.types.str;
            description = "Usenet books download category name.";
          };
        };
      };
      default = { };
      description = "Shared media download category names.";
    };

    shared = {
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
        description = "Path to the machine-specific secrets directory";
      };

      databaseSecret = lib.mkOption {
        default = ../machines/${hostSpecific.hostName}/prowlarr.db;
        type = lib.types.path;
        description = "Path to the Prowlarr database secret file";
      };

      sopsKeyPath = lib.mkOption {
        default = "/etc/sops/age/keys.txt";
        type = lib.types.str;
        description = "Path to the SOPS age key file";
      };

      # To get the list of all avaliable fonts
      #
      # fc-list : family | sort -u
      font = lib.mkOption {
        default = {
          mono = "JetBrainsMono Nerd Font Mono";  # Monospaced: every character same width (i == w). Used for code/terminal
          serif = "DejaVu Serif";  # Proportional with decorative strokes/feet on letters (i < w). Example: Times New Roman. Used for formal documents
          sansSerif = "UbuntuSans Nerd Font";  # Proportional without decorative strokes (i < w). Example: Arial/Ubuntu. Used for modern UI/reading
        };
        type = lib.types.attrsOf lib.types.str;
        description = "Fonts to be used on the system";
      };

      # Artifact/cache estimates (FP16 KV, one sequence; 128K = 131,072 tokens):
      # - gemma4:e2b: 7.2 GB artifact; ~11.86 MiB cache at 1K; ~774 MiB at 128K.
      # - qwen3.5:4b: 3.4 GB artifact; ~85.41 MiB cache at 1K; ~4.55 GiB at 128K with MTP.
      AIDefaultModels = lib.mkOption {
        default = {
          quickAnswer = "gemma4:e2b";
          quickAnswerCandidate = "qwen3.5:4b";
        };
        type = lib.types.attrsOf lib.types.str;
        description = "Default AI models for different use cases";
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

          description = "SSH public keys for machines that need passwordless login";
      };

      pathToDataDirectory = lib.mkOption {
        default = "/data";
        type = lib.types.str;
        description = "Path to the data directory";
      };

      pathToMediaDirectory = lib.mkOption {
        default = "/media";
        type = lib.types.str;
        description = "Path to the media directory";
      };

      # shared between home-manger and nixos.
      syncthing = lib.mkOption {
        type = lib.types.submodule {
          options = {

            syncDir = lib.mkOption {
              default = "${config.custom.shared.primeUserHomeDirectory}/Sync";
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
