{ config, lib, ... }:

let
  cfg = config.custom.programs.gitCommitSignaturesWithGpg;
in
{
  options.custom.programs.gitCommitSignaturesWithGpg.enable = lib.mkEnableOption "Enable Git configuration";

  config = lib.mkIf cfg.enable {
    programs.git = {

      extraConfig = {

        commit = {
          gpgSign = true;
        };

        gpg = {
          program = "gpg";
        };
      };
    };
  };
}

