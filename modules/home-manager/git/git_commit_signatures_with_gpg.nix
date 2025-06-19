{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.gitCommitSignaturesWithGpg;
in
{
  options.customOptions.enableModule.gitCommitSignaturesWithGpg = lib.mkEnableOption "Enable Git configuration";

  config = lib.mkIf cfg {
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

