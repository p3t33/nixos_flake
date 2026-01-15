{ config, lib, pkgs, ... }:
let
  cfg = config.programs.aider-chat;
in
{
  # Can be used both with local models(via ollama) and with remote once.
  # intended for level 2(file scoped) and 3(file agent) models(as described in ollama.nix file).
  config = lib.mkIf cfg.enable {
    programs.aider-chat = {
      package = pkgs.aider-chat;
      # based on yaml configs form aider webpage.
      settings = {
        gitignore = true;
      };
    };
  };
}

