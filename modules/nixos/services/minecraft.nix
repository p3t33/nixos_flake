{ config, lib, pkgs, ... }:

let
  cfg = config.services.minecraft-server;
in
{
  config = lib.mkIf cfg.enable {
    services.minecraft-server = {
      eula = true;
      # nix eval --raw nixpkgs#minecraftServers --apply 's: builtins.concatStringsSep "\n" (builtins.attrNames s)'
      package = pkgs.pkgs.minecraftServers.vanilla-1-21;

      dataDir = "/var/lib/minecraft";
      openFirewall = true;

      declarative = true;
      jvmOpts = "-Xms512M -Xmx4G -XX:+UseG1GC";

      serverProperties = {
        server-port = 25565;
        motd = "NixOS Minecraft (Creative)";

        gamemode = "creative";
        difficulty = "peaceful";

        spawn-protection = 0;
        pvp = false;
        allow-flight = true;
        enable-command-block = true;

        online-mode = true;
        white-list = true;
        enforce-whitelist = true;
        # Explicitly disable RCON
        enable-rcon = false;
        broadcast-rcon-to-ops = false;

        view-distance = 10;
        simulation-distance = 8;
        max-players = 4;
      };

      # https://mcuuid.net/
      whitelist = {
      };
    };
  };
}
