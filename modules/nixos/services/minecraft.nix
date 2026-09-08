{ config, lib, pkgs, ... }:

let
  cfg = config.services.minecraft-server;
in
{
  config = lib.mkIf cfg.enable {
    sops.secrets.minecraft-whitelist = {
      owner = "minecraft";
      mode = "0400";
      restartUnits = [ "minecraft-server.service" ];
    };

    # Replace the whitelist symlink recreated by the upstream preStart script.
    systemd.services.minecraft-server.preStart = lib.mkAfter ''
      whitelist_tmp=$(mktemp .whitelist.XXXXXX)
      trap 'rm -f "$whitelist_tmp"' EXIT
      install -m 0600 ${lib.escapeShellArg config.sops.secrets.minecraft-whitelist.path} "$whitelist_tmp"

      if ! ${pkgs.jq}/bin/jq -e -s '
        length == 1 and (.[0] |
          type == "array" and all(.[];
            type == "object"
            and (.name | type == "string" and length > 0)
            and (.uuid | type == "string" and length == 36 and
              test("^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"))
          )
        )
      ' "$whitelist_tmp" >/dev/null 2>&1; then
        echo "Invalid Minecraft whitelist secret: expected a JSON array of name/UUID objects" >&2
        exit 1
      fi

      mv -Tf "$whitelist_tmp" whitelist.json
      trap - EXIT
    '';

    services.minecraft-server = {
      eula = true;
      # nix eval --raw nixpkgs#minecraftServers --apply 's: builtins.concatStringsSep "\n" (builtins.attrNames s)'
      # Review this explicit version choice on every NixOS stable upgrade.
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

        spawn-radius = 0;
        view-distance = 10;
        simulation-distance = 8;
        max-players = 4;
      };

    };
  };
}
