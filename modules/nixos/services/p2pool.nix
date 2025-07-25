{ config, lib, pkgs, ... }:

let
  cfg = config.custom.services.p2pool;
  p2poolUser = "p2pool";
in
{
  options.custom.services.p2pool = {
    enable = lib.mkEnableOption "Enable P2Pool service";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.p2pool;
      description = "The p2pool package to use.";
    };

    p2pPort = lib.mkOption {
      type = lib.types.port;
      default = 37889;
      description = "Port used by P2Pool to sync with other P2Pool nodes (open in firewall).";
    };

    address = lib.mkOption {
      type = lib.types.str;
      example = "43...YourMoneroAddress...";
      description = "Your Monero address to receive pool rewards.";
    };

    monerodHost = lib.mkOption {
      type = lib.types.str;
      default = config.customGlobal.localHostIPv4;
      description = "The Monero daemon RPC host.";
    };

    rpcPort = lib.mkOption {
      type = lib.types.port;
      default = config.services.monero.rpc.port;
      description = "The Monero daemon RPC port.";
    };

    stratumPort = lib.mkOption {
      type = lib.types.port;
      default = 3333;
      description = "The port on which the P2Pool stratum server listens for miner connections.";
    };

    stratum = lib.mkOption {
      type = lib.types.str;
      default = "${config.customGlobal.anyIPv4}:${toString config.custom.services.p2pool.stratumPort}";
      description = "The full address (IP:port) the P2Pool stratum server binds to, used by miners (e.g., xmrig) to submit shares.";
    };

    dataDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/p2pool";
      description = "Directory for P2Pool data.";
    };

    extraArgs = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      example = [ "--no-daemon" ];
      description = "Extra command-line arguments to pass to p2pool.";
    };
  };

  config = lib.mkIf cfg.enable {
    sops.secrets.xmr_wallet = {
      restartUnits = [ config.systemd.services.p2pool.name];
    };

    users.users.${p2poolUser} = {
      isSystemUser = true;
      group = p2poolUser;
      home = cfg.dataDir;
      createHome = true;
    };

    users.groups.${p2poolUser} = {};

    systemd.services.p2pool = {
      description = "Monero P2Pool Node";
      after = [
        "network.target"
        config.systemd.services.monero.name
      ];

      wantedBy = [ config.systemd.targets.multi-user.name ];

      # the wallet address is passed to the services as an envionment file,
      # haveing a single line:
      #
      # XMR_WALLET=<wallet address>
      #
      # This file is generted form a sops-secret with a single yaml line:
      #
      # xmr_wallet: XMR_WALLET=<wallet address>
      serviceConfig = {
        User = p2poolUser;
        Group = p2poolUser;
        WorkingDirectory = cfg.dataDir;
        EnvironmentFile = config.sops.secrets.xmr_wallet.path;
        ExecStart = ''
          ${cfg.package}/bin/p2pool \
            --wallet $XMR_WALLET \
            --rpc-port ${toString cfg.rpcPort} \
            --host ${cfg.monerodHost} \
            --stratum ${cfg.stratum} \
            ${lib.concatStringsSep " " cfg.extraArgs}
        '';
        Restart = "always";
      };
    };

    networking.firewall.allowedTCPPorts = [ cfg.p2pPort ];
  };
}
