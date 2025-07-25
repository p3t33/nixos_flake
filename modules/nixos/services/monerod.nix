{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.monerod;
  p2pPort = 18080;
in
{
  options.customOptions = {
    enableModule.monerod = lib.mkEnableOption "Enable monerod (monero node)";

    monerod = {
      zmqPort = lib.mkOption {
        type = lib.types.port;
        default = 18083;
        description = "Port for monerod's ZMQ publisher.";
      };
    };
  };

  config = lib.mkIf cfg {
    services.monero = {
      enable = true;
      dataDir = "/var/lib/monero";
      rpc.port = 18081;
      extraConfig = ''
        prune-blockchain=1
        zmq-pub=tcp://${config.customGlobalOptions.localHostIPv4}:${toString config.customOptions.monerod.zmqPort}
        out-peers=32
        in-peers=64
        disable-dns-checkpoints=1
        enable-dns-blocklist=1
      '';
      priorityNodes = [
        "p2pmd.xmrvsbeast.com:${toString p2pPort}"
        "nodes.hashvault.pro:${toString p2pPort}"
      ];
    };

    networking.firewall.allowedTCPPorts = [ p2pPort ];
  };
}
