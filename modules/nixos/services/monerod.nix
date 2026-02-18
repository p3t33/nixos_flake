{ config, lib, ... }:

let
  p2pPort = 18080;
in
{
  options.custom = {
    monerod = {
      zmqPort = lib.mkOption {
        type = lib.types.port;
        default = 18083;
        description = "Port for monerod's ZMQ publisher.";
      };
    };
  };

  config = lib.mkIf config.services.monero.enable {
    services.monero = {
      dataDir = "/var/lib/monero";
      rpc.port = 18081;
      extraConfig = ''
        prune-blockchain=1
        zmq-pub=tcp://${config.custom.shared.localHostIPv4}:${toString config.custom.monerod.zmqPort}
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
