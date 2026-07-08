{ config, lib, hostSpecific, ... }:
let
  cfg = config.custom.virtualization.incus;
  bridgeName = "incusbr0";
in
{
  options.custom.virtualization.incus.storagePoolSize = lib.mkOption {
    type = lib.types.str;
    default = "20GiB";
    description = "Size of the file-backed ZFS pool created by Incus preseed.";
  };

  config = lib.mkIf config.virtualisation.incus.enable {
    # Incus on NixOS is unsupported with iptables (enforced by a module
    # assertion). nftables is also required so Incus's own nft table coexists
    # with the firewall rather than fighting a legacy ip_tables backend.
    #
    # WARNING: enabling nftables blacklists the legacy ip_tables module. Any
    # service that shells out to raw `iptables` for NAT (e.g. the WireGuard
    # server's MASQUERADE postSetup hook) must be verified/converted to nft
    # before enabling Incus on that host.
    networking.nftables.enable = true;

    # With native nftables the firewall and Incus live in SEPARATE base chains
    # at the same hooks. An accept in one chain is not final, so nixos-fw's drop
    # policy silently eats instance DHCP/DNS on incusbr0 unless the bridge is
    # trusted. (Same lesson as the libvirt bridge in kvm.nix.) Lists merge
    # across modules, so this appends to any other trustedInterfaces.
    networking.firewall.trustedInterfaces = [ bridgeName ];

    # Non-root access to the Incus daemon socket.
    users.users.${hostSpecific.primeUsername}.extraGroups = [ "incus-admin" ];

    # Declarative init (replaces `incus admin init`). preseed is create/update
    # only -- it never REMOVES resources removed from here.
    #
    # incusbr0 uses 10.0.100.0/24 (NAT). Storage is a file-backed zfs pool: on
    # hosts whose root is already zfs (via disko) this loopback is transitional
    # -- migrate to a native dataset (config.source = "<pool>/incus", drop size)
    # once a real pool exists.
    virtualisation.incus.preseed = {
      storage_pools = [
        {
          name = "default";
          driver = "zfs";
          config.size = cfg.storagePoolSize;
        }
      ];
      networks = [
        {
          name = bridgeName;
          type = "bridge";
          config = {
            "ipv4.address" = "10.0.100.1/24";
            "ipv4.nat" = "true";
            "ipv6.address" = "none";
          };
        }
      ];
      profiles = [
        {
          name = "default";
          devices = {
            eth0 = {
              name = "eth0";
              network = bridgeName;
              type = "nic";
            };
            root = {
              path = "/";
              pool = "default";
              type = "disk";
            };
          };
        }
      ];
    };
  };
}
