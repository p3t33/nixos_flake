{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.custom.networking.disableLegacyBridgeNetfilter;
in
{
  options.custom.networking.disableLegacyBridgeNetfilter.enable =
    lib.mkEnableOption "legacy bridge netfilter hook bypass";

  # Linux kernel documentation calls br_netfilter legacy and discourages its use. It is needed only
  # for legacy iptables, ip6tables, or arptables processing of bridged traffic, including NAT. For
  # intentional layer 2 filtering, it recommends native nftables bridge-family rules.
  #
  # https://docs.kernel.org/networking/bridge.html#netfilter
  #
  # libvirt explicitly recommends setting all three values to 0 for VM bridges because host firewall
  # rules commonly block guest traffic unexpectedly.
  #
  # https://wiki.libvirt.org/Net.bridge.bridge-nf-call_and_sysctl.conf.html
  #
  # This issue was observed with the libvirt internal bridge and with the defined bridge (br0).
  #
  # This implementation follows systemd's documented pattern:
  # https://www.freedesktop.org/software/systemd/man/latest/sysctl.d.html
  #
  # The sysctls disable the bridge-netfilter hooks. The udev rule reapplies them if br_netfilter
  # loads after the early-boot sysctl pass.
  config = lib.mkIf cfg.enable {
    boot.kernel.sysctl = {
      "net.bridge.bridge-nf-call-iptables" = 0;
      "net.bridge.bridge-nf-call-ip6tables" = 0;
      "net.bridge.bridge-nf-call-arptables" = 0;
    };

    services.udev.extraRules = ''
      ACTION=="add", SUBSYSTEM=="module", KERNEL=="br_netfilter", RUN+="${pkgs.systemd}/lib/systemd/systemd-sysctl --prefix=/net/bridge"
    '';
  };
}
