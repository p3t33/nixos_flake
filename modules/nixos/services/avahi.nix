{ config, lib, ... }:
{
  options.custom.services.avahi.publisher.enable =
    lib.mkEnableOption "mDNS/DNS-SD publishing — announces this host and allows services (CUPS, Samba) to register DNS-SD records via D-Bus";

  config = lib.mkMerge [
    # Basic Avahi configuration needed on any machine that consumes mDNS/DNS-SD.
    # Enables .local hostname resolution and allows mDNS traffic through the firewall.
    (lib.mkIf config.services.avahi.enable {
      services.avahi = {
        # Tells the OS to use Avahi when resolving .local hostnames. Without this,
        # programs calling getaddrinfo("nas.local") would skip mDNS and ask the
        # regular DNS server, which knows nothing about .local names.
        nssmdns4    = true;
        # mDNS uses UDP multicast (not TCP sessions) on port 5353. The firewall
        # must be open for mDNS traffic to flow in both directions — queries,
        # responses, and proactive announcements.
        openFirewall = true;
      };
    })

    # Publisher: makes this host discoverable on the LAN.
    #
    # publish.addresses — Avahi responds to mDNS queries asking "who is nas.local?"
    # with this host's IP. Also proactively multicasts its address records on
    # startup and on IP changes so other hosts can update their caches.
    #
    # publish.userServices — allows services such as CUPS and Samba to register
    # their own DNS-SD service records with Avahi via D-Bus, so Avahi can
    # broadcast them on the network (e.g. "Dymo printer available at nas:631").
    #
    # Setting enable = true here also triggers the consumer block above, so this
    # host gets nssmdns4 and openFirewall as well.
    (lib.mkIf config.custom.services.avahi.publisher.enable {
      services.avahi = {
        enable                = true; # starts the Avahi daemon.
        # Master switch for all publishing — without this, publish.addresses
        # and publish.userServices have no effect.
        publish.enable        = true;
        publish.addresses     = true; # respond to queries about this host's hostname/IP.
        # allows other services such as CUPS, Samba to register with Avahi
        # for it to broadcast them on the network.
        publish.userServices  = true;
      };
    })
  ];
}
