{ config, lib, pkgs, hostSpecific, ... }:
let
  subnet    = "${config.custom.shared.${hostSpecific.hostName}.subnetPrefix}0/24";
  localHost = config.custom.shared.localHostIPv4;
  anyIPv4   = config.custom.shared.anyIPv4;
  cupsPort  = 631;
in
{
  options.custom.services.printing = {
    client.enable = lib.mkEnableOption "CUPS client — enables printing and desktop extras (label software, lp group)";
    server.enable = lib.mkEnableOption "CUPS print server — shares locally-attached USB printers on the LAN";
  };

  config = lib.mkMerge [

    (lib.mkIf config.custom.services.printing.client.enable {
      # Starts the CUPS daemon. On its own this allows printing to any printer
      # explicitly configured, but no auto-discovery of network printers.
      # Auto-discovery is handled by cups-browsed, which defaults to enabled
      # when avahi is running (services.printing.browsed.enable defaults to
      # services.avahi.enable). cups-browsed subscribes to Avahi's D-Bus and
      # receives events when _ipp._tcp DNS-SD records appear or disappear,
      # then automatically adds or removes them as permanent CUPS queues.
      #
      # So for this config to work with WiFi printers that use mDNS it is assumed
      # that the avahi configuration is also enabled.
      #
      # the web GUI for CUPS is available on http://localhost:<cupsPort>.
      services.printing.enable = true;

      # cups-browsed does not need to be explicitly enabled here — it defaults to
      # services.avahi.enable, so it starts automatically when Avahi is running.
      # Enable Avahi on the host and cups-browsed comes along for free.
      #
      # WiFi printers (e.g. the Brother) are discovered automatically via cups-browsed
      # and work without any driver package — they support driverless IPP (IPP Everywhere/
      # AirPrint) which lets CUPS render and send jobs without a PPD. A driver package
      # exists (pkgs.cups-brother-hll2350dw) but is not needed.
      # that is why there is no driver configuration for the client section at the moment.

      environment.systemPackages = with pkgs; [
        glabels-qt  # label printing software
      ];

      # CUPS runs as its own user (lp/cups). This group membership allows the
      # primary user to query printer status and manage jobs directly via CUPS.
      users.users.${hostSpecific.primeUsername} = {
        extraGroups = [ "lp" ];
      };
    })

    # Server: shares a locally-attached USB printer over the network.
    # CUPS accepts IPP print jobs from LAN clients, applies the driver, and sends
    # the rendered output to the USB device.
    # defaultShared marks the printer as shared (accessible to network clients via IPP).
    # browsing advertises it via DNS-SD (Avahi) so clients auto-discover it —
    # without advertising, clients would need to manually point at this host's address.
    #
    # The following configuration amounts to a printer connected to the host that enabled this
    # option being advertised over the network and accessible by hosts on the same private
    # network to give it print jobs.
    (lib.mkIf config.custom.services.printing.server.enable {

      services.printing = {
        # with the config below the CUPS GUI will be available at http://<server ip>:<cupsPort>
        enable          = true;
        #
        # Drivers
        # -------
        #
        # Required for the USB connected printer.
        # Installs the printer (Dymo) PostScript Printer Description (PPD) file that describes
        # the printer's capabilities and the filters that tell CUPS how to convert a generic print job
        # into raw commands the printer hardware understands. This just makes the files available —
        # it DOES NOT CONFIGURE ANY PRINTER QUEUE (does not register the printer with CUPS).
        #
        # Note that as part of the registration (hardware.printers.ensurePrinters), we specify a model,
        # in this case "lw450.ppd". This file comes from pkgs.cups-dymo. Without this file,
        # ensurePrinters would fail because CUPS can't find the specified model.
        drivers         = [ pkgs.cups-dymo ];
        #
        # Networking configurations
        # -------------------------
        # This part is what allows traffic to reach CUPS over the private network.
        #
        # The interface to which CUPS will bind and accept traffic on.
        listenAddresses = [ "${anyIPv4}:${toString cupsPort}" ];
        # CUPS rejects IPP requests from IPs outside this list, regardless of firewall, and of
        # the interface it exposes its port on.
        allowFrom       = [ localHost subnet ];
        # Opens CUPS port in the host firewall, so other hosts on the private network can reach it.
        openFirewall    = true;
        #
        #
        # Printer availability
        # --------------------
        # A printer connected via USB can be viewed in 3 ways (assuming network is set):
        # - 1. Not shared: local only (available only to the machine it is connected to), and even if
        #   a remote machine sets its network path and firewall and listenAddresses allows the traffic,
        #   CUPS will reject the job.
        # - 2. Shared, not advertised: remote clients can access the printer but they need to know about
        #   it and configure its path — no "discovery over mDNS".
        # - 3. Shared + advertised: remote clients can access the printer and get its path for "free" via
        #   auto-discovery (mDNS via Avahi).
        #
        # The following two options when both are enabled create "3. Shared + advertised"
        #
        # Sets `DefaultShared Yes` in cupsd.conf. Marks every printer added to CUPS
        # as shared by default, and by itself makes any printer added to CUPS act as
        # "2. Shared".
        # Note that printers declared via hardware.printers.ensurePrinters have the
        # option to define themselves as shared using ppdOptions.printer-is-shared = "true"
        # explicitly, which overlaps with this option.
        defaultShared   = true;
        # Sets `Browsing Yes` in cupsd.conf. Advertises shared printers on the LAN
        # via DNS-SD (Avahi). This option complements defaultShared and moves the config to
        # "3. Shared + advertised".
        browsing        = true;

        # Printer discovery (disabled)
        # ----------------------------
        # services.printing.browsed.enable defaults to services.avahi.enable.
        # Since Avahi is running on the NAS (publisher mode), cups-browsed would
        # start automatically and discover network printers like the Brother WiFi
        # printer. The NAS role is to share the USB printer, not to consume
        # network printers, so we explicitly disable it. This is a separate daemon,
        # not part of CUPS itself, whose sole job is discovering remote printers on the network.
        # It listens to Avahi for _ipp._tcp DNS-SD announcements and when it sees one, automatically
        # adds that printer as a local CUPS queue.
        browsed.enable  = false;
      };

      # This translates to running lpadmin (on every system activation or on system boot),
      # which is the CUPS command-line tool for managing printer queues. It's the standard
      # way to add, modify, or remove printers from CUPS without using the web UI.
      #
      # Note that this only adds the USB printer — it does not provide drivers for it,
      # nor does it have anything to do with making it available on the network.
      hardware.printers = {
        ensurePrinters = [
          {
            # free-form label.
            name      = "Dymo";
            # Also a label.
            location  = hostSpecific.hostName;
            # this was obtained by running:
            # lpinfo -v
            # on the machine to which the printer is connected.
            deviceUri = "usb://DYMO/LabelWriter%20450?serial=07060209344895";
            # this was obtained by running:
            # lpinfo -m
            # which lists all PPD files available on the system (installed by drivers = [ pkgs.cups-dymo ] above)
            # and picking the one matching the LabelWriter 450 model.
            model     = "lw450.ppd";
            # Explicitly marks this specific printer as shared via lpadmin -o.
            # This overlaps with defaultShared = true which sets the default for all printers. Having both
            # is redundant but makes the intent explicit.
            ppdOptions.printer-is-shared = "true";
          }
        ];
      };
    })
  ];
}
