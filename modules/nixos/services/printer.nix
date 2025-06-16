{ hostSpecific, pkgs, ... }:
{
  # Note for cups service(printing = enable) to automatically discover
  # IPP printers avahi daemon service is required.

  environment.systemPackages = with pkgs; [
    # software for printing labels.
    glabels-qt
  ];
  # Enable CUPS to print documents
  services.printing.enable = true;
  # My dymo printer required a driver, my brother printer did not although there
  # is a package for it(pkgs.cups-brother-hll2350dw).
  services.printing.drivers = [ pkgs.cups-dymo ];

  # CUPS, the print server, runs as its own user (usually lp or cups)
  # For you to interact directly with CUPS – for example, to query
  # printer status, manage print jobs, or even to send a job – you
  # need to be recognized by CUPS as someone who has permission to do so.
  # This recognition often depends on being a member of the lp group,
  # as you've found. I was having issues without this setting.
  users.users.${hostSpecific.primeUsername} = {
    extraGroups = [ "lp" ];
  };

}
