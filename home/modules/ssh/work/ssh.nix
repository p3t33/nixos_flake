{ ... }:
{

  programs.ssh = {
    enable = true;
    extraConfig = "
      Host 1-dev-wgv
          HostName 10.81.91.14
	  User kmedrish
          IdentityFile ~/.ssh/id_rsa

      Host 2-dev-wgv
          HostName 10.81.91.16
	  User kmedrish
	  IdentityFile ~/.ssh/id_rsa

      Host 3-dev-wgv
          HostName 10.81.91.18
          User kmedrish
	  IdentityFile ~/.ssh/id_rsa

      Host 4-dev-wgv
          HostName 10.81.91.36
          User kmedrish
	  IdentityFile ~/.ssh/id_rsa

      Host 5-dev-wgv
          HostName 10.81.91.44
          User kmedrish
	  IdentityFile ~/.ssh/id_rsa
	
      Host usb_base_bwc
          HostName 192.168.99.2
          User root
	  IdentityFile ~/.ssh/devices_rsa_no_pass

      Host hotspot_bwc
          HostName 192.168.98.1
          User root
	  IdentityFile ~/.ssh/devices_rsa_no_pass
       ";
  };
}
