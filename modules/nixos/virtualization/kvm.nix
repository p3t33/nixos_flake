{ pkgs, config, ... }:
{
  virtualisation = {
    libvirtd = {
      enable = true;

      qemu = {
        # Use full QEMU to emulate other architectures (e.g., aarch64 on x86)
        package = pkgs.qemu;
        runAsRoot = true; # required for PCI passthrough and raw device access

        # Required for shared folders with virtiofs
        vhostUserPackages = [ pkgs.virtiofsd ];

        # Enable UEFI firmware support (Full = with Secure Boot and extra features)
        ovmf = {
          enable = true;
          packages = [ pkgs.OVMFFull.fd ];  # ← Full UEFI support, including secure boot.
        };

        # Trusted Platform Module: lets VM emulate a hardware TPM (needed by Windows 11 and Secure Boot)
        swtpm = {
          enable = true;
          package = pkgs.swtpm;
        };
      };
    };

    spiceUSBRedirection.enable = true;
  };


  users.users.${config.userDefinedGlobalVariables.primeUsername} = {
    extraGroups = [ "libvirtd" ];
  };

  environment.systemPackages = with pkgs; [
    virt-manager
    qemu_kvm
    qemu
    libvirt
    OVMF
  ];

  # On first boot after fresh install of NixOS there is no defulat netowrk set to be used by libvirt
  #
  # sudo virsh net-list --all
  #  Name      State      Autostart   Persistent
  #  ----------------------------------------------
  # default   inactive   no          yes
  #
  # which then requries to execute manually
  # sudo virsh net-start default
  # sudo virsh net-autostart default
  #
  # This service executes a oneshot script that will parse the "sudo virsh net-list --all" and
  # enable(and make it auto start) the default network.
  #
  # This script assumes that /var/lib/libvirt/qemu/networks/default.xml exists.
  # This script winll not work inside of qemu virtual machine as hosts default will
  # set the guest VM to the same default.xml values(VM will get an error:
  # "error: internal error: Network is already in use by interface <>".
  systemd.services.libvirt-enable-default-network = {
    description = "Ensure libvirt default network is started and set to autostart";
    after = [ "libvirtd.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = pkgs.writeShellScript "libvirt-net-default-activate" ''
        set -euo pipefail
        export LIBVIRT_DEFAULT_URI="qemu:///system"

        echo "Checking current libvirt network state..."
        output="$(${pkgs.libvirt}/bin/virsh net-list --all)"
        echo "Output:"
        echo "$output"
        echo "---- end of virsh net-list --all ----"

        state_line="$(echo "$output" | grep '^\s*default\s')"
        echo "Parsed default network line: $state_line"

        if [ -z "$state_line" ]; then
          echo "Default network is not defined. Skipping."
          exit 0
        fi

        state=$(echo "$state_line" | ${pkgs.gawk}/bin/awk '{print $2}')
        autostart=$(echo "$state_line" | ${pkgs.gawk}/bin/awk '{print $3}')

        if [ "$state" != "active" ]; then
          echo "Starting default network..."
          ${pkgs.libvirt}/bin/virsh net-start default
        else
          echo "Default network already active."
        fi

        if [ "$autostart" != "yes" ]; then
          echo "Enabling autostart for default network..."
          ${pkgs.libvirt}/bin/virsh net-autostart default
        else
          echo "Autostart already enabled."
        fi

        echo "Finished setting up default libvirt network."
      '';
    };
  };

}
