# modules/mtp.nix
{ config, lib, pkgs, ... }:
let
  cfg = config.custom.services.gvfs;
in
{
  # Enable GVFS (GNOME Virtual File System) to provide desktop-level access
  # to special storage protocols like MTP, SFTP, SMB, and WebDAV.
  # Required for automatic detection of devices such as modern Kindles in
  # file managers and applications like Calibre.
    options.custom.services.gvfs.enable = lib.mkEnableOption "GVFS + optional MTP tools";

    config = lib.mkIf cfg.enable {
      services.gvfs.enable = true;
      environment.systemPackages = with pkgs; [ libmtp jmtpfs ];
    };
}

