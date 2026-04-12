{ config, lib, pkgs, ... }:

{
  config = lib.mkIf config.programs.nix-ld.enable {
    # nix-ld provides a stub dynamic linker at the path that most Linux binaries
    # expect (/lib64/ld-linux-x86-64.so.2). Without it, running unpatched
    # pre-compiled binaries (e.g. vendor toolchains, downloaded CLIs) fails with
    # "No such file or directory" even though the ELF file exists. nix-ld
    # intercepts the loader call and delegates to the real NixOS linker.
    programs.nix-ld  = {
      package = pkgs.nix-ld;

      # right now default list of libraries is set, this option
      # allows to add more.
      # programs.nix-ld.libraries
    };
  };
}
