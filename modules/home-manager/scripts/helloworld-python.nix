{ config, lib, pkgs, ... }:
let
  cfg = config.custom.scripts.hellowordPython;
  helloword-python =
    pkgs.writers.writePython3Bin "helloword-python" { libraries = [ pkgs.python3Packages.PyGithub ]; }
      ''
        # import os

        if __name__ == '__main__':
            print('Hello, world!')
      '';
in
{
  options.custom.scripts.hellowordPython.enable = lib.mkEnableOption "Install helloword-python script";
  config = lib.mkIf cfg.enable {
    home.packages = [ helloword-python ];
  };
}
