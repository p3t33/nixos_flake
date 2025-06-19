{ config, lib, pkgs, ... }:
let
  cfg = config.customOptions.enableModule.hellowordPython;
  helloword-python =
    pkgs.writers.writePython3Bin "helloword-python" { libraries = [ pkgs.python3Packages.PyGithub ]; }
      ''
        # import os

        if __name__ == '__main__':
            print('Hello, world!')
      '';
in
{
  options.customOptions.enableModule.hellowordPython = lib.mkEnableOption "Install helloword-python script";
  config = lib.mkIf cfg {
    home.packages = [ helloword-python ];
  };
}
