{ lib, pkgs, ... }:
let
  helloword-python = pkgs.writers.writePython3Bin "helloword-python" { libraries = [ pkgs.python3Packages.PyGithub ]; } ''
     # import os

     if __name__ == '__main__':
         print('Hello, world!')
  '';
in
{
    home.packages = [ helloword-python ];
}
