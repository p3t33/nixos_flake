{ lib, pkgs, ... }:
let
  helloworld-bash = pkgs.writeShellScriptBin "helloworld-bash" ''
    echo Hello World
  '';
in
{
    home.packages = [ helloworld-bash ];
}
