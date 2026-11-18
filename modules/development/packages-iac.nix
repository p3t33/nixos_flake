{ ... }:
{
  flake.modules.nixos.packages-iac = { config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [

    # infrastructure as code
    packer
    vagrant
    ansible
  ];
};
}
