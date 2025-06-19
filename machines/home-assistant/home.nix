{
  imports = [
    ./sops-home.nix
    ../../modules/home-manager # imported via default.nix
  ];

  customOptions = {
    enableHomeManagerProfile = {
      core = true;
    };
  };

}
