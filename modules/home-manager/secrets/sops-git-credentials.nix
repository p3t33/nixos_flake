{ config, userDefinedGlobalVariables, ... }:
{
  sops = {
    age.keyFile = userDefinedGlobalVariables.sopsKeyPath;
    secrets.git = { };
  };
}
