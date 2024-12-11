{ config, ...}:
{
      sops = {
          age.keyFile = config.userDefinedGlobalVariables.sopsKeyPath;
          secrets.git = {};
      };
}
