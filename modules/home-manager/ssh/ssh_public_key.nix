{ config, ... }:
{

  home.file = {
    ".ssh/smartcard.pub".text = ''
      ${config.userDefinedGlobalVariables.sshPublicKey}
    '';
  };
}
