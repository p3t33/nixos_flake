{ hostSpecification, ... }:
{

  home.file = {
    ".ssh/smartcard.pub".text = ''
      ${hostSpecification.sshPublicKey}
    '';
  };
}
