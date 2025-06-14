{ pkgs, config, ... }:
{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  # Since 23.05 there will be a failed assertion if zsh is not enabled here.
  programs.zsh.enable = true;

  users.defaultUserShell = pkgs.zsh;
  users.users.${config.hostSpecification.primeUsername} = {
    isNormalUser = true;
    # The hash, as a string or as a file need to be sutiable for the chpasswd -e command
    # which means that at least at the moment argon2 will not work for now.
    # to create the hash and keep it out of the history
    #
    # read -s password && echo "$password" | mkpasswd -s
    #
    # By default Yescrypt is used for hasing.
    hashedPasswordFile = config.sops.secrets.initial_hashed_password.path;
    description = config.hostSpecification.primeUsername;
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
    packages = with pkgs; [ ];
  };

  users.groups.${config.hostSpecification.primeUsername} = {
    members = [ config.hostSpecification.primeUsername ];
  };

  users.groups.${config.userDefinedGlobalVariables.mediaGroup} = {
    members = [ config.hostSpecification.primeUsername ];
  };

  users.groups.${config.userDefinedGlobalVariables.dataGroup} = {
    members = [ config.hostSpecification.primeUsername ];
  };
}
