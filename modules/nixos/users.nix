{ pkgs, config, ... }:
{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  # Since 23.05 there will be a failed assertion if zsh is not enabled here.
  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;
  users.users.${config.userDefinedGlobalVariables.username} = {
    isNormalUser = true;
    # The hash, as a string or as a file need to be sutiable for suitable for the chpasswd -e command
    # which means that at least at the moment argon2 will not work for now.
    initialHashedPassword = config.userDefinedGlobalVariables.initialHashedPassword;
    description = config.userDefinedGlobalVariables.username;
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [];
  };

  users.groups.${config.userDefinedGlobalVariables.username} = {
    members = [ config.userDefinedGlobalVariables.username ];
  };

  users.groups.media = {
    members = [ config.userDefinedGlobalVariables.username ];
  };

  users.groups.data = {
    members = [ config.userDefinedGlobalVariables.username ];
  };
}
