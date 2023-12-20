{ pkgs, config, ... }:
{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  # Since 23.05 there will be a failed assertion if zsh is not enabled here.
  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;
  users.users.${config.userDefinedGlobalVariables.username} = {
    isNormalUser = true;
    initialPassword = config.userDefinedGlobalVariables.initialPassword;
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
}
