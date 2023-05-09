{ pkgs, config, ... }:
{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.defaultUserShell = pkgs.zsh;
  users.users.${config.userDefinedGlobalVariables.username} = {
    isNormalUser = true;
    initialPassword = config.userDefinedGlobalVariables.initialPassword;
    description = config.userDefinedGlobalVariables.username;
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [];
  };
}
