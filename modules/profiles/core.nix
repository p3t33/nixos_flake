{ ... }:
{
  flake.modules.nixos.profile-system = { config, lib, ... }:
    let g = config.custom.profiles.system; in
    {
      config = lib.mkIf g.core.enable {
        custom.apps.development.enable  = true;
        custom.apps.cliUtilities.enable = true;
        custom.apps.encryption.enable   = true;
        custom.networking.disableLegacyBridgeNetfilter.enable = true;
        programs.nix-ld.enable = true;
      };
    };

  flake.modules.nixos.profile-system-services = { config, lib, ... }:
    let g = config.custom.profiles.systemServices; in
    {
      config = lib.mkIf g.core.enable {
        custom.services.tmuxd.enable = true;
        custom.services.mutagen.enable = true;
        custom.services.watchman.enable = true;
      };
    };

  flake.modules.homeManager.profile-home-manager = { config, lib, ... }:
    let g = config.custom.profiles.homeManager; in
    {
      config = lib.mkIf g.core.enable {
        programs.starship.enable = true;
        programs.tmux.enable = true;
        programs.zellij.enable = true;
        programs.fzf.enable = true;
        programs.neovim.enable = true;
        programs.git.enable = true;
        programs.delta.enable = true;
        programs.zoxide.enable = true;
        programs.direnv.enable = true;
        programs.zsh.enable = true;
        programs.bash.enable = true;
        programs.bat.enable = true;
        programs.lazygit.enable = true;
        programs.atuin.enable = true;
        programs.yazi.enable = true;
        programs.navi.enable = true;
        programs.tealdeer.enable = true;
        programs.pay-respects.enable = true;
        programs.claude-code.enable = true;
        programs.codex.enable = true;
        programs.mcp.enable = true;
        custom.lnav.enable = true;
        custom.scripts.cheatSh.enable = true;
        custom.programs.pi.enable = true;
      };
    };
}
