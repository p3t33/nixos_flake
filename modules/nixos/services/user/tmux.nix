{ config, pkgs, lib, ... }:

let
  cfg = config.custom.services.tmuxd;
  tmux = lib.getExe pkgs.tmux;

  # tmux 3.6+ (built with --enable-systemd) spawns each pane into its own
  # tmux-spawn-*.scope cgroup for systemd-oomd isolation. These scopes live
  # outside the tmux service's cgroup. Before 3.6 all pane processes lived
  # inside the service's cgroup, so systemd's KillMode=control-group (the
  # default) would catch any stragglers after kill-server.
  #
  # With scopes, that backstop is gone. Any process that doesn't exit on its
  # own after kill-server becomes an orphan whose scope lingers with a 90s
  # default stop timeout blocking shutdown. The scope cleanup after
  # kill-server catches these stragglers.
  #
  # One known case is neovim: it forks its TUI from its core, the core runs
  # with --embed and no PTY, communicating with the TUI over an RPC pipe.
  # According to neovim's docs the --embed process should exit when the
  # channel closes, but in practice idle instances occasionally fail to
  # detect the closure.
  tmux-graceful-stop = pkgs.writeShellScript "tmux-graceful-stop" ''
    ${tmux} kill-server 2>/dev/null || true
    sleep 1
    ${pkgs.systemd}/bin/systemctl --user kill 'tmux-spawn-*.scope' 2>/dev/null || true
    sleep 1
    ${pkgs.systemd}/bin/systemctl --user kill --signal=SIGKILL 'tmux-spawn-*.scope' 2>/dev/null || true
  '';
in
{
  options.custom.services.tmuxd.enable = lib.mkEnableOption "Enable tmux systemd user service";

  config = lib.mkIf cfg.enable {
    systemd.user.services.tmux = {
      description = "tmux server";

      # creates the [Service] section
      # based on the emacs systemd service
      # does not source uses a login shell so does not load ~/.zshrc in case this is needed
      # just add -l(E.g bash -cl "...").
      serviceConfig = {
        Type = "forking";
        Restart = "on-failure";
        ExecStart = "${lib.getExe' pkgs.bash "bash"} -c 'source ${config.system.build.setEnvironment} ; ${tmux} start-server'";
        ExecStop = tmux-graceful-stop;
        TimeoutStopSec = 10;
      };

      wantedBy = [ "default.target" ];
    };
  };
}
