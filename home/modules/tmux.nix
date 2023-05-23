{ pkgs, ... }:
{

programs.tmux = {
  enable = true;
  secureSocket = false;
  terminal = "screen-256color";
  disableConfirmationPrompt = true;
  prefix = "C-a";
  keyMode = "vi";
  baseIndex = 1;
  clock24 = true;
  sensibleOnTop = true;

  plugins = with pkgs.tmuxPlugins; [
    {
        plugin = tmux-fzf;
        extraConfig = ''
        TMUX_FZF_LAUNCH_KEY="C-f"
        '';
    }

    {
        plugin = resurrect;
        extraConfig = "set -g @resurrect-strategy-nvim 'session'";
    }

    # vim-tmux-navigator plugin has a dual function(although name implies only
    # vim integration )
    #
    # This plugin adds smart movments between tmux panes and vim windows. By using
    # Ctrl+h/j/k/l you will be able to move across tmux pane into pane with
    # vim inside it and then move inside the vim windows and back seamlessly
    # For this integration to work counterpart plugin needs to be added to vim.
    #
    # If the conterpart isn't installed the only functionality that will be added
    # is the aability to move between panes using Ctr+h/j/k/l in tmux.
    {
        plugin = vim-tmux-navigator;
    }

    {
        plugin = yank;
        extraConfig = ''
          bind-key -T copy-mode-vi v send-keys -X begin-selection
          bind-key -T copy-mode-vi C-v send-keys -X rectangle-toggle
          bind-key -T copy-mode-vi y send-keys -X copy-selection-and-cance
        '';
    }
    continuum
    nord
  ];

  extraConfig = ''
    # kill a session
    bind-key X kill-session
    # sets the length of session name
    set -g status-left-length 30

    # don't detach from tmux once last window of session is closed and instead
    # atttach to another existing session if one exist.
    set-option -g detach-on-destroy off

    # don't rename windows automatically
    set-option -g allow-rename off

    # Ensure window index numbers get reordered on delete
    set-option -g renumber-windows on

    # Set easier window split kyes
    bind-key v split-window -h
    bind-key h split-window -v

    # Enable mouse mode(tmux 2.1++)
    setw -g mouse on

    set-option -g status-position top

    bind-key -r f run-shell 'tmux neww tmux-sessionizer'
    bind-key -r i run-shell 'tmux neww cheat-sh'
    # S easier move of windows
    bind-key -r < swap-window -t -
    bind-key -r > swap-window -t +

    # Plugins
    # -------

    # Tmux resurrect

    #set -g @resurrect-strategy-nvim 'session'
    #set -g @continuum-boot 'on'

    # Tmux continuum settings
    set -g @continuum-restore 'on'

    #set -g @continuum-save-interval '1'

    # Windows
    set -g set-titles 'on'

    set -g set-titles-string '#{pane_title}'

    bind -n S-Left previous-window
    bind -n S-Right next-window

    # reload config
    bind-key r source-file ~/.config/tmux/tmux.conf \; display-message "~/.tmux.conf reloaded."

   '';
  };

}
