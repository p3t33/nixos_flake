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
        plugin = resurrect;
        extraConfig = "set -g @resurrect-strategy-nvim 'session'"; 
    }
    continuum
    nord
  ];

  extraConfig = "
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

    bind -n M-h select-pane -L
    bind -n M-l select-pane -R
    bind -n M-k select-pane -U
    bind -n M-j select-pane -D
   ";
  };

}
