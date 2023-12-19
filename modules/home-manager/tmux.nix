{ pkgs, ... }:
let
  tmux-nerd-font-window-name = pkgs.tmuxPlugins.mkTmuxPlugin {
    pluginName = "tmux-nerd-font-window-name.tmux";
    version = "unstable-2023-08-22";
    rtpFilePath = "tmux-nerd-font-window-name.tmux";
    src = pkgs.fetchFromGitHub {
      owner = "joshmedeski";
      repo = "tmux-nerd-font-window-name";
      rev = "c2e62d394a290a32e1394a694581791b0e344f9a";
      sha256 = "stkhp95iLNxPy74Lo2SNes5j1AA4q/rgl+eLiZS81uA=";
    };
  };
in
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

  # Important notice about tmux plugins and the tmux.conf that generated from
  # this tmux.nix
  #
  # in general plugins may and will conflict with each other, so the order in
  # which tmux loads them matters.  This is true when editing directly tmux.conf
  # and same applies when putting them in the plugins list in NixOS.
  #
  # Specifically in my current list. The theme plugin(nord) must be included before
  # continuum and same is true for everything that might set tmux status-right
  # as the auto save command(set -g @continuum-save-interval '1')
  # gets written into status-right. So everything that sets status-right would have to be loaded
  # beforehand.
  # Furthermore the continuum plugin must be loaded after the resurrect plugin.
  plugins = with pkgs.tmuxPlugins; [
    # The nord plugin or any other theme should on the top of the list
    # of the plugins. As it writes into the status-right which breaks the
    # set -g @continuum-save-interval for the continuum plugin.
    nord

    # This plugin needs to be loaded before continuum or else continuum, will
    # not work.
    {
        plugin = resurrect;
        extraConfig = ''

          # I have tested this strategy to work with neovim but it is not enough to have
          # Session.vim at the root of the path from which the plugin is going to do the restore
          # it is important that for neovim to be saved to be restored from the path where Session.vim
          # exist for this flow to kick in. Which means that even if tmux-resurrect saved the path with
          # Session.vim in it but vim was not open at the time of the save of the sessions then when
          # tmux-resurrect restore the window with the path with Session.vim nothing will happen.

          # Furthermore I currently using vim-startify which among other things is able to restore
          # from Session.vim if neovim is opened from the path where Session.vim exist. So in a
          # sense I don't really need tmux resurrect to restore the session as this already
          # taken care of and this functionality becomes redundant. But as I am not sure if I keep
          # using vim-startify or its auto restore feature and it do not conflict in any way that
          # I know of with set -g @resurrect-strategy-* I decided to keep it enabled for the time being.
          set -g @resurrect-strategy-nvim 'session'
          set -g @resurrect-strategy-vim 'session'

          set -g @resurrect-capture-pane-contents 'on'

          # This three lines are specific to NixOS and they are intended
          # to edit the tmux_resurrect_* files that are created when tmux
          # session is saved using the tmux-resurrect plugin. Without going
          # into too much details the strings that are saved for some applications
          # such as nvim, vim, man... when using NixOS, appimage, asdf-vm into the
          # tmux_resurrect_* files can't be parsed and restored. This addition
          # makes sure to fix the tmux_resurrect_* files so they can be parsed by
          # the tmux-resurrect plugin and successfully restored.
          resurrect_dir="$HOME/.tmux/resurrect"
          set -g @resurrect-dir $resurrect_dir
          set -g @resurrect-hook-post-save-all 'target=$(readlink -f $resurrect_dir/last); sed "s| --cmd .*-vim-pack-dir||g; s|/etc/profiles/per-user/$USER/bin/||g" $target | sponge $target'
        '';
    }

    # vim-tmux-navigator plugin has a dual function(although name implies only
    # vim integration )
    #
    # This plugin adds smart movements between tmux panes and vim windows. By using
    # Ctrl+h/j/k/l you will be able to move across tmux pane into pane with
    # vim inside it and then move inside the vim windows and back seamlessly
    # For this integration to work counterpart plugin needs to be added to vim.
    #
    # If the counterpart isn't installed the only functionality that will be added
    # is the ability to move between panes using Ctr+h/j/k/l in tmux.
    {
        plugin = vim-tmux-navigator;
    }
    # The copy buffer for tmux is separate from the system one, in the past in
    # order to sync the two there was a need to install tmux-yank but it looks like
    # Tmux now sends the OSC52 escape code that tells the terminal(one that support this)
    # to not display the following characters, but to copy them into the clipboard
    # instead.
    #
    # The reason that this plugin is still included because it provides a quick way to copy what
    # what is on the command line and once in copy mode to copy the PWD. I might just replace the
    # plugin with keybindings(based on send-keys).
    {
        plugin = yank;
    }
    # For some reason this plugin by default only copy into the Tmux copy buffer
    # and so I had to explicitly state the command to make it copy into the system
    # clipboard as swell.
    {
        plugin = tmux-thumbs;
        extraConfig = ''
          set -g @thumbs-command 'tmux set-buffer -- {} && tmux display-message "Copied {}" && printf %s {} | xclip -i -selection clipboard'
          set -g @thumbs-key C-y
        '';
    }
    {
        plugin = extrakto;
        extraConfig = ''
          set -g @extrakto_key M-y
          set -g @extrakto_split_direction v
        '';
    }
    # a few words about @continuum-boot and @continuum-systemd-start-cmd that
    # are not used as part of the extraConfig for the continuum plugin.
    #
    # @continuum-boot - when set will generate a user level systemd unit file
    # which it will save to ${HOME}/.config/systemd/user/tmux.service and enable
    # it.
    #
    # @continuum-systemd-start-cmd - The command used to start the tmux server
    # is determined via this configuration, and this command is set in the
    # context of the systemd unit file that is generated by setting @continuum-boot
    # when this option is not set the default will be "tmux new-session -d"
    # This setting provides a more fine grain option over the creation of the
    # systemd unit.
    #
    # Having said all that, it is important to understand that systemd units
    # are defined as .nix settings and then created when NixOS is built and
    # nothing is generated "willy-nilly" by applications.
    # So this aspect of the plugin is already taken care of by me in a separate
    # systemd unit that is responsible to start tmux when system starts.
    #
    # set -g @continuum-save-interval is written into the status-right which
    # means that any other plugin that writes into status-right needs to be
    # loaded first or the autosave functionality will not work.
    #
    # More then that it looks like the autosave feature(set -g @continuum-save-interval)
    # only works if you are attached to tmux, for some reason it does not work
    # in detached mode. Maybe if no one is attached then there is nothing
    # changing and so nothing to save.
    #
    # If autosave option interval is not set there is a default of 15 minutes
    # and it worked for me when tested.
    {
       plugin = continuum;
       extraConfig = ''
         set -g @continuum-restore 'on'
         set -g @continuum-save-interval '10'
       '';
    }
    {
        plugin = tmux-nerd-font-window-name;
        extraConfig = ''
            set -g @plugin 'joshmedeski/tmux-nerd-font-window-name'
        '';
    }


  ];

  extraConfig = ''
    # kill a session
    bind-key X kill-session
    # sets the length of session name
    set -g status-left-length 30

    # don't detach from tmux once last window of session is closed and instead
    # attach to another existing session if one exist.
    set-option -g detach-on-destroy off

    # don't rename windows automatically
    set-option -g allow-rename off

    # Ensure window index numbers get reordered on delete
    set-option -g renumber-windows on

    # Set easier window split keys
    bind-key v split-window -h
    bind-key h split-window -v

    # Enable mouse mode(tmux 2.1++)
    setw -g mouse on

    set-option -g status-position top

    bind-key -r s run-shell 'tmux popup -E -w 80% -h 80% "bash tmux-sessionizer"'
    bind-key g new-window 'lazygit; tmux kill-pane'

    bind-key -r i run-shell 'tmux neww cheat-sh'
    # Easier move of windows
    bind-key -r Home swap-window -t - \; select-window -t -
    bind-key -r End swap-window -t + \; select-window -t +


    # Orders the session list by time of last access
    # The default being to order by the index that is assigned when
    # session was created.
    bind S choose-tree -sZ -O time

    # switch to last session
    bind-key L switch-client -l

    # Pane to window
    unbind !
    bind-key w break-pane

    # Windows
    set -g set-titles 'on'

    set -g set-titles-string '#{pane_title}'

    bind -n S-Left previous-window
    bind -n S-Right next-window

    # A more consistent(with i3wm) and ergonomic(qwerty) to focus on a pane
    unbind z
    bind-key f resize-pane -Z

    # quick yank of the text in the current line without going into selection

    # vim idiomatic selection and yanking
    # ==================================
    # This section is done in addition to the yanking plugins I already installed
    # as those plugins are "one shot yank" intended to achieve something
    # very specific while the following keybindings are intended to select and yank
    # text the same way you will do in vim.
    #
    # Comparing to vim, tmux starts in INSERT mode and by executing prefix+[
    # tmux goes into NORMAL mode and the following keybindings mimic
    # selecting text in vim NORMAL mode.

    # Go into selection and once done press Enter or y to yank
    # -----------------
    # Go into selection mode(vim VISUAL mode)
    # to yank selected.
    bind-key -T copy-mode-vi v send-keys -X begin-selection
    #
    # Go into selection mode of line(vim VISUAL LINE mode)
    bind-key -T copy-mode-vi V send-keys -X select-line
    #
    # Go into rectangle selection mode(vim VISUAL BLOCK mode)
    bind-key -T copy-mode-vi C-v run-shell 'tmux send-keys -X rectangle-toggle; tmux send-keys -X begin-selection'

    # Yanking
    # -------
    # This is the action that tapping the Enter key will do by default for tmux
    # The only reason this is here is because this is how yanking is done
    # vim.
    # I created costume keybinding that removes \n when coping a line.
    # not sure if it might break some flow and it requires perl and xclip.
    # so I left the version that adds \n and has no dependencies in comment.
    #bind-key -T copy-mode-vi y send-keys -X copy-selection-and-cancel
    bind-key -T copy-mode-vi y send-keys -X copy-pipe-and-cancel "perl -pe 'chomp if eof' | tmux load-buffer - ; tmux save-buffer - | xclip -i -selection clipboard"
    # ==================================

    # Creating new windows and sessions
    # ==================================
    bind Enter new-window
    bind-key -r o command-prompt -p "Name of new session:" "new-session -s '%%'"
    # ==================================

    # reload config
    bind-key r source-file ~/.config/tmux/tmux.conf \; display-message "~/.tmux.conf reloaded."

   '';
  };

}
