{ config, pkgs, lib, ... }:
let
  cfg = config.programs.neovim;
in
{
  config = lib.mkIf cfg.enable {
  programs.firefox = {
    policies = {
      ExtensionSettings = {
        "firenvim@lacamb.re" = {
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/firenvim/latest.xpi";
          installation_mode = "normal_installed";
        };
      };
    };
  };

  # As part of installing firenvim plugin using a regular plugin manger there is an installtion
  # command that needs to be executed once.
  #
  # This command can also be executed from the command line and according to the plugin documentation
  # it is:
  # $ nvim --headless "+call firenvim#install(0) | q"
  #
  # This command creates manifest(a JSON file located at ~/.mozilla/native-messaging-hosts).
  # So the file I am creating here is kind of a workaround.
  home.file = {
    ".mozilla/native-messaging-hosts/firenvim.json".text = ''
      { "name": "firenvim", "description": "Turn your browser into a Neovim GUI.", "path": "${config.home.homeDirectory}/.local/share/firenvim/firenvim", "type": "stdio", "allowed_extensions": ["firenvim@lacamb.re"]}
    '';
  };

  programs.neovim = {

    plugins = with pkgs.vimPlugins; [
      {
        plugin = firenvim;
        type = "lua";
        # Makes sure that neovim is only used when it is explicitly called with Ctrl-e
        # which is very usefull as neovim mostly sutable for long text editing
        # and "breaks the flow" for "short editing" such as google search.
        #
        # Made sure to disable status bar when neovim is used inside Firefox.
        config = ''
          if vim.g.started_by_firenvim then
              vim.g.firenvim_config = {
                  globalSettings = {
                      alt = "all",
                  },
                  localSettings = {
                      [".*"] = {
                          cmdline = "neovim",
                          priority = 0,
                          selector = "textarea",
                          takeover = "never",
                      },
                  },
              }

              local function setup_firenvim()
                  vim.opt.filetype = "markdown"
                  vim.opt.ruler = false
                  vim.opt.showcmd = false
                  vim.opt.laststatus = 0
                  vim.opt.showtabline = 0
              end

              vim.api.nvim_create_augroup("firenvim", {})
              vim.api.nvim_create_autocmd("FileType", {
                      group = "firenvim",
                      pattern = "text",
                      callback = setup_firenvim,
              })
          end
        '';
      }

    ];
  };
  };
}
