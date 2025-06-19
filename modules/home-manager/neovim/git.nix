{ config, pkgs, lib, ... }:
let
  cfg = config.programs.neovim;
in
{
  config = lib.mkIf cfg.enable {
  programs.neovim = {

    extraPackages = with pkgs; [
      # Dependencies
      # ----------------
      git
    ];

    plugins = with pkgs.vimPlugins; [
    {
      plugin = gitsigns-nvim;
      type = "lua";
      config = ''
        require('gitsigns').setup {
            on_attach = function(bufnr)
            local gs = package.loaded.gitsigns
                vim.keymap.set('n', "<leader>gd", gs.reset_hunk)
                vim.keymap.set('v', "<leader>gd", function() gs.reset_hunk {vim.fn.line('.'), vim.fn.line('v')} end)
                vim.keymap.set('n', "<leader>gb", gs.toggle_current_line_blame)
                vim.keymap.set('n', "]g", function()
                        if vim.wo.diff then return ']c' end
                        vim.schedule(function() gs.next_hunk() end)
                        return '<Ignore>'
                        end, {expr=true})
                vim.keymap.set('n', "[g", function()
                        if vim.wo.diff then return '[c' end
                        vim.schedule(function() gs.prev_hunk() end)
                        return '<Ignore>'
                        end, {expr=true})
        end
        }
      '';
    }
    ];
  };
  };
}

