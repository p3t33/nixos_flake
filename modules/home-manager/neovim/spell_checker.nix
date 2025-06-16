{ config, ... }:
let
  neovimCustomDictionaryPathrelativeToHomeDirectory = ".config/nvim/spell/nixen.utf-8";
in
{
  config.programs.neovim.extraLuaConfig = ''
    -- ----------------------------------------------------------
    -- spelling and costum dictrionary use
    -- ----------------------------------------------------------
    -- Define the custom command for nixen spell file compilation
    vim.api.nvim_create_user_command('MakeNixSpell', function()
            vim.cmd('mkspell! ~/${neovimCustomDictionaryPathrelativeToHomeDirectory}.spl ~/${neovimCustomDictionaryPathrelativeToHomeDirectory}.add')
            end, {})

    local function compile_nix_spell_file_if_needed()
        local spellfile = vim.fn.expand("~/${neovimCustomDictionaryPathrelativeToHomeDirectory}.spl")
        local addfile = vim.fn.expand("~/${neovimCustomDictionaryPathrelativeToHomeDirectory}.add")

        if vim.fn.filereadable(addfile) == 0 then
        return
        end

        if vim.fn.filereadable(spellfile) == 0 then
        vim.notify("Generating spell file...", vim.log.levels.INFO)
        vim.cmd('silent! MakeNixSpell')
    vim.loop.sleep(3000)
        return
        end

        local spell_timestamp = vim.fn.system('stat -c %Y ' .. vim.fn.shellescape(spellfile))
        local add_timestamp = vim.fn.system('stat -c %Y ' .. vim.fn.shellescape(addfile))

        if not vim.fn.filereadable(spellfile) or add_timestamp > spell_timestamp then
        vim.cmd('silent! MakeNixSpell')
        end
        end

        -- Autocommand to call the function at Vim start
        vim.api.nvim_create_autocmd("VimEnter", {
                pattern = "*",
                callback = compile_nix_spell_file_if_needed,
                })

    -- Spell checker key mapping and autocommands
    vim.api.nvim_set_keymap('n', '<F5>', ":setlocal spell! spellsuggest=best,5 spelllang=en_us,nixen<CR>", { noremap = true, silent = true })

    vim.api.nvim_create_autocmd("FileType", {
            pattern = "gitcommit,markdown",
            command = "setlocal spell spellsuggest=best,5 spelllang=en_us,nixen",
            })
    -- ------------------------------------
        -- Jump to misspelled words
        -- ------------------------
        vim.keymap.set('n', ']s', ']sz=', { noremap = true, silent = true, desc = "Next misspelled word & suggest" })
        vim.keymap.set('n', '[s', '[sz=', { noremap = true, silent = true, desc = "Previous misspelled word & suggest" })
        -- ------------------------
  '';

  config = {
    home.file = {
      # Use the nested config.global.neovimCustomDictionaryPathrelativeToHomeDirectory directly
      "${neovimCustomDictionaryPathrelativeToHomeDirectory}.add".text =
        ''
          tmux
          Tmux
          Kobi
          alacritty
          Adv360
          ZMK
          NixOS
          ToDo
          rofi
          systemd
          nixos
          polybar
          i3
          i3wm
          xorg
          zsh
          VirtualBox
          KVM
          rsync
          gpg
          autosave
          lua
          syncthing
          autostart
          sxhkd
          neovim
        '';
    };
  };
}
