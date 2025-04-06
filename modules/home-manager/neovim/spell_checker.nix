{ config, ... }:
{
  config.programs.neovim.extraLuaConfig = ''
    -- ----------------------------------------------------------
    -- spelling and costum dictrionary use
    -- ----------------------------------------------------------
    -- Define the custom command for nixen spell file compilation
    vim.api.nvim_create_user_command('MakeNixSpell', function()
            vim.cmd('mkspell! ~/${config.userDefinedGlobalVariables.neovimCustomDictionaryPathrelativeToHomeDirectory}.spl ~/${config.userDefinedGlobalVariables.neovimCustomDictionaryPathrelativeToHomeDirectory}.add')
            end, {})

    local function compile_nix_spell_file_if_needed()
        local spellfile = vim.fn.expand("~/${config.userDefinedGlobalVariables.neovimCustomDictionaryPathrelativeToHomeDirectory}.spl")
        local addfile = vim.fn.expand("~/${config.userDefinedGlobalVariables.neovimCustomDictionaryPathrelativeToHomeDirectory}.add")

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
        -- There is a need in the second function with thd delay or the kebiding won't jumpt to the end
        -- of the word.
        --
        -- Jump to the next misspelled word, activate fix mode, and move to the end of the word after correction
        vim.keymap.set('n', ']s', function()
                vim.api.nvim_feedkeys("]s", "n", false)  -- Jump to the next misspelled word
                vim.api.nvim_feedkeys("z=", "n", false)  -- Trigger spell correction
                vim.defer_fn(function()                  -- Wait for you to select a correction
                    vim.api.nvim_feedkeys("e", "n", false) -- Move to the end of the word
                    end, 100)                                -- Adjust this delay if necessary
                end, opts)

        -- Jump to the previous misspelled word, activate fix mode, and move to the end of the word after correction
        vim.keymap.set('n', '[s', function()
                vim.api.nvim_feedkeys("[s", "n", false)  -- Jump to the previous misspelled word
                vim.api.nvim_feedkeys("z=", "n", false)  -- Trigger spell correction
                vim.defer_fn(function()                  -- Wait for you to select a correction
                    vim.api.nvim_feedkeys("e", "n", false) -- Move to the end of the word
                    end, 100)                                -- Adjust this delay if necessary
                end, opts)
        -- ------------------------
  '';

  config = {
    home.file = {
      # Use the nested config.global.neovimCustomDictionaryPathrelativeToHomeDirectory directly
      "${config.userDefinedGlobalVariables.neovimCustomDictionaryPathrelativeToHomeDirectory}.add".text =
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
