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
      gdb
      lldb
    ];

    plugins = lib.mkOrder 500 (with pkgs.vimPlugins; [
      nvim-dap-ui
      {
        plugin = nvim-dap;
        type = "lua";
        config = ''

            local dap = require("dap")

            -- requres gdb 14.0+ which I didn't installed yet.
            dap.adapters.gdb = {
                type = "executable",
                command = "${lib.getExe pkgs.gdb}",
                args = { "-i", "dap" }
            }


            dap.adapters.lldb = {
                type = 'executable',
                command = '${lib.getExe' pkgs.lldb "lldb-dap"}',
                name = 'lldb'
            }

            dap.configurations.c = {
            {
                name = "Launch",
                type = "lldb",
                request = "launch",
                program = function()
                    return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
                    end,
                cwd = vim.fn.getcwd(),
                stopAtEntry = true,
                args = {},
            },
            }

            -- Auto open and close debug ui
            -- ----------------------------
            local dapui = require("dapui")
            dapui.setup() -- Setup dapui

            -- Automatically open dapui when the debug session starts
            dap.listeners.after.event_initialized["dapui"] = function()
                dapui.open()
            end
            -- ----------------------------

          dap.configurations.cpp = dap.configurations.c -- For C++

          local opts = { noremap = true, silent = true }
          -- Setting breakpoints
          vim.keymap.set('n', '<leader>db', function() require'dap'.toggle_breakpoint() end, opts)

          -- Launching debug sessions and resuming execution
          vim.keymap.set('n', '<leader>dc', function() require'dap'.continue() end, opts)

          -- Stepping over code
          vim.keymap.set('n', '<leader>do', function() require'dap'.step_over() end, opts)

          -- Stepping into code
          vim.keymap.set('n', '<leader>di', function() require'dap'.step_into() end, opts)

          -- close debug session
          vim.keymap.set('n', '<leader>dq', function()
              require('dap').disconnect()
              require('dap').close()
              require('dapui').close()
              end, opts)
        '';
      }

    ]);
  };
  };
}
