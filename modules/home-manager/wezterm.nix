{ config, lib, pkgs, ... }:

{
  config = lib.mkIf config.programs.wezterm.enable {
    programs.wezterm = {
      package = pkgs.wezterm;
      enableZshIntegration = false;
      enableBashIntegration = false;

      extraConfig = ''
        wezterm.on("format-window-title", function(tab, pane, tabs)
          local process_name = pane.foreground_process_name or ""
          if not process_name:match("/bash$") and not process_name:match("/zsh$") then
            return
          end

          local cwd = pane.current_working_dir
          local hostname = wezterm.hostname()
          if not cwd or cwd.scheme ~= "file" or not cwd.file_path
            or (cwd.host ~= "" and cwd.host ~= "localhost" and cwd.host ~= hostname) then
            return
          end

          local path = cwd.file_path
          local home = os.getenv("HOME")
          if home and (path == home or path:sub(1, #home + 1) == home .. "/") then
            path = "~" .. path:sub(#home + 1)
          end

          local prefix = pane.is_zoomed and "[Z] " or ""
          if #tabs > 1 then
            prefix = prefix .. string.format("[%d/%d] ", tab.tab_index + 1, #tabs)
          end
          return prefix .. string.format("%s@%s:%s", os.getenv("USER") or "", hostname, path)
        end)
      '';

      settings = {
        font = lib.generators.mkLuaInline ''wezterm.font("${config.custom.shared.font.mono}")'';
        font_size = 16;
        color_scheme = "Nord (Gogh)";
        window_background_opacity = 0.95;
        scrollback_lines = 10000;
        hide_tab_bar_if_only_one_tab = true;
        hide_mouse_cursor_when_typing = true;
        window_close_confirmation = "NeverPrompt";
        keys = [
          {
            key = "Escape";
            mods = "CTRL";
            action = lib.generators.mkLuaInline "wezterm.action.ActivateCopyMode";
          }
          {
            key = "x";
            mods = "CTRL|SHIFT";
            action = lib.generators.mkLuaInline "wezterm.action.DisableDefaultAssignment";
          }
        ];
      };
    };
  };
}
