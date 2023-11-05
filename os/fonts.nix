{ pkgs, config, ... }:
{
    # will need to be updated from fonts.fonts to fonts.packages post 23.05
    # In the future I might move this settings to the home-manger using the
    # fonts.fontconfig.enable directive.
    fonts = {
        enableDefaultFonts = true;
        fonts = with pkgs; [
            nerdfonts
            powerline-fonts
            font-awesome
        ];
        fontconfig = {
            defaultFonts = {
                # Each character in a monospace font takes up the same amount of horizontal space
                # his uniform spacing is useful for aligning characters vertically,
                # which is why these fonts are preferred in coding environments and
                # text editors where such alignment is beneficial.
                #
                # I am using nerdfonts which are patched font with extra glyphs.
                # This type of font should be used with vim, tmux, VSCode...
                monospace = [ config.userDefinedGlobalVariables.font.mono ];
                # Sans-serif fonts lack the small projecting features at the end of strokes
                # They tend to have a more modern and clean look.
                #
                # Sans-serif fonts are often used for shorter lengths of text,
                # such as captions, labels, and headings. They are also commonly
                # used in user interfaces (UI) and on screens because they can be easier to
                # read at various screen resolutions, and their simpler design can be more legible
                # at smaller sizes.
                #
                # This type of fonts should be used for GUI elements such as
                # window decorations, browser settings...
                sansSerif = [ config.userDefinedGlobalVariables.font.sansSerif ];
                # Serif fonts have small lines or strokes attached to the ends of
                # letters and symbols. This is believed to lead the eye along lines of
                #text, making long passages easier to read.
                #
                # Serif fonts are often used for large blocks of text, like in books,
                # newspapers, and lengthy articles.
                serif = [ config.userDefinedGlobalVariables.font.serif ];
            };
            antialias = true;
            hinting = {
                enable = true;
                autohint = true;
            };
        };
    };
}

