# As the order in which settings are being evaluate by neovim is important there
# is a need to understand how nix generates the configurations for neovim.
#
# There are 4 types of configurations
# - extraConfig: which is general vim script.
# - extraLuaConfig: which is general lua configuration.
# - plugin config: which can be of type lua or vim script.
#
# What is generated
# - init.lua file which is located at ~/.config/nvim/init.lua
# - vim script config file that is not located directly inside of ~/.config/nvim
#   but is part of the /nix/store.
#
# init.lua is sourcing the vim script config at the very top. Meaning that
# typescript configuration will be evaluated by neovim first and only then lua
# configuration.
#
# How are the configuration files being constructed?
# - for lua, everything in extraLuaConfig goes to the very top, then all the
# configuration provided by the plugins from type lua.
# - for vim type script everything in extraConfig goes to the very top then all the configuration
# provided by the plugins that are not defined as lua.
#
#
# Each module uses lib.mkOrder to control evaluation order:
#   100 = core, 200 = spell_checker, 300 = ui, 400 = completion,
#   500 = debugger, 600 = search_and_select, 700 = git.
# This applies to both extraLuaConfig and plugins lists.
#
# To make things more readable, in .nix files that have both plugins and extraConfig/extraLuaConfig
# I put the extraConfig/extraLuaConfig block to the top(as they will be generated).
{
  imports = [
     # All modules use config.programs.neovim.enable to be activated, with
     # Core being the essential module.
    ./core.nix
    ./spell_checker.nix
    ./ui.nix
    ./completion.nix
    ./debugger.nix
    ./search_and_select.nix
    ./git.nix
  ];
}
