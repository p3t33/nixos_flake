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
# There are two degrees of freedom when it comes to configuration(both plugins
# and configuration in extraCofnig/extraLuaConfig).
# - The order of imports at the top of the file.
# - The order of plugins.
#
# Meaning:
# - The first plugin in the first import will be the first
# plugin configuration to be generated right after the configuration in
# extraLuaConfig/extraConfig.
# - The first extraConfig/extraLuaConfig in the first import will be at the very
# top of init.lua or the vim typescript files.
#
# To make things more redable, in .nix files that have both plugins and extraConfig/extraLuaConfig
# I put the extraConfig/extraLuaConfig block to the top(as they will be generated).
#
# *for some reason extraLuaConfig inside of spell_checker got to the top of the init.lua
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
    ./firefox_integration.nix
  ];
}
