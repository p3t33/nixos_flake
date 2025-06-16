let
  editor = "nvim";
in
{
  # Variables that will be set "system wide" in the context of the user.
  # E.g, by setting MANPAGER it will be available to bash, zsh, fish with
  # the alternative limiting the scope and setting this variables in a file
  # such as .bashrc using "export".
  home.sessionVariables = {
    EDITOR = editor;
    SUDO_EDITOR = editor;
    MANPAGER = "nvim +Man!";
  };
}
