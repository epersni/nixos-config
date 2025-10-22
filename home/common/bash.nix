{ config, pkgs, ... }:

{
  programs.bash.shellAliases = {
      ll = "ls -alh --color --group-directories-first";
      ncopy = "xclip -selection clipboard";
      npaste = "xclip -selection clipboard -o";
      prettyjson = "python -m json.tool";
  };
  programs.bash.historyControl = ["ignoredups" "erasedups"];
  programs.bash.sessionVariables = {
    MANWIDTH = 80;
  };
}
