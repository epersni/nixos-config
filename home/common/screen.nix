{ config, pkgs, ... }:

{
  home.file = {
    ".screenrc".text = ''
      caption always # activates window caption
      caption string '%{= wk}[ %{k}%H %{k}][%= %{= wk}%?%-Lw%?%{r}(%{r}%n*%f%t%?(%u)%?%{r})%{k}%?%+Lw%?%?%= %{k}][%{b} %d/%m %{k}%c %{k}]' # good looking window bar 
    '';
  };
}


