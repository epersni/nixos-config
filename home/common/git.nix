{ config, pkgs, ... }:

{
  programs.git = {
    enable = true;
    userName = "nicklas";
    userEmail = "nicklas.m.persson@gmail.com";
    extraConfig = {
      alias = {
        ll = ''
          log --pretty=format:"%C(yellow)%h%Cred%d\ %Creset%s%Cblue\ [%cn]" --decorate --numstat'';
        hist =
          "log --graph --pretty=format:'%C(auto)%h%d %s %C(black)%C(bold)(%cr by %an)' --abbrev-commit --date=relative";
        lb =
          "for-each-ref --sort=-committerdate refs/heads --format='%(committerdate:relative) %09 %(committerdate:short) %09 %(refname:short)'";
      };
      core = { editor = "vim"; };
    };
  };
}

