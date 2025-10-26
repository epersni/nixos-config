{ config, pkgs, ... }:

{
  programs.git = {
    enable = true;
    userName = "nicklas";
    userEmail = "nicklas.m.persson@gmail.com";
    extraConfig = {
      alias = {
        ll = "log --pretty=format:\"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]\" --decorate --numstat";
        lg = "log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(reset) %C(bold green)(%ar)%C(reset)%C(bold yellow)%d%C(reset)%n %C(reset)%s%C(reset) %C(dim white)- %an%C(reset)'";
        lb = "for-each-ref --sort=-committerdate refs/heads --format='%(committerdate:relative) %09 %(committerdate:short) %09 %(refname:short)'";
      };
      core = {
        editor = "vim";
      };
    };
  };
}

