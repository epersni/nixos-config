{ config, pkgs, ... }:

{
  programs.git = {
    enable = true;
    userName = "nicklas";
    userEmail = "nicklas.m.persson@gmail.com";
    extraConfig = {
      core = {
        editor = "vim";
      };
    };
  };
}

