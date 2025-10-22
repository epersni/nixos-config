{ config, pkgs, ... }:

{
  xresources.properties = {
    "XTerm*faceName" = "DejaVu Sans Mono";
    "XTerm*faceSize" = "10";
    "xterm*loginshell" = "true";
    "xterm*savelines" = "16384";
    "xterm*charClass" = "33:48,36-47:48,58-59:48,61:48,63-64:48,95:48,126:48";
    "xterm*rightScrollBar" = "false";
    "xterm*ScrollBar" = "false";
    "xterm*scrollTtyOutput" = "false";
    # dpi makes system font bigger
    "Xft.dpi" = "120";
    "*.foreground" = "#4a4543";
    "*.background" = "#f7f7f7";
    "*.cursorColor" = "4a4543";
    #"! black";
    "*.color0" = "#090300";
    "*.color8" = "#5c5855";
    #"! red";
    "*.color1" = "#db2d20";
    "*.color9" = "#db2d20";
    #"! green";
    "*.color2" = "#01a252";
    "*.color10" = "01a252";
    #"! yellow";
    "*.color3" = "#99900b";
    "*.color11" = "#c9c36b";
    #"! blue";
    "*.color4" = "#01a0e4";
    "*.color12" = "#01a0e4";
    #"! magenta";
    "*.color5" = "#a16a94";
    "*.color13" = "#a16a94";
    #"! cyan";
    "*.color6" = "#2b808a";
    "*.color14" = "#738587";
    "*.color7" = "#a5a2a2";
    "*.color15" = "#f7f7f7 ";
  };

}

