{ config, ... }:

{
  imports = [
    ../common/bash.nix
    ../common/vim.nix
    ../common/git.nix
    ../common/xresources.nix
    ../common/screen.nix
  ];
  home.username = "nicklas";
  home.homeDirectory = "/home/nicklas";
  home.stateVersion = "24.05";
  xsession.enable = true;
  xsession.windowManager.xmonad = {
    enable = true;
    extraPackages = hpkgs: [ hpkgs.xmonad-contrib ];
  };
  home.file = {
    ".xmonad/xmobarrc".text = ''
      Config { font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
          , borderColor = "black"
          , border = TopB
          , bgColor = "black"
          , fgColor = "grey"
          , position = TopW L 100
          , commands = [  Run Network "eth0" ["-L","0","-H","32","--normal","green","--high","red"] 10
                          , Run Network "eth1" ["-L","0","-H","32","--normal","green","--high","red"] 10
                          , Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
                          , Run Memory ["-t","Mem: <usedratio>%"] 10
                          , Run Swap [] 10
                          , Run DiskU [("/", "sda1: <used>(<size>)"), ("sda1", "<usedbar>")] ["-L", "20", "-H", "50", "-m", "1", "-p", "3"] 20
                          , Run ThermalZone 0 ["-t","<temp>C"] 30
                          , Run ThermalZone 1 ["-t","<temp>C"] 30
                          , Run ThermalZone 2 ["-t","<temp>C"] 30
                          , Run CoreTemp ["-t", "<core0>,<core1>,<core2>,<core3>C", "-L", "40", "-H", "60", "-l", "lightblue", "-n", "gray90", "-h", "red"] 50
                          , Run Com "uname" ["-s","-r"] "" 36000
                          , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                          , Run StdinReader
                          ]
          , sepChar = "%"
          , alignSep = "}{"
          , template = "%StdinReader% | %cpu% | %memory%, %swap% | %disku% | CpuTemp: %coretemp%, TempZones: %thermal0%, %thermal1%, %thermal2% }{<fc=#ee9a00>%date%</fc> | %uname% "
          , lowerOnStart = True
        }
    '';
    ".xmonad/xmonad.hs".text = ''
      import XMonad
      import XMonad.Util.Run(spawnPipe)
      import XMonad.Hooks.ManageDocks
      import XMonad.Hooks.DynamicLog
      import System.IO(hPutStrLn)

      main = do
        xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
        xmonad defaultConfig
              { manageHook = manageDocks <+> manageHook defaultConfig
              , layoutHook = avoidStruts  $  layoutHook defaultConfig
              , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
              , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle  = xmobarColor "green" "" . shorten 50
                        }
              , modMask = mod4Mask -- Use Super instead of Alt
              -- more changes
              }
    '';
  };

  programs.home-manager.enable = true;
}
