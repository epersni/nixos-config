{ config, ... }:

{
  imports = [ 
      ../common/bash.nix
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
    ".vimrc".text = ''
      set colorcolumn=80
      set ruler
      set rulerformat=%l\:%c
      au FileType gitcommit set cc=72
      set number
      set showmatch
      syntax enable
      set wildmenu
      set wildmode=longest,list
      set hidden
      set backspace=2
      set noswapfile
      set nobackup
      set smarttab
      set shiftwidth=2
      set tabstop=2
      set expandtab
      set autoindent
      set smartindent
      set wrap
      nnoremap <C-j> <C-w>j
      nnoremap <C-k> <C-w>k
      nnoremap <C-h> <C-w>h
      nnoremap <C-l> <C-w>l
      let g:netrw_sort_direction = "normal"
      let g:netrw_sort_by = "name"
      let g:netrw_sort_options = "i"
      let g:netrw_liststyle = 1
      let g:netrw_sort_sequence = '[\/]$,\<core\%(\.\d\+\)\=,\.[a-np-z]$,Makefile,makefile,SConstruct,SConscript,*,\.o$,\.obj$,	\.info$,\.swp$,\.bak$,\~$'
      set ttyfast
      set lazyredraw
      set visualbell
      set t_vb=
      set laststatus=2
      set mouse=a
      filetype indent plugin on
      set ignorecase
      set hlsearch

      " Rename tabs to show tab number.
      if exists("+showtabline")
          function! MyTabLine()
              let s = ""
              let wn = ""
              let t = tabpagenr()
              let i = 1
              while i <= tabpagenr("$")
                  let buflist = tabpagebuflist(i)
                  let winnr = tabpagewinnr(i)
                  let s .= "%" . i . "T"
                  let s .= (i == t ? "%1*" : "%2*")
                  let s .= " "
                  let wn = tabpagewinnr(i, "$")

                  let s .= "%#TabNum#"
                  let s .= i
                  " let s .= "%*"
                  let s .= (i == t ? "%#TabLineSel#" : "%#TabLine#")
                  let bufnr = buflist[winnr - 1]
                  let file = bufname(bufnr)
                  let buftype = getbufvar(bufnr, "buftype")
                  if buftype == "nofile"
                      if file =~ "/."
                          let file = substitute(file, ".*\/\ze.", "", "")
                      endif
                  else
                      let file = fnamemodify(file, ":p:t")
                  endif
                  if file == ""
                      let file = "[No Name]"
                  endif
                  let s .= " " . file . " "
                  let i = i + 1
              endwhile
              let s .= "%T%#TabLineFill#%="
              let s .= (tabpagenr("$") > 1 ? "%999XX" : "X")
              return s
          endfunction
          set stal=2
          set tabline=%!MyTabLine()
          set showtabline=1
          highlight link TabNum Special
      endif
    '';
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
