{ config, pkgs, ... }:

{
  home.username = "nicklas";
  home.homeDirectory = "/home/nicklas";
  home.stateVersion = "24.11";
  home.packages = with pkgs; [
  ];
  xsession.enable = true;
  xsession.windowManager.xmonad = {
    enable = true;
    extraPackages = hpkgs: [ hpkgs.xmonad-contrib ];
  };
  xresources.properties = {
    "XTerm*faceName"= "DejaVu Sans Mono";
    "XTerm*faceSize"= "10";
    "xterm*loginshell"= "true";
    "xterm*savelines"= "16384";
    "xterm*charClass"= "33:48,36-47:48,58-59:48,61:48,63-64:48,95:48,126:48";
    "xterm*rightScrollBar"= "false";
    "xterm*ScrollBar"= "false";
    "xterm*scrollTtyOutput"= "false";
    # dpi makes system font bigger
    "Xft.dpi"="120";
    "*.foreground"="#4a4543";
    "*.background"="#f7f7f7";
    "*.cursorColor"="4a4543";
    #"! black";
    "*.color0"="#090300";
    "*.color8"="#5c5855";
    #"! red";
    "*.color1"="#db2d20";
    "*.color9"="#db2d20";
    #"! green";
    "*.color2"="#01a252";
    "*.color10"="01a252";
    #"! yellow";
    "*.color3"="#99900b";
    "*.color11"= "#c9c36b";
    #"! blue";
    "*.color4"="#01a0e4";
    "*.color12"="#01a0e4";
    #"! magenta";
    "*.color5"="#a16a94";
    "*.color13"="#a16a94";
    #"! cyan";
    "*.color6"="#2b808a";
    "*.color14"="#738587";
    "*.color7"="#a5a2a2";
    "*.color15"="#f7f7f7 ";
  };
  home.file = {
    ".vimrc".text=''
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
    ".bashrc".text=''
    alias ll='ls -alh --color --group-directories-first'
    alias ncopy='xclip -selection clipboard'
    alias npaste='xclip -selection clipboard -o'
    alias prettyjson='python -m json.tool'
    if [ -f /etc/bashrc ]; then
            . /etc/bashrc
    fi
    # Do not record duplicates in command history
    export HISTCONTROL=ignoreboth:erasedups
    '';
    ".gitconfig".text=''
    [user]
      name = nicklas
      email = nicklas.m.persson@gmail.com
    [core]
      editor = vim 
    '';
    ".screenrc".text=''
    caption always # activates window caption
caption string '%{= wk}[ %{k}%H %{k}][%= %{= wk}%?%-Lw%?%{r}(%{r}%n*%f%t%?(%u)%?%{r})%{k}%?%+Lw%?%?%= %{k}][%{b} %d/%m %{k}%c %{k}]' # good looking window bar 
    '';
    ".xmonad/xmobarrc".text=''
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
    ".xmonad/xmonad.hs".text=''
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

  home.sessionVariables = {
    MANWIDTH=80;
  };
  programs.home-manager.enable = true;
}
