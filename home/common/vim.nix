{ config, ... }:

{
  programs.vim = {
    enable = true;
    defaultEditor = true;
    settings = {
      background = "light";
      hidden = true;
      number = true;
    };
    extraConfig = ''
      set colorcolumn=80
      set ruler
      set rulerformat=%l\:%c
      set showmatch
      set wildmenu
      set wildmode=longest,list
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
      set ttyfast
      set lazyredraw
      set visualbell
      set t_vb=
      set laststatus=2
      set mouse=a
      set ignorecase
      set hlsearch
      au FileType gitcommit set cc=72
      nnoremap <C-j> <C-w>j
      nnoremap <C-k> <C-w>k
      nnoremap <C-h> <C-w>h
      nnoremap <C-l> <C-w>l

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

      let g:netrw_sort_direction = "normal"
      let g:netrw_sort_by = "name"
      let g:netrw_sort_options = "i"
      let g:netrw_liststyle = 1
      let g:netrw_sort_sequence = '[\/]$,\<core\%(\.\d\+\)\=,\.[a-np-z]$,Makefile,makefile,SConstruct,SConscript,*,\.o$,\.obj$,	\.info$,\.swp$,\.bak$,\~$'
      filetype indent plugin on
    '';
  };
}
