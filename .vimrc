set nocompatible
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'bling/vim-airline'
Bundle 'mattn/webapi-vim'
Bundle 'mattn/gist-vim'
Bundle 'airblade/vim-gitgutter'
Bundle 'kien/rainbow_parentheses.vim'
Bundle 'tmhedberg/SimpylFold'
Bundle 'nathanaelkane/vim-indent-guides'
Bundle 'scrooloose/nerdtree'
Bundle 'chilicuil/vim-sprunge'
Bundle 'itchyny/calendar.vim'
Bundle 'jimenezrick/vimerl'
Bundle 'Glench/Vim-Jinja2-Syntax'
Bundle 'tpope/timl'
Bundle 'sjl/tslime2.vim'
Bundle 'jceb/vim-orgmode'
Bundle 'scrooloose/syntastic'
Bundle 'kien/ctrlp.vim'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'tpope/vim-fugitive'
Bundle 'MarcWeber/vim-addon-mw-utils'
Bundle 'tomtom/tlib_vim'
Bundle 'honza/vim-snippets'
Bundle 'garbas/vim-snipmate'
Bundle 'lyska/lolcode.vim'
Bundle 'flazz/vim-colorschemes'

colorscheme grb256

syntax on
filetype plugin indent on

set whichwrap=<,>,[,],b,
set backspace=indent,eol,start
set ruler
set cursorline
set cursorcolumn
set number
set background=dark
set autoread
set mouse=a

" Leader
let mapleader = " "
let g:mapleader = " "
nmap <leader>w :w!<cr>

" Highlight a line to read over later
nnoremap <silent> <Leader>l ml:execute 'match Search /\%'.line('.').'l/'<CR>

" Turn backup off, since most stuff is in SVN, git et.c anyway...
set nobackup
set nowb
set noswapfile

" But also have C-like languages use C spacing
" Thanks much jdhore
set ai
au BufRead,BufNewFile *.c,*.cpp,*.cxx,*.hpp,*.c++,*.hh,*.hxx,*.ipp,*.moc,*.tcc,*.inl set cindent
au BufRead,BufNewFile *.c,*.cpp,*.cxx,*.hpp,*.c++,*.hh,*.hxx,*.ipp,*.moc,*.tcc,*.inl set tabstop=4
au BufRead,BufNewFile *.c,*.cpp,*.cxx,*.hpp,*.c++,*.hh,*.hxx,*.ipp,*.moc,*.tcc,*.inl set shiftwidth=8
au BufRead,BufNewFile *.c,*.cpp,*.cxx,*.hpp,*.c++,*.hh,*.hxx,*.ipp,*.moc,*.tcc,*.inl set colorcolumn=120
set cinoptions=>s,e0,n0,f0,{0,}0,^0,=s,ps,t0,c3,+s,(2s,us,)20,*30,gs,hs

" Other language specific hacks
autocmd Filetype python setlocal expandtab tabstop=4 shiftwidth=4

" Lolcode is special
au Filetype lolcode setl et ts=4 sw=4

" Email should wrap at 75 characters to allow for replies on an 80 character
" terminal
au Filetype mail let &colorcolumn="70,75"
au Filetype mail set spell
au Filetype mail setlocal textwidth=75
au Filetype mail set expandtab

" APKBUILD files have spacing like python
au BufRead APKBUILD setl noexpandtab softtabstop=0 tabstop=4 shiftwidth=4 nosmarttab

" Make tabs visible
set list
set listchars=tab:>-,trail:~,extends:>,precedes:<

" Default settings
set tabstop=4
"set softtabstop=4
"set shiftwidth=4
"set nowrap
"set smarttab

set encoding=utf-8

" Append modeline after last line in buffer.
" Use substitute() instead of printf() to handle '%%s' modeline in LaTeX
" files.
function! AppendModeline()
    let l:modeline = printf(" vim: set ts=%d sw=%d tw=%d %set :",
                \ &tabstop, &shiftwidth, &textwidth, &expandtab ? '' : 'no')
    let l:modeline = substitute(&commentstring, "%s", l:modeline,
    "")
    call append(line("$"), l:modeline)
endfunction

nnoremap <silent> <Leader>ml :call AppendModeline()<CR>

command WQ wq
command Wq wq
command W w
command Q q

set wildmenu
set wildignore=*.o,*~,*.pyc

" Delete extra spaces 4 at a time
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
match ExtraWhitespace /\s\+$\| \+\ze\t/

" Paste macros
" Thanks to jdhore
map <F8> :set paste<CR>
map <F9> :set nopaste<CR>
imap <F8> <C-O>:set paste<CR>
imap <F9> <nop>
set pastetoggle=<F9>
map <F3> gg=G:w<cr>
nmap <leader>sp gg=G:w<cr>

" status line
set ls=2
set statusline=%F%m%r%h%w\ >\ FORMAT=%{&ff}\ >\ TYPE=%Y\ >\ BUF=\#%n\ <\ POS=%04l,%04v\ <\ %p%%\ <\ LEN=%L

" Lvimrc
" if .lvimrc exists in parent directory of loaded file, load it as config
if filereadable('../.lvimrc')
    source ../.lvimrc
endif
if filereadable('./.lvimrc')
    source ./.lvimrc
endif

" Color column definition
let &colorcolumn="80"
highlight ColorColumn ctermbg=52

" resume at line
autocmd BufReadPost *
            \ if ! exists("g:leave_my_cursor_position_alone") |
            \     if line("'\"") > 0 && line ("'\"") <= line("$") |
            \         exe "normal g'\"" |
            \     endif |
            \ endif

" text files at 78 cols
autocmd BufNewFile,BufRead *.txt
            \ if &tw == 0 && ! exists("g:leave_my_textwidth_alone") |
            \     setlocal textwidth=78 |
            \ endif

" Delete trailing white space on save, useful for Python and CoffeeScript ;)
func! DeleteTrailingWS()
    exe "normal mz"
    %s/\s\+$//ge
    exe "normal `z"
endfunc
autocmd BufWrite *.py :call DeleteTrailingWS()

nmap <leader>d :call DeleteTrailingWS()<cr>
nmap <leader>rt :retab<cr>

" Spellchecking
map <leader>ss :setlocal spell!<cr>

" Gist
let g:gist_clip_command = 'xclip -selection clipboard'
let g:gist_show_privates = 1
let g:gist_detect_filetype = 1
let g:gist_open_browser_after_post = 1
let g:gist_post_private = 1

" Airline
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1

nmap <leader>g :Gist<cr>


