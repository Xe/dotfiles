filetype off
set nocompatible
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'Shougo/unite.vim'
Plugin 'tyru/open-browser.vim'
Plugin 'lambdalisue/vim-gista'
Plugin 'airblade/vim-gitgutter'
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'tmhedberg/SimpylFold'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'
Plugin 'Shougo/vimproc.vim'
Plugin 'Shougo/vimshell.vim'
Plugin 'chilicuil/vim-sprunge'
Plugin 'jimenezrick/vimerl'
Plugin 'Glench/Vim-Jinja2-Syntax'
Plugin 'tpope/vim-fugitive'
Plugin 'vim-scripts/vimwiki'
Plugin 'Xe/vim-licenses'
Plugin 'vim-scripts/fountain.vim'
Plugin 'tkztmk/vim-vala'
Plugin 'ekalinin/Dockerfile.vim'
Plugin 'junegunn/goyo.vim'
Plugin 'jnwhiteh/vim-golang'
Plugin 'Raimondi/delimitMate'
Plugin 'leafo/moonscript-vim'
Plugin 'nsf/gocode', {'rtp': 'vim/'}
Plugin 'valloric/YouCompleteMe'

call vundle#end()

set t_Co=256

colorscheme desert

syntax on
filetype plugin indent on

set whichwrap=<,>,[,],b,
set backspace=indent,eol,start
set number
set background=dark
set autoread
set mouse=a
set showcmd             " Show commands on the right as they're being typed
set selection=exclusive " Don't include cursor in visual selections

hi CursorColumn term=none cterm=none ctermbg=234
hi CursorLine term=none cterm=none ctermbg=236

hi Folded term=none cterm=none ctermbg=232
hi Comment term=bold cterm=bold ctermfg=green

" Turn backup off, since most stuff is in SVN, git et.c anyway...
set nobackup
set nowb
set noswapfile

" But also have C-like languages use C spacing
" Thanks much jdhore
set ai
au BufRead,BufNewFile *.c,*.cpp,*.cxx,*.hpp,*.c++,*.hh,*.hxx,*.ipp,*.moc,*.tcc,*.inl set cindent
au BufRead,BufNewFile *.c,*.cpp,*.cxx,*.hpp,*.c++,*.hh,*.hxx,*.ipp,*.moc,*.tcc,*.inl set tabstop=8
au BufRead,BufNewFile *.c,*.cpp,*.cxx,*.hpp,*.c++,*.hh,*.hxx,*.ipp,*.moc,*.tcc,*.inl set shiftwidth=8
set cinoptions=>s,e0,n0,f0,{0,}0,^0,=s,ps,t0,c3,+s,(2s,us,)20,*30,gs,hs

" Other language specific hacks
autocmd Filetype python setlocal expandtab tabstop=4 shiftwidth=4

" Lolcode is special
au Filetype lolcode setl et ts=4 sw=4

" So is Elixir
au Filetype elixir setl et ts=2 sw=2

" Make Lua have ruby spacing for reading
au Filetype lua setl et ts=2 sw=2
au Filetype moon setl et ts=2 sw=2

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
"set tabstop=4
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
highlight ColorColumn ctermbg=58

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

" Gist
let g:gist_clip_command = 'xclip -selection clipboard'
let g:gist_show_privates = 1
let g:gist_detect_filetype = 1
let g:gist_open_browser_after_post = 1
let g:gist_post_private = 1

" Airline
"let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 0
let g:airline_theme = 'airlineish'

" Leader
let mapleader = " "
let g:mapleader = " "

" Highlight a line to read over later
nnoremap <silent> <Leader>l ml:execute 'match Search /\%'.line('.').'l/'<CR>
nmap <leader>w :w!<cr>
nmap <leader>d :call DeleteTrailingWS()<cr>
nmap <leader>rt :retab<cr>
nmap <leader>g :Gist<cr>
nnoremap <silent> <Leader>ml :call AppendModeline()<CR>
nmap <leader>sp gg=G:w<cr>

" Spellchecking
map <leader>ss :setlocal spell!<cr>

" Rainbow Parens
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

let g:rbpt_colorpairs = [
    \ ['49',  '49'],
    \ ['192', '19'],
    \ ['190', '243'],
    \ ['203', '22'],
    \ ['170', '27'],
    \ ['52',  '52'],
    \ ['57',  '57'],
    \ ['95',  '95'],
    \ ['250', '250'],
    \ ['17',  '17'],
    \ ['208', '55'],
    \ ['201', '23'],
    \ ['25',  '25'],
    \ ['94',  '94'],
    \ ]

let g:rbpt_max = 14

" License
let g:licenses_authors_name = 'Christine Dodrill <xena@yolo-swag.com>'

" Goyo
function! Goyo_before()
  silent !tmux set status off
  set noshowmode
  set noshowcmd
  " ...
endfunction

function! Goyo_after()
  silent !tmux set status on
  set showmode
  set showcmd
  " ...
endfunction

let g:goyo_callbacks = [function('Goyo_before'), function('Goyo_after')]

let g:gista#github_user = 'Xe'

let g:vimshell_prompt_expr = 'escape(fnamemodify(getcwd(), ":~").">", "\\[]()?! ")." "'
let g:vimshell_prompt_pattern = '^\%(\f\|\\.\)\+> '
