colorscheme niidesert

syntax on
filetype plugin indent on

set whichwrap=<,>,[,],b,
set backspace=indent,eol,start
set ruler
set number
set background=dark

" But also have C-like languages use C spacing
" Thanks much jdhore
set ai
au BufRead,BufNewFile *.c,*.h,*.cpp,*.cxx,*.hpp,*.cc,*.c++,*.hh,*.hxx,*.ipp,*.moc,*.tcc,*.inl set cindent
au BufRead,BufNewFile *.c,*.h,*.cpp,*.cxx,*.hpp,*.cc,*.c++,*.hh,*.hxx,*.ipp,*.moc,*.tcc,*.inl set tabstop=8
au BufRead,BufNewFile *.c,*.h,*.cpp,*.cxx,*.hpp,*.cc,*.c++,*.hh,*.hxx,*.ipp,*.moc,*.tcc,*.inl set shiftwidth=8
set cinoptions=>s,e0,n0,f0,{0,}0,^0,=s,ps,t0,c3,+s,(2s,us,)20,*30,gs,hs

" Default settings
set tabstop=4
set softtabstop=4
set shiftwidth=4
set nowrap
set smarttab

set ls=2
set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [BUF=\#%n]\ [POS=%04l,%04v]\ [%p%%]\ [LEN=%L]

" Delete extra spaces 4 at a time
:highlight ExtraWhitespace ctermbg=red guibg=red
:match ExtraWhitespace /\s\+$/
:match ExtraWhitespace /\s\+$\| \+\ze\t/

" Paste macros
" Thanks to jdhore
map <F8> :set paste<CR>
map <F9> :set nopaste<CR>
imap <F8> <C-O>:set paste<CR>
imap <F9> <nop>
set pastetoggle=<F9>

" Lvimrc
" if .lvimrc exists in parent directory of loaded file, load it as config
let lvimrc_path = expand('%:p:h') . '/.lvimrc'
if filereadable(lvimrc_path)
	execute 'so' lvimrc_path
endif

" Color column definition
let &colorcolumn="80,".join(range(121,999),",")
highlight ColorColumn ctermbg=52

