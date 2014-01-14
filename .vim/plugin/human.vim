" Vim plugin to enhance the writing of human readable formats, such as Markdown.
" Last Change:  2013 October 25
" Maintainer:   Vivien Didelot <vivien@didelot.org>
" License:      This file is distributed under the Beerware license.

if exists("g:loaded_human")
    finish
endif
let g:loaded_human = 1

augroup human
    autocmd!

    autocmd BufNewFile,BufRead *.{md,mdown,markdown} setlocal filetype=markdown

    autocmd FileType markdown,rst,asciidoc,pod call s:HumanSpellChecking()
    autocmd FileType markdown,rst,asciidoc,pod call s:HumanFormatting()

    autocmd BufNewFile,BufRead *.{txt,textile,rdoc,org,creole,mediawiki} call s:HumanSpellChecking()
    autocmd BufNewFile,BufRead *.{txt,textile,rdoc,org,creole,mediawiki} call s:HumanFormatting()

    autocmd FileType gitcommit call s:HumanSpellChecking()

    autocmd FileType help setlocal nospell
augroup END

function s:HumanSpellChecking()
    setlocal nolist
    setlocal spell
    if &keywordprg == "man"
        setlocal keywordprg=sdcv
    endif
    setlocal dictionary=/usr/share/dict/words
    setlocal complete=.,w,k
    setlocal infercase
endfunction

function s:HumanFormatting()
    setlocal et ts=4 sw=4
    setlocal nojs
    setlocal textwidth=0
    setlocal wrap
    setlocal autoindent
    setlocal formatoptions=tnb1aw
endfunction

if !exists(":HumanSpellChecking")
    command -nargs=0 HumanSpellChecking :call s:HumanSpellChecking()
endif

if !exists(":HumanFormatting")
    command -nargs=0 HumanFormatting :call s:HumanFormatting()
endif

" vim: ts=4 et sw=4
