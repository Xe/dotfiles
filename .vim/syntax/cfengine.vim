" Vim syntax file
" Language:	Cfengine configure file
" Maintainer:	Christain Pearce <christian@pearcec.com>
" Last Change:	2004 Sep 23
" Version:  0.3

" 1. Add this file to ~/.vim/sytnax/
"
" 2. Add the following to ~/.vim/scripts.vim
"
"     if did_filetype()
"      finish
"    endif
"    if search('^#\s*vim:\s*set\s*syntax=cfengine')
"      setf cfengine
"    endif
"
" 3. Put the following anywhere in the comments of a cfengine file
" to automatically load cfengine highlighting.
"
"   vim: set syntax=cfengine
"

" 0.3 - Fixed PHP highlighting inside strings
" 0.2 - Fixed TODO
"     - Improved the class pattern match to allow for oneliners
"     - Added define=classname to the list of things to be highlighted
" 0.1 - First release

" TODO: Add highlighting to variables under the control section
"       Add highlighting to groups under the groups|classes section
"       Fix define=classname,classname


" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Include PHP syntax hightlighting
if version < 600
  syn include @phpTop <sfile>:p:h/php.vim
else
  syn include @phpTop syntax/php.vim
endif
syn sync clear
unlet b:current_syntax

syn match       cfengineClassOperator        "[$!&|.()]"       contained display
syn match       cfengineVarSelector     "[$(){}]"       contained display

syn keyword	cfengineTodo	contained TODO FIXME XXX
" Avoid matching "text#text", used in /etc/disktab and /etc/gettytab
syn match	cfengineComment	"^#.*" contains=cfengineTodo
syn match	cfengineComment	"\s#.*"ms=s+1 contains=cfengineTodo
syn region      cfengineIdentifier  start="\${" end="}" oneline contains=cfengineVarSelector keepend extend
syn region      cfengineIdentifier  start="\$(" end=")" oneline contains=cfengineVarSelector keepend extend
syn region      cfenginePHP     start="<?" end="?>" contains=@phpTop keepend extend
syn match	cfengineSection	"\s*\a\+:\s*$"he=e-1
syn match       cfengineClass   "\s*\S*::"he=e-2 contains=cfengineClassOperator
syn match       cfengineClass "define=\w\+"hs=s+7
syn region	cfengineString	start=+"+ skip=+\\\\\|\\"+ end=+"+ oneline contains=cfengineIdentifier,cfenginePHP keepend extend
syn region	cfengineString	start=+'+ skip=+\\\\\|\\'+ end=+'+ oneline contains=cfengineIdentifier,cfenginePHP keepend extend

" Do something for equals


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_conf_syntax_inits")
  if version < 508
    let did_conf_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink cfengineComment	Comment
  HiLink cfengineTodo	Todo
  HiLink cfengineString	String
  HiLink cfengineIdentifier	Identifier
  HiLink cfengineSection    Statement
  HiLink cfengineClassOperator    Operator
  HiLink cfengineVarSelector    Operator
  HiLink cfengineClass  Special

  delcommand HiLink
endif

let b:current_syntax = "cfengine"

" vim: ts=8 sw=2
