" Configuration file for vim
set modelines=0		" CVE-2007-2438

" Normally we use vim-extensions. If you want true vi-compatibility
" remove change the following statements
set nocompatible	" Use Vim defaults instead of 100% vi compatibility
set backspace=2		" more powerful backspacing

" show line number
set number
" case insensitive
set ignorecase
set incsearch
set hlsearch

" Don't write backup file if vim is being called by "crontab -e"
au BufWrite /private/tmp/crontab.* set nowritebackup
" Don't write backup file if vim is being called by "chpass"
au BufWrite /private/etc/pw.* set nowritebackup
"colorscheme molokai
syntax on
filetype off

" neobundle
if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim
  call neobundle#rc(expand('~/.vim/bundle/'))
endif
" originalrepos on github
NeoBundle 'mattn/httpstatus-vim'
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'mattn/qiita-vim'
NeoBundle 'thinca/vim-quickrun'
NeoBundle 'plasticboy/vim-markdown'
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'davidhalter/jedi-vim'
NeoBundle 'Shougo/neobundle.vim'
NeoBundle 'Shougo/vimproc'
NeoBundle 'VimClojure'
NeoBundle 'Shougo/vimshell'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/neocomplcache'
NeoBundle 'Shougo/neosnippet'
NeoBundle 'jpalardy/vim-slime'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'tomasr/molokai'
"NeoBundle 'bling/vim-airline'
NeoBundle 'itchyny/lightline.vim'
let g:lightline = {
      \ 'colorscheme': 'solarized-dark',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component': {
      \   'readonly': '%{&filetype=="help"?"":&readonly?"⭤":""}',
      \   'modified': '%{&filetype=="help"?"":&modified?"+":&modifiable?"":"-"}'
      \ },
      \ 'component_visible_condition': {
      \   'readonly': '(&filetype!="help"&& &readonly)',
      \   'modified': '(&filetype!="help"&&(&modified||!&modifiable))'
      \ },
      \ 'separator': { 'left': '⮀', 'right': '⮂' },
      \ 'subseparator': { 'left': '⮁', 'right': '⮃' }
      \ }
NeoBundle 'TwitVim'

filetype plugin indent on     " required!
filetype indent on

set t_Co=256
set background=dark
colorscheme solarized


" jedi/neocomplete settings
autocmd FileType python setlocal omnifunc=jedi#completions

let g:jedi#auto_vim_configuration = 0

if !exists('g:neocomplete#force_omni_input_patterns')
	let g:neocomplete#force_omni_input_patterns = {}
endif
let g:neocomplete#force_omni_input_patterns.python = '\h\w*\|[^. \t]\.\w*'

" Open marked.app with quickrun
let g:quickrun_config = {}
let g:quickrun_config.markdown = {
      \ 'outputter' : 'null',
      \ 'command'   : 'open',
      \ 'cmdopt'    : '-a',
      \ 'args'      : 'Marked',
      \ 'exec'      : '%c %o %a %s',
      \ }

"" unite.vim {{{
" The prefix key.
 nnoremap    [unite]   <Nop>
 nmap    <Leader>f [unite]
"  
"  " unite.vim keymap
"  " <a
"  href="https://github.com/alwei/dotfiles/blob/3760650625663f3b08f24bc75762ec843ca7e112/.vimrc"
"  target="_blank" rel="noreferrer" style="cursor:help;display:inline
"  !important;">https://github.com/alwei/dotfiles/blob/3760650625663f3b08f24bc75762ec843ca7e112/.vimrc</a>
  nnoremap [unite]u  :<C-u>Unite -no-split<Space>
  nnoremap <silent> [unite]f :<C-u>Unite<Space>buffer<CR>
  nnoremap <silent> [unite]b :<C-u>Unite<Space>bookmark<CR>
  nnoremap <silent> [unite]m :<C-u>Unite<Space>file_mru<CR>
  nnoremap <silent> [unite]r :<C-u>UniteWithBufferDir file<CR>
  nnoremap <silent> ,vr :UniteResume<CR>
"   
"   " vinarise
   let g:vinarise_enable_auto_detect = 1
"    
"    " unite-build map
    nnoremap <silent> ,vb :Unite build<CR>
    nnoremap <silent> ,vcb :Unite build:!<CR>
    nnoremap <silent> ,vch :UniteBuildClearHighlight<CR>
    "" }}}

nnoremap <silent> ,gs :<C-u>GitGutterToggle<CR>
nnoremap <silent> ,gh :<C-u>GitGutterLineHighlightsToggle<CR>

" Twitvim 関連
let twitvim_count = 40
nnoremap <C-t> :<C-u>PosttoTwitter<CR>
nnoremap ,tf :<C-u>FriendsTwitter<CR><C-w>j
nnoremap ,tu :<C-u>UserTwitter<CR><C-w>j
nnoremap ,tr :<C-u>RepliesTwitter<CR><C-w>j
nnoremap ,tn :<C-u>NextTwitter<CR>

autocmd FileType twitvim call s:twitvim_my_settings()
function! s:twitvim_my_settings()
  set nowrap
endfunction
syntax on
filetype detect
