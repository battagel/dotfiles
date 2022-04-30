set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'vim-airline/vim-airline'

Plugin 'preservim/nerdtree'
Plugin 'preservim/tagbar'
Plugin 'preservim/nerdcommenter' "Use gcc to comment lines
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'jistr/vim-nerdtree-tabs'
Plugin 'tiagofumo/vim-nerdtree-syntax-highlight'

Plugin 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plugin 'junegunn/fzf.vim'

Plugin 'neoclide/coc.nvim', { 'branch': 'release'}

Plugin 'sheerun/vim-polyglot'
" Plugin 'mattn/emmet-vim' not currently needed
Plugin 'frazrepo/vim-rainbow'
" Plugin 'tabnine/YouCompleteMe' Maybe doesnt work?
Plugin 'dense-analysis/ale'
Plugin 'vim-syntastic/syntastic'
Plugin 'airblade/vim-gitgutter'
Plugin 'christoomey/vim-tmux-navigator'
" Has to load after most plugins
Plugin 'ryanoasis/vim-devicons'
" Plugin 'prettier/vim-prettier', {'do': 'yum install' }

" Python specific
Plugin 'vim-scripts/indentpython.vim'
Plugin 'nvie/vim-flake8'
"Plugin 'jiangmiao/auto-pairs'

" For Typescript
" Plugin 'HerringtonDarkholme/yats.vim'

" To take a look at
" Changes the default vim command to a dashboard for ease of use
" Plugin 'glepnir/dashboard-nvim'

" Colour Themes
Plugin 'tomasiser/vim-code-dark'
Plugin 'sainnhe/sonokai'
Plugin 'sonph/onehalf'
Plugin 'morhetz/gruvbox'
Plugin 'joshdick/onedark.vim'
Plugin 'savq/melange'
Plugin 'tomasr/molokai'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Colour theme
colorscheme sonokai
hi Normal guibg=NONE ctermbg=NONE
set termguicolors

" Stop comments being italic - causes highlighting problem
let g:sonokai_disable_italic_comment=1

" Fix backspace not working issue
set backspace=indent,eol,start

" Line numbers on
set number
" Bell notification off
set belloff=all
" Turn on syntax highlighting
syntax on
" Enable filetype plugins
filetype plugin on

" Tab navigation
nnoremap H :tabprevious<CR>
nnoremap L :tabnext<CR>

" Encoding type
set encoding=utf-8

" Clipboard
set clipboard=unnamed

" Mouse active
set mouse=a

" Set view command tab autocompletetion on 
set wildmenu

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugin Settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Nerd Tree
nnoremap <silent> <C-z> :NERDTreeToggle<CR>
nnoremap <silent> <C-x> :NERDTreeFind<CR>
let NERDTreeQuitOnOpen = 1
let NERDTreeShowHidden=1
let NERDTreeAutoDeleteBuffer = 1
let g:webdevicons_conceal_nerdtree_brackets = 1

" Tagbar
nnoremap <silent> <C-s> :TagbarToggle<CR>

" Airline
let g:airline_powerline_fonts=1
let g:airline#extensions#tabline#enabled = 1

" Fuzzy Finder
nnoremap <C-p> :Files<Cr>

" Coc.nvim
let g:coc_disable_startup_warning = 1
" Run :CocInstall <package> to install new language support
" Run :CocList to view packages

" Rainbow brackets
let g:rainbow_active = 1

" Status Line
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*

" Dashboard
let g:dashboard_default_executive ='fzf'

" Syntastic
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_python_checkers = ['flake8']
let g:syntastic_python_python_exec = 'python3'

" This will highlight bad white space live when coding - Kinda annoying
" highlight BadWhitespace ctermbg=red guibg=red
au BufNewFile,BufRead *.py
    \ set tabstop=4 |
    \ set softtabstop=4 |
    \ set shiftwidth=4 |
    \ set textwidth=79 |
    \ set expandtab |
    \ set autoindent |
    \ set fileformat=unix
"au BufRead,BufNewFile *.py,*.pyw match BadWhitespace /^\t\+/
"au BufRead,BufNewFile *.py,*.pyw, set textwidth=100
"au BufRead,BufNewFile *.py,*.pyw,*.c,*.h match BadWhitespace /\s\+$/

let python_highlight_all=1

" YouCompleteMe
" let g:ycm_autoclose_preview_window_after_completion=1
" map <leader>g  :YcmCompleter GoToDefinitionElseDeclaration<CR>
