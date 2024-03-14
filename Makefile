# Commands
ECHO := echo
STOW := stow --target=$(HOME)
SH := sh -c
CLONE := git clone
VIM := vim
SOURCE := source
DOCKER := docker

# Variables
NAME := dotfiles
VER := latest
.DEFAULT_GOAL := dotfiles

# Run the oh-my-zsh install script
.phony: dotfiles
dotfiles:
	$(ECHO) "Installing dotfiles..."
	$(STOW) bash zsh vim tmux
	$(ECHO) "Dotfiles installed."

# Full installation from scratch
.phony: install
install: dotfiles source coc
	$(ECHO) "Completed full installation."

# Source the dotfiles
.phony: source
source:
	$(SOURCE) ~/.profile ~/.zshrc ~/.vimrc ~/.tmux.conf ~/.tmux.conf.local

# Install Coc extensions
.phony: coc
coc:
	$(VIM) -c 'CocInstall -sync coc-snippets coc-prettier coc-lists coc-git coc-diagnostic coc-markdownlint coc-cmake coc-clangd coc-json coc-html|q'	

# # Run the oh-my-zsh install script
# .phony: oh-my-zsh
# oh-my-zsh:
# 	$(SH) "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
# 	$(CLONE) --depth=1 "https://github.com/romkatv/powerlevel10k.git" "${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k"
# 	$(CLONE) "https://github.com/zsh-users/zsh-autosuggestions" "${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/plugins/zsh-autosuggestions"
# 	$(CLONE) "https://github.com/z-shell/F-Sy-H.git" "${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/plugins/F-Sy-H"
