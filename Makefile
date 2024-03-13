# Commands
ECHO := echo
SH := sh
CLONE := git clone
STOW := stow --target=$(HOME)
SOURCE := source
DOCKER := docker

# Variables
NAME := dotfiles
VER := latest
.DEFAULT_GOAL := dotfiles

# Full installation from scratch
.phony: install
install: oh-my-zsh dotfiles source
	$(ECHO) "Completed full installation."

# Run the oh-my-zsh install script
.phony: dotfiles
dotfiles:
	$(ECHO) "Installing dotfiles..."
	$(STOW) bash zsh vim tmux
	$(ECHO) "Dotfiles installed."

# Source the dotfiles
.phony: source
source:
	$(SOURCE) ~/.profile ~/.zshrc ~/.vimrc ~/.tmux.conf ~/.tmux.conf.local

# Run the oh-my-zsh install script
.phony: oh-my-zsh
oh-my-zsh:
	$(SH) -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
    $(CLONE) --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k
	$(CLONE) clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
	$(CLONE) clone https://github.com/z-shell/F-Sy-H.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/plugins/F-Sy-H

