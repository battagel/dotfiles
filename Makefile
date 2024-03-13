# Commands
ECHO := echo
STOW := stow --target=$(HOME)
SOURCE := source
DOCKER := docker

# Variables
NAME := dotfiles
VER := latest
.DEFAULT_GOAL := dotfiles

# Full installation from scratch
.phony: install
install: oh-my-zsh zsh-suggestions dotfiles source
	$(ECHO) "Completed full installation."

# Run the oh-my-zsh install script
.phony: dotfiles
dotfiles:
	$(ECHO) "Installing dotfiles..."
	$(STOW) zsh vim tmux
	$(ECHO) "Dotfiles installed."

# Source the dotfiles
.phony: source
source:
	$(SOURCE) ~/.profile ~/.zshrc ~/.vimrc ~/.tmux.conf ~/.tmux.conf.local

# Run the oh-my-zsh install script
.phony: oh-my-zsh
oh-my-zsh:
	sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# Install zsh-autosuggestions plugin
.phony: zsh-suggestions
zsh-suggestions:
	git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

