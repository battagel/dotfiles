# Commands
ECHO := echo
STOW := stow --target=$(HOME) --ignore=.DS_Store --verbose
SH := sh -c
CLONE := git clone
VIM := vim
SOURCE := source
DOCKER := docker

# Variables
NAME := dotfiles
VER := latest
.DEFAULT_GOAL := options

# Run the oh-my-zsh install script
.PHONY: stow
stow:
	$(ECHO) "Installing dotfiles..."
	$(ECHO) "Dotfiles installed."

.PHONY: options
options:
	@echo "Select which dotfiles to install:"
	@echo "1. Bash"
	@echo "2. Zsh"
	@echo "3. Vim"
	@echo "4. Tmux"
	@echo "5. Emacs"
	@echo "Enter the numbers of the options you want to select separated by spaces:"
	@bash -c 'read -r options; \
		if [ -z "$$options" ]; then \
			echo "No option selected, exiting..."; \
			exit 1; \
			fi; \
		for opt in $$options; do \
			case $$opt in \
				1) $(STOW) bash;; \
				2) $(STOW) zsh;; \
				3) $(STOW) vim;; \
			  4) $(STOW) tmux;; \
			  5) $(STOW) emacs;; \
				*) echo "Invalid option: $$opt"; exit 1;; \
			esac; \
		done'
		@echo "Dotfiles loaded, Enjoy!"
	
# Full installation from scratch
.PHONY: install
install: dotfiles source coc
	$(ECHO) "Completed full installation."

# Source the dotfiles
.PHONY: source
source:
	$(SOURCE) ~/.profile ~/.zshrc ~/.vimrc ~/.tmux.conf ~/.tmux.conf.local

# Install Coc extensions
.PHONY: coc
coc:
	$(VIM) -c 'CocInstall -sync coc-snippets coc-prettier coc-lists coc-git coc-diagnostic coc-markdownlint coc-cmake coc-clangd coc-json coc-html|q'	

# # Run the oh-my-zsh install script
# .PHONY: oh-my-zsh
# oh-my-zsh:
# 	$(SH) "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
# 	$(CLONE) --depth=1 "https://github.com/romkatv/powerlevel10k.git" "${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k"
# 	$(CLONE) "https://github.com/zsh-users/zsh-autosuggestions" "${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/plugins/zsh-autosuggestions"
# 	$(CLONE) "https://github.com/z-shell/F-Sy-H.git" "${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/plugins/F-Sy-H"
