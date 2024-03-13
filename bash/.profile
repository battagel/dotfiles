###################
# - Bash Config - #
###################
# Happens only on login shells. TMUX defaults to login shells

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

##################
# GENERAL CONFIG #
##################

# Environment variables
export SHELL=/bin/zsh

# Bash Terminal Beautification (Not zsh)
ACCENT_COL="\[$(tput setaf 215)\]"
TEXT_COL1="\[$(tput setaf 249)\]"
RESET_COL="\[$(tput sgr0)\]"
if [[ "$TERM" =~ 256color ]]; then
    PS1="${TEXT_COL1}\u${RESET_COL}@${ACCENT_COL}\h${RESET_COL}:${TEXT_COL1}\w${RESET_COL}>"
fi
echo -ne "\e]12;white\a"

# Aliases
## General
alias ll="ls -l" 
alias la="ls -al"
alias cl="clear"

cd $HOME  