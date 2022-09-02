#!/bin/bash

# ENVIRONMENT VARIABLES
export PATH=/opt/hpe/bin:auto/share/bin:~/.local/bin:~/.emacs.d/bin:$PATH
export TERM=xterm-256color
export PYTHONPATH=:/auto/share/repo/cfl.latest/lib/:/auto/share/repo/zeus.latest/infra/lib/:/auto/share/repo/zeus.latest/infra
unset http_proxy https_proxy HTTP_PROXY HTTPS_PROXY

# TERMINAL BEAUTIFICATION

ACCENT_COL="\[$(tput setaf 215)\]"
TEXT_COL1="\[$(tput setaf 249)\]"
RESET_COL="\[$(tput sgr0)\]"

if [ -f /etc/bashrc ]; then
      . /etc/bashrc   # --> Read /etc/bashrc, if present.
fi

if [[ "$TERM" =~ 256color ]]; then
    PS1="${TEXT_COL1}\u${RESET_COL}@${ACCENT_COL}\h${RESET_COL}:${TEXT_COL1}\w${RESET_COL}>"
fi

echo -ne "\e]12;white\a"

#  ALIASES

alias gst="git status"
alias gaa="git add ."
alias gau="git add -u"
alias gcmsg="git commit -m "
alias gl="git pull"
alias gp="git push"
alias la="ls -al"
alias ll="ls -l"
alias l="ls -l"
alias cl="clear"

# Nimble Specific
alias li_pebble="package.pl --op list --branch=default --date --verbose"
alias li_ocean='package.pl --op list --branch=rel-ocean --date --verbose'
alias li_fiji='package.pl --op list --branch=rel-fiji --date --verbose'
alias li_newport='package.pl --op list --branch=rel-newport --date --verbose'

alias dock_con='sudo docker exec -it cfl-matt /bin/bash -c "sudo su - battagel"'

alias pypath='export PYTHONPATH=$PYTHONPATH:`pwd`'
. "$HOME/.cargo/env"
