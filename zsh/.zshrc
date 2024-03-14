######################
######################
##    ZSH CONFIG    ##
######################
######################

if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

########################
# HOST SPECIFIC CONFIG #
########################

buk=false
cxo=false
if [[ "$HOSTNAME" == "cxo-vdt8-008" ]]; then
    cxo=true
    export HOME=/auto/homecxo.nas01/battagel
    cd ~
elif [[ "$HOSTNAME" == "buk-vdt8-054" ]]; then
    buk=true
    export HOME=/auto/homebuk.nas01/battagel
fi

#########################
# ENVIRONMENT VARIABLES #
#########################

export PATH=/opt/hpe/bin:auto/share/bin:~/.local/bin:~/.config/emacs/bin:$PATH
export TERM="xterm-256color"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

if [[ "$buk" == true || "$cxo" == true ]]; then
    export PATH=/opt/hpe/bin:/auto/homebuk.nas01/battagel/.local/bin:/auto/homebuk.nas01/battagel/.emacs.d/bin:/opt/hpe/bin:/auto/homebuk.nas01/battagel/.local/bin:/opt/hpe/bin:/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/auto/homebuk.nas01/battagel/bin:/auto/homebuk.nas01/battagel/.cargo/bin/navi:/auto/share/bin
    export HFSIM_SIMULATION_ROOT="/data/workspace/hfsim"
fi

#######
# ZSH #
#######

export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="powerlevel10k/powerlevel10k"
plugins=(git zsh-autosuggestions python F-Sy-H)
source $ZSH/oh-my-zsh.sh

###########
# ALIASES #
###########

alias dot="cd ~/repos/dotfiles"
alias ws="cd /data/workspace/battagel"
alias arcus="cd /data/workspace/battagel/repos/arcus/container"
alias podls="sudo podman image ls"
alias podclear="sudo podman image ls | grep swiss_npi | awk '{print \$3}' | sudo xargs -I {} sudo podman image rm {} || true"
