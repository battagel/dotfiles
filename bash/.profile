 # If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

########################
# HOST SPECIFIC CONFIG #
########################

if [[ "$HOSTNAME" == "cxo-vdt8-008" ]]; then
    export HOME=/auto/homecxo.nas01/battagel
    export PATH=/opt/hpe/bin:/auto/homebuk.nas01/battagel/.local/bin:/auto/homebuk.nas01/battagel/.emacs.d/bin:/opt/hpe/bin:/auto/homebuk.nas01/battagel/.local/bin:/opt/hpe/bin:/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/auto/homebuk.nas01/battagel/bin:/auto/homebuk.nas01/battagel/.cargo/bin/navi:/auto/share/bin
    export HFSIM_SIMULATION_ROOT="/data/workspace/hfsim"
elif [[ "$HOSTNAME" == "buk-vdt8-054" ]]; then
    export HOME=/auto/homebuk.nas01/battagel
    export PATH=/opt/hpe/bin:/auto/homebuk.nas01/battagel/.local/bin:/auto/homebuk.nas01/battagel/.emacs.d/bin:/opt/hpe/bin:/auto/homebuk.nas01/battagel/.local/bin:/opt/hpe/bin:/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/auto/homebuk.nas01/battagel/bin:/auto/homebuk.nas01/battagel/.cargo/bin/navi:/auto/share/bin
    export HFSIM_SIMULATION_ROOT="/data/workspace/hfsim"
fi

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
alias ll="ls -al"
alias cl="clear"
## Work
alias arcus="cd /data/workspace/battagel/repos/arcus/container"
alias podls="sudo podman image ls"
alias podclear="sudo podman image ls | grep swiss_npi | awk '{print $3}' | sudo xargs -I {} sudo podman image rm {} || true"
alias src="source ~/.profile"
