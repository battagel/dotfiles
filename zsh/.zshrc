return
#################################
#+-----------------------------+#
#|         Zsh Config          |#
#+-----------------------------+#
#################################

if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="powerlevel10k/powerlevel10k"
plugins=(git zsh-autosuggestions python F-Sy-H zsh-completions)
source $ZSH/oh-my-zsh.sh

# Use brew install bash instead
export PATH="/opt/homebrew/bin:$PATH"

# Keybindings
bindkey -e # Use emacs keybindings
bindkey '^p' history-search-backward
bindkey '^n' history-search-forward

# History
HISTSIZE=5000
SAVEHIST=$HISTSIZE
HISTDUP=erase
setopt appendhistory
setopt sharehistory
setopt hist_ignore_space 
setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_find_no_dups 

# Completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' # Case-insensitive completion
zstyle ':completion:*' list-colors '${(s.:.)LS_COLORS}' # Add colours

# Fzf
source <(fzf --zsh)
# Consider zoxide and fzf-tab

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


#########
# EMACS #
#########

vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}

find_file() {
    vterm_cmd find-file "$(realpath "${@:-.}")"
}

say() {
    vterm_cmd message "%s" "$*"
}

open_file_below() {
    vterm_cmd find-file-below "$(realpath "${@:-.}")"
}

###########
# ALIASES #
###########

alias ls="ls --color"

# Explore Directories
explore_dir() { # List files in a dir and open them in vim
    dir=$1
    depth=$2
    local selected_file
    selected_file=$(find "$dir" -maxdepth "$depth" -type f -not -path '*/.git/*' -not -name '*.DS_Store*' | sort | sed "s|$dir||"| fzf)
    [[ -n $selected_file ]] && vim "$dir/$selected_file"
}
alias vimdot="explore_dir ~/repos/dotfiles 3"
alias vimf="explore_dir . 6"

# Fuzzy Find
alias fman="compgen -c | fzf | xargs man" # Man pages for all commands
alias falias="alias | fzf | xargs eval "

# Files
alias du -ah . | sort -hr | head -n 10 # Largest files in the current directory

# Moving
alias dot="cd ~/repos/dotfiles" # Where we are now
alias ws="cd /data/workspace/battagel" # Linux Workspace
alias arcus="cd /data/workspace/battagel/repos/arcus/container" # Arcus container workspace

# Git - Zsh already comes with a bunch of these
alias gs="git switch -"

# Podman, Docker, k8s
alias podls="sudo podman image ls"
alias podclear="podls | grep swiss_npi | awk '{print \$3}' | sudo xargs -I {} sudo podman image rm {} || true"
#alias kpd="kubectl get pods -A --no-header | fzf | awk '{print $2, $1}' | xargs -n 2 sh -c 'kubectl describe pod $0 -n $1" # Describe a pod
#alias kpl="kubectl get pods -A --no-header | fzf | awk '{print $2, $1}' | xargs -n 2 sh -c 'kubectl exec -it $0 -n $1 -- /bin/bash'" # Open a shell in a pod
alias kgp='kubectl get pods --all-namespaces'
alias kgs='kubectl get services --all-namespaces'

function kLogsPreviewAllContainers() {
    kubectl get pods --all-namespaces -o jsonpath='{range .items[*]}{.metadata.namespace} {.metadata.name}{"\n"}' | fzf --preview="kubectl logs {2} --namespace {1} --all-containers" --preview-window=up:80%
}
alias klogsp=kLogsPreviewAllContainers

function kLogsAllContainers() {
    kubectl get pods --all-namespaces -o jsonpath='{range .items[*]}{.metadata.namespace} {.metadata.name}{"\n"}' | fzf | read -r namespace pod
    kubectl logs $pod --namespace $namespace --all-containers -f
}
alias klogs=kLogsAllContainers

function kLogsAllContainersAllPodsInDeployment() {
    kubectl get deployments --all-namespaces -o jsonpath='{range .items[*]}{.metadata.namespace} {.metadata.name}{"\n"}' | fzf | read -r namespace deployment
    kubectl logs -f deployment/$deployment --namespace $namespace -f
}
alias klogsdeployment=kLogsAllContainersAllPodsInDeployment

function kLogsContainer() {
    # The first argument to this function should be the container name
    # kubectl get pods -o name | fzf --preview="kubectl logs {} --container $1 | tail -20" --preview-window=up:80%
    local container
    container=$1
    if [[ ! -z "${container// }" ]]; then
        kubectl get pods --all-namespaces -o jsonpath='{range .items[*]}{.metadata.namespace} {.metadata.name}{"\n"}' | fzf --preview="echo kubectl logs {2} --namespace {1} --container $container" --preview-window=up:80% --preview-label="Logs for container $1"
    else
        print "Usage: kLogsContainer <container name>"
    fi
}

function kexSh() {
    local containers
    local container
    kubectl get pods --all-namespaces -o jsonpath='{range .items[*]}{.metadata.namespace}{"\t"}{.metadata.name}{"\n"}' | fzf | read -r namespace pod
    containers=$(kubectl get pod -n $namespace $pod -o jsonpath='{.spec.containers[*].name}')
    container=$(echo ${containers/ /\\n} | fzf)
    kubectl exec -n $namespace --stdin --tty $pod --container $containers -- /bin/sh
}

function kexBash() {
    local containers
    local container
    kubectl get pods --all-namespaces -o jsonpath='{range .items[*]}{.metadata.namespace}{"\t"}{.metadata.name}{"\n"}' | fzf | read -r namespace pod
    containers=$(kubectl get pod -n $namespace $pod -o jsonpath='{.spec.containers[*].name}')
    container=$(echo ${containers/ /\\n} | fzf)
    kubectl exec -n $namespace --stdin --tty $pod --container $container -- /bin/bash
}

function kd() {
    kubectl get $1 --all-namespaces -o jsonpath='{range .items[*]}{.metadata.namespace}{"\t"}{.metadata.name}{"\n"}' | fzf --preview="echo '{}' | xargs kubectl describe $1 -n" | xargs kubectl describe $1 -n
}

function kdelete() {
    kubectl get $1 --all-namespaces -o jsonpath='{range .items[*]}{.metadata.namespace}{"\t"}{.metadata.name}{"\n"}' | fzf | xargs kubectl delete $1 -n
}

alias kdpod='kd pod'
alias kdelpod='kdelete pod'
alias kdservice='kd service'
alias kdelservice='kdelete service'
alias kg='kubectl get '
export PATH="$HOME/.cargo/bin:$PATH"
