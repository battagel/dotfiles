# Clone files into linux system.

# Remove all files from the cloned folder

# Install git

yum install git

# Install vim

yum install vim

# Source vim config

source .vimrc

# Install tmux 3.2a

yum install http://mirror.ghettoforge.org/distributions/gf/el/8/testing/x86_64/tmux-3.2a-3.gf.el8.x86_64.rpm

# Config requires perl

yum install perl

# Source the config files - First file may throw some errors

source .tmux.conf
source .tmux.conf.local

# Test load tmux

tmux
