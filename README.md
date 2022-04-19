# Here are the steps you need to follow to use this setup

Clone files into linux system. 
Move all config files (no git files but including .vim folder) to the ~ directory

Install git
```
yum install git
```

Install vim and other dependancies
```
yum install vim
yum install cmake
yum install python3
```

Coc.nvim requires some special attention
First we need to install nodejs 12 for coc.nvim to work
https://computingforgeeks.com/how-to-install-nodejs-on-centos-fedora/

```
sudo yum -y update
curl -sL https://rpm.nodesource.com/setup_12.x | sudo bash -
sudo yum clean all && sudo yum makecache fast
sudo yum install -y gcc-c++ make
sudo yum install -y nodejs            <--- Make sure to uninstall previous node versions
node -v
```

If the following steps dont work try this:
https://www.codegrepper.com/code-examples/shell/centos+7+install+nodejs+12

Go into vimrc file
```
vim .vimrc
```

Source vim config
```
:source .vimrc
```

Install all plugins
```
:PluginInstall
```

!NOTE! Vundle will fail as it is a submodule for this git

Need to finish installing coc.nvim dependancies
```
cd .vim/bundle/coc.nvim
yarn install
```

Install coc.nvim python language support in vim
```
:CocInstall coc-pyright
```

Install tmux 3.2a
```
yum install http://mirror.ghettoforge.org/distributions/gf/el/8/testing/x86_64/tmux-3.2a-3.gf.el8.x86_64.rpm
```

Config requires perl
```
yum install perl
```

Source the config files - First file may throw some errors
```
source .tmux.conf
source .tmux.conf.local
```

Test load tmux
```
tmux
```

Any errors regarding ^M found in files run this:
```
yum install dos2unix
dos2unix <file>
```

# Features of this config package
- NERDTree
- Linting
- Git Integration
- Autocomplete
- TagBar
- Fuzzy Finder
- Pretty colours
- Vim TMUX integration
- Easily edited config files using hotkeys