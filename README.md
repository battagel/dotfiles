# Here are the steps you need to follow to use this setup

Clone files into linux system using the following:
```
git clone --resursive-submodules -j8 git@github.com:battagel/config_files.git
```
Move all config files (no git files) to the ~ directory

## Oh-My-Zsh - Mac only
Install ohmyzsh
```
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
```
Move my custom theme into .oh-my-zsh/themes
```
mv matthewbattagel.zsh-theme ~/.oh-my-zsh/themes
```
Include the theme in the newly created .zshrc file
```
ZSH_THEME="matthewbattagel"
```

## Vim
Install vim and other dependancies
```
yum install vim
yum install cmake
yum install python3
```

Double check if vim is version 8.2. If not manually install using these
https://phoenixnap.com/kb/how-to-install-vim-centos-7 - For installing dependencies
https://gist.github.com/yevrah/21cdccc1dc65efd2a4712781815159fb - For compiling

Coc.nvim requires some special attention
First we need to install nodejs 12 for coc.nvim to work
Linux:
```
sudo yum -y update
curl -sL https://rpm.nodesource.com/setup_12.x | sudo bash -
sudo yum clean all && sudo yum makecache fast
sudo yum install -y gcc-c++ make
sudo dnf module enable nodejs:12
sudo dnf install nodejs
sudo node -v
```

https://computingforgeeks.com/how-to-install-nodejs-on-centos-fedora/
If the following steps dont work try this:
https://www.codegrepper.com/code-examples/shell/centos+7+install+nodejs+12

Or for Mac:
```
brew install nodejs
```

Go into vimrc file
```
vim .vimrc
```

Source vim config
```
:source .vimrc
```

Install all vim plugins
```
:PluginInstall
```

!NOTE! Vundle will fail as it is a submodule for this git

Need to finish installing coc.nvim dependancies
```
cd .vim/bundle/coc.nvim
npm install
OR
yarn install
```

## Dont forget to install the coc language servers in vim - This should be done automatically when you open vim!
Double check these have installed
```
:CocInstall coc-tsserve
:CocInstall coc-pyright
:CocInstall coc-prettier
:CocInstall coc-lists
```

Type :CocConfig and add these lines
```
{
  "coc.preferences.formatOnSaveFiletypes": [
    "typescript",
    "typescriptreact",
    "python"
   ]
}
```
You need to use a specific font to enable dev icons
https://github.com/ryanoasis/nerd-fonts
Download .zip
Run either the .sh or the .ps1 file with the specific font as a param
```
./install.ps1 Hack
./install.sh Hack
```
Apply the specific font in the terminal. Reload terminal app.

## TMUX
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

Install Tmux plugins from inside tmux
```
<leader>+r
```

Any errors regarding ^M found in files run this:
```
yum install dos2unix
dos2unix <file>
```

# All done!!

# Features of this config package
- NERDTree
- NERDCommenter
- Linting
- Git Integration
- Autocomplete
- TagBar
- Fuzzy Finder
- Pretty colours
- Vim TMUX integration
- Easily edited config files using hotkeys
