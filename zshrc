# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"
#ZSH_THEME="cypher"


# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to disable command auto-correction.
# DISABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="dd.mm.yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder


# User configuration
export PATH="/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/bin/core_perl:/home/tlu/.local/bin/:"
export PYENV_ROOT="$HOME/.pyenv"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)

plugins=(git archlinux git extract common-aliases git-extras autojump zsh-nvm docker)

# c'est bizare de faire ca...
source $ZSH/oh-my-zsh.sh


export MANPATH="/usr/local/man:$MANPATH"

# Preferred editor for local and remote sessions
 if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='leafpad'
 else
   export EDITOR='nano'
 fi

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/dsa_id"

# My Export
export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR="nano"
export DIFFEDITCMD="emacs -diff"
export DICTIONARY="fr-FR"
export BROWSER="firefox"
export PYTEST_ADDOPTS="--color=yes"
export CHROME_BIN=/usr/bin/chromium

# [TEST] pour steam-manjaro
export LIBGL_DRI3_DISABLE=1 steam

# My Local
export LC_TIME="fr_FR.UTF-8"
export LANG="fr_FR.UTF-8"

# Alias
#alias emacs="emacs -nw"

alias e="emacsclient -t"
alias se="sudo emacs -nw"

alias q="exit"
alias l="ls"
alias eog="gpicview"
alias dasize="du -a * | sort -nr | head -10"
alias daclean="sudo pacman -Sc; sudo pacman -Rns $(pacman -Qtdq);"
alias batty="acpi"
alias lycos="grep -nr --exclude-dir={downloads,venv,bower_components,misc,.vendors,.idea,.git,.cache,__pycache__,.tmp,libs,dist,node_modules,.vagrant,htmlcov,cov_html} --exclude=\*.{pyc,~,#,log,coverage}"
alias z="xlock;"
alias dodo="systemctl suspend"
alias yasu="yaourt -Syu"
alias agenda="emacsclient -t /home/tlu/Dropbox/agenda/agenda.org"
alias todo="emacsclient -t /home/tlu/Dropbox/agenda/today.org"
alias today="python2 /usr/bin/gcalcli agenda 7am 11:55pm"
alias addcal="python2 /usr/bin/gcalcli add --calendar 'Thomas L.'"
alias week="python2 /usr/bin/gcalcli agenda"

alias forcepull="git fetch --all && git reset --hard origin/master"


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
