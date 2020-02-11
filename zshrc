# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.

#ZSH_THEME="robbyrussell"
#ZSH_THEME="cypher"
ZSH_THEME="agnoster"

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
export PYENV_ROOT="$HOME/.pyenv"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)

plugins=(git archlinux git extract common-aliases git-extras autojump docker pyenv python ssh-agent)

# pour avoir l'agent-forwarding
zstyle :omz:plugins:ssh-agent agent-forwarding on

# c'est bizare de faire ca...
source $ZSH/oh-my-zsh.sh

#export MANPATH="/usr/local/man:$MANPATH"

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#     export EDITOR='leafpad'
# else
#     export EDITOR='nano'
# fi

# Compilation flags
export ARCHFLAGS="-arch x86_64"

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

# Calendar (broken)
alias today="emacsclient -t ~/agenda/today.org"
alias day="gcalcli agenda 7am 11:55pm"

# git alias
alias forcepull="git fetch --all && git reset --hard origin/master"
alias gs="git status"
alias gl="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"

# Wifi alias
alias connectwifi="nmtui-connect"

# invivo
alias iac_invivo="docker run -it -p 31444:31444 -v ~/.ssh/known_hosts:/home/ops/.ssh/known_hosts -v ~/working/invivo:/home/ops/invivo gitlab.tooling.invivodigitalfactory.com:5005/infra/terraform-wrapper/iac-wrapper:latest /bin/bash"


# Node & nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Invivo
export DOCKER_COMPOSE_SSH_AGENT_SOCKET=$SSH_AUTH_SOCK

# TEST Vterm inside emacs
vterm_prompt_begin() {
    printf "\e]51;C\e\\"
}
vterm_prompt_end() {
    printf "\e]51;A$(whoami)@$(hostname):$(pwd)\e\\";
}
PROMPT='%{$(vterm_prompt_begin)%}'$PROMPT'%{$(vterm_prompt_end)%}'

autoload -U add-zsh-hook
add-zsh-hook -Uz preexec(){printf "\e]51;B\e\\";}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='printf "\e]51;Evterm-clear-scrollback\e\\";tput clear'
fi
