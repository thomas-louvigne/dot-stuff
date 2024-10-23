# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:/home/thomas-luquet/.cargo/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="jaischeema"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
HIST_STAMPS="dd/MM/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git emacs ssh-agent zoxide extract common-aliases git-extras kate man zsh-syntax-highlighting battery bgnotify web-search battery gradle httpie ssh zsh-navigation-tools)

source $ZSH/oh-my-zsh.sh

# User configuration

#export MANPATH="/usr/local/man:$MANPATH"


# You may need to manually set your language environment
# export LANG=en_US.UTF-8

#export ALTERNATE_EDITOR="emacsclient -t"
export DIFFEDITCMD="emacs -diff"
export DICTIONARY="fr-FR"
export BROWSER="firefox"
export PYTEST_ADDOPTS="--color=yes"

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='emacsclient -t'
else
  export EDITOR='nano'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"


# My Local
export LC_TIME="fr_FR.UTF-8"
export LANG="fr_FR.UTF-8"

# Alias
alias e="emacsclient -t"
alias bl="emacsclient -t ~/Nextcloud/backlog.org"
alias se="sudo emacs -nw"
alias q="exit"
alias l="exa"
alias ls="exa"
alias eog="gpicview"
alias dasize="du -ah * | sort -nr | head -10"
alias daclean="sudo pacman -Sc; sudo pacman -Rns $(pacman -Qtdq);"
alias batty="acpi"
alias lycos="grep -nr --exclude-dir={downloads,venv,bower_components,misc,.vendors,.idea,.git,.cache,__pycache__,.tmp,libs,dist,node_modules,.vagrant,htmlcov,cov_html} --exclude=\*.{pyc,~,#,log,coverage,css,scss}"
alias zlock="xlock -nolock;"
alias dodo="systemctl suspend"
alias money="boobank list -f simple --select balance --no-keys --condition \"id=00040474642@boursorama\""
alias forcepull="git fetch --all && git reset --hard origin/master"
alias gs="git status"
alias gl="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias connectwifi="nmtui-connect"
alias tree2="exa --tree --level=2"
alias cd=z #Zoxixde
alias bc=tcalc
alias cat=bat
bat -p Nextcloud/backlog.org --line-range 0:10

# [TEST] Qu'est ce que c'est ? Surement un truc de vterm pour emacs
if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
        source /etc/profile.d/vte.sh
fi
