# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="minimal"

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
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git zsh-syntax-highlighting rails ruby bower bundler docker fasd git-flow git-extras source history-substring-search)

source $ZSH/oh-my-zsh.sh

# User configuration

export PATH=$HOME/bin:/usr/local/bin:$PATH
# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8
#
bindkey -v
export KEYTIMEOUT=1

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='vim'
else
    export EDITOR='vim'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

eval "$(fasd --init posix-alias zsh-hook zsh-wcomp zsh-ccomp )"
export PATH="$HOME/.rbenv/bin:$PATH"
if [[ `uname` == 'Darwin' ]]
then
    source /usr/local/bin/virtualenvwrapper_lazy.sh
else
    source /usr/bin/virtualenvwrapper_lazy.sh
fi


# zsh-substring
# source ~/.zsh-substring/zsh-history-substring-search.zsh

# bind k and j for VI mode
# bindkey -M vicmd 'k' history-substring-search-up
# bindkey -M vicmd 'j' history-substring-search-down


# Setup zsh-autosuggestions
#source ~/.zsh-autosuggestions/autosuggestions.zsh

# Enable autosuggestions automatically
#zle-line-init() {
#    zle autosuggest-start
#}
#zle -N zle-line-init

# use ctrl+t to toggle autosuggestions(hopefully this wont be needed as
# zsh-autosuggestions is designed to be unobtrusive)
#bindkey '^T' autosuggest-toggle

# Accept suggestions without leaving insert mode
#bindkey '^f' vi-forward-word
# # or
#bindkey '^f' vi-forward-blank-word
#
#fpath=(~/.zsh-completions/src/ $fpath)

alias v='f -e vim' # quick opening files with vim
alias m='f -e mplayer' # quick opening files with mplayer
alias o='a -e xdg-open' # quick opening files with xdg-open
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'
#export PATH="/hel/.bin:$PATH"
export PULSE_LATENCY_MSEC=60
# MPD daemon start (if no other user instance exists)
#[ ! -s ~/.config/mpd/pid  ] && mpd
#
export HAGARD_IP=162.243.207.120
export NDURNZ_IP=107.170.154.110

# Setup zsh-autosuggestions
#source /home/rscnt/.zsh-autosuggestions/autosuggestions.zsh
bindkey -e

# bind P and N for EMACS mode
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

#export PATH="$GOPATH/bin:$GOROOT/bin:~/Apps/android-studio/bin:$HOME/bin:$HOME/scripts:$HOME/.cask/bin:$HOME/.scripts:$PATH"
#source $HOME/.gvm/scripts/gvm
#export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"
export PATH=$PATH:/home/rscnt/.cask/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin:/home/rscnt/.local/bin:/home/rscnt/bin

export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

export GOPATH="$HOME/gocode"
export PATH="$PATH:$GOPATH/bin"
export GOROOT="$HOME/go"
export PATH="$PATH:$GOROOT/bin"

docker-machine-wrapper() {
    CONFIG_NAME="$1"
    shift
    docker $(docker-machine config "$CONFIG_NAME") $@
    
}
 
alias docker-mw="docker-machine-wrapper" 
fpath=($HOME/packages/zsh-completions/src/ $fpath)


show-swarm-token() {
    SWARM_FILE_NAME="$1"
    MACHINE_NAME="$2"
    echo $( cat "$SWARM_FILE_NAME"  | grep "$MACHINE_NAME" | tr ',' ' ' | awk '{print substr($1,0)}' )
}
eval SSH_AUTH_SOCK=/tmp/ssh-4SJK5qiGLwtn/agent.8342; export SSH_AUTH_SOCK;
SSH_AGENT_PID=8343; export SSH_AGENT_PID;
echo Agent pid 8343;
eval `dircolors $HOME/.dir_colors`
alias tmux='TERM=xterm-termite tmux'
alias tmux-r="TERM=xterm-termite tmux attach -t $(whoami)"
