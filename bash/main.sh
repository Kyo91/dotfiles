#!/bin/bash
# A collection of general aliases/functions that I keep synced between different *nix files

# Linux vs OSX differences
PLATFORM='linux'
SED='sed'
LS='ls'
DF='df'
DU='du'
TAR='tar'

unamestr=`uname`
if [[ $unamestr == Darwin ]]; then
    PLATFORM='osx'
    SED="g$SED"
    LS="g$LS"
    DF="g$DF"
    DU="g$DU"
    TAR="g$TAR"
fi

# Settings
shopt -s histappend # Append to history file, don't overwrite it
HISTSIZE=100000
HISTFILESIZE=100000
HISTCONTROL=ignoreboth # Bash history ignores duplicate lines
export GTAGSLABEL=pygments
CONFIG_FILE="$HOME/dotfiles/bash/main.sh"
PRIVATE_CONFIG_FILE="$HOME/dotfiles/bash/private.sh"
export EDITOR=vim
export PAGER=less
export AUTOSSH_PORT=0 # autossh
export SPARK_HOME="$HOME/spark"

# Common Lisp Roswell Binaries
if [ -d "$HOME/.roswell/bin" ]; then
        export PATH=$PATH:"$HOME/.roswell/bin"
fi

# Fancy GIT bash prompt (https://github.com/magicmonty/bash-git-prompt)
if [[ $unamestr == Darwin ]]; then
    if [ -f "/usr/local/opt/bash-git-prompt/share/gitprompt.sh" ]; then
        __GIT_PROMPT_DIR="/usr/local/opt/bash-git-prompt/share"
        source "/usr/local/opt/bash-git-prompt/share/gitprompt.sh"
        export GIT_PROMPT_THEME=Solarized
    fi
fi

# Lambda prompt
export PS1="${PS1%??} λ "

# Functions
function body {
    local lines=$(wc -l "$1" | awk '{print $1}')
    local values=$(( ${lines} - 1 ))
    tail -n "${values}" "$1"
}

# Create directories and cd into first one
function mcd {
    mkdir -pv "$@"
    pushd "$1"
}

function gb {
    if [[ -f "$PWD/gradlew" ]]; then
        $PWD/gradlew build -x test
    else
        gradle build -x test
    fi
}

function gt {
    if [[ -f "$PWD/gradlew" ]]; then
        $PWD/gradlew test
    else
        gradle test
    fi
}

function als {
    echo "alias" $1'="'$2'"' >> $CONFIG_FILE
    source $CONFIG_FILE
}

function alsp {
    echo "alias" $1'="'$2'"' >> $PRIVATE_CONFIG_FILE
    source $PRIVATE_CONFIG_FILE
}

function load {
    emacsclient -n $@
    echo "Loaded into Emacs:"
    ls -l $@ | awk '{print $9}'
}

function agload {
    targets=$(ag -l -- $1 $2)
    load $targets
}

function ssht {
        if [[ -z $@ ]]; then
                echo "usage: $0 <hostname> [params]"
                exit 1;
        fi
        $(which ssh) $@ -t "sh -c 'tmux a || tmux'";
}

function du {
        $DU -ach "$@" | gsort -h | less
}

function edit {
        "$EDITOR" $(which "$1")
}

function gfind { # Find file from root, pipe errors (usually permission errors) to /dev/null
        find / -name "$1" 2>/dev/null
}


# Aliases
alias last-commit='git log --stat -n 1 && git log -p -n 1'
alias ls="$LS -CF --color=auto"
alias lsl="$LS -lhFA | less"
alias cd..='cd ..'
alias pop='cd -'
alias fhere='find . -name '
alias df="$DF -Tha --total"
alias sed="$SED" # Use GNU sed instead of BSD sed
alias grep="grep -E"
alias psg="ps aux | grep -v grep | grep -i -e VSZ -e" # Filters ps to match supplied grep regex term. (Ignores grep call)
alias mkdir="mkdir -pv" # Auto create parents and show created dirs
alias curl='curl -L -C - ' # Auto follow redirect & continue downloads
alias gs="git status"
alias ga='git add -u'
alias gw='./gradlew'
alias addrc="$EDITOR $CONFIG_FILE"
alias refresh=". ~/.bashrc"
# Enable/disable upstream branch on git prompt
alias upshow="GIT_PROMPT_SHOW_UPSTREAM=1"
alias uphide="GIT_PROMPT_SHOW_UPSTREAM="

# Immediately open file(s) in existing emacs frame
alias ec='emacsclient -n'
alias ecrc="emacsclient -n $CONFIG_FILE"
alias sshconfig="$EDITOR ~/.ssh/config"

# sftp compression
alias sftp='sftp -C'

if [[ $PLATFORM = 'osx' ]]; then
    alias copy-window='screencapture -cs'
fi

alias fixup="git add -u && git fix"
alias pyspark="PYSPARK_DRIVER_PYTHON=bpython $SPARK_HOME/bin/pyspark"
alias spark-submit="$SPARK_HOME/bin/spark-submit --executor-memory 4G"
