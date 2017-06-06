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
HISTSIZE=100000
HISTFILESIZE=100000
export GTAGSLABEL=pygments
CONFIG_FILE="$HOME/dotfiles/bash/main.sh"
export EDITOR=vim

# Fancy GIT bash prompt (https://github.com/magicmonty/bash-git-prompt)
if [[ $unamestr == Darwin ]]; then
    if [ -f "/usr/local/opt/bash-git-prompt/share/gitprompt.sh" ]; then
        __GIT_PROMPT_DIR="/usr/local/opt/bash-git-prompt/share"
        source "/usr/local/opt/bash-git-prompt/share/gitprompt.sh"
    fi
fi

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


# Aliases
alias last-commit='git log --stat -n 1 && git log -p -n 1'
alias ls="$LS -CF --color=auto"
alias lsl="$LS -lhFA | less"
alias cd..='cd ..'
alias pop='cd -'
alias fhere='find . -name '
alias df="$DF -Tha --total"
alias sed="$SED" # Use GNU sed instead of BSD sed
alias du="$DU -ach | gsort -h | less"
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
