#!/bin/bash
# Collection of general functions that I tend to use in scripts

function echon {
    # echo -n even if not available.
    echo "$*" | tr -d '\n'
}

function prompt {
    echo -n $@ " "
}

function repeat {
    # Repeat $1 $2 times
    for (( i=1 ; i<=$2; i++ )) ; do echon "$1"; done
    echo ""
}

# Function for translating yes/no into exit codes.
# YES : 0, NO : 1, other : 2
function yes_or_no {
    local yes=$(grep -iE "^y(es)?" <(echo $@))
    local no=$(grep -iE "^n(o)?" <(echo $@))

    if [ ! -z $yes ]; then
        return 0;
    elif [ ! -z $no ]; then
        return 1;
    else
        return 2;
    fi
}

# Useful function for displaying & acting on yes/no input from user.
# Usage: choose <default (y or n)> <prompt> <yes action> <no action>
function choose {
    local default="$1"
    local prompt="$2"
    local choice_yes="$3"
    local choice_no="$4"
    local answer

    read -p "$2" answer

    # If answer is blank, then default
    [ -z "$answer" ] && answer="$default"

    yes_or_no "$answer"
    answer="$?"

    echo "$answer"

    if [ "$answer" -eq 0 ]; then
        eval "$choice_yes"
        return "$?";
    elif [ "$answer" -eq 1 ]; then
        eval "$choice_no"
        return "$?";
    else
        return "$answer"
    fi
}
