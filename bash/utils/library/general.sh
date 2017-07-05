#!/bin/bash
# Collection of general functions that I tend to use in scripts

function echon {
    # echo -n even if not available.
    echo "$*" | tr -d '\n'
}

function prompt {
    echo -n $@ " "
}
