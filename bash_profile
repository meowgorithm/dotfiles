#!/usr/bin/env bash

# Pull in profile stuff
[ -f $HOME/.profile ] && . $HOME/.profile

# Pull in bashrc
[ -f $HOME/.bashrc ] && . $HOME/.bashrc

# Use (some of) bashrc for non-interactive shells
export BASH_ENV="$HOME/.bashrc"
