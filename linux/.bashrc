#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

alias d='export DISPLAY=:0'

if [[ -z $DISPLAY && $(tty) = /dev/tty1 ]]; then
    exec startx
fi
