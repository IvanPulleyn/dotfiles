# -*- mode: sh; -*-
##############################################################################

[ -z "$PS1" ] && return

export PATH=$HOME/rpx/sys/bin:$PATH
export PATH=$HOME/.cabal/bin:$PATH
export PATH=/usr/lib/postgresql/8.3/bin:$PATH

export MANPATH=$HOME/rpx/sys/man:$MANPATH

export HISTCONTROL=ignoreboth
export HISTFILESIZE=10000
export HISTSIZE=10000

export PGUSER=$USER
export PGDATABASE=accelerator_development
export PGDATA=$HOME/rpx/sys/var/pgdata
export PGHOST=/tmp

export PS1='\u@\h\$ '

export EDITOR=emacs
export VISUAL=$EDITOR

shopt -s histappend
shopt -s checkwinsize

function rmtmp
{
    find . -name \*~ -print0 | xargs -0 rm -f
}

##############################################################################
