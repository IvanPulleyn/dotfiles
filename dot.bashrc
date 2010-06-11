# -*- mode: sh; -*-
##############################################################################

[ -z "$PS1" ] && return

export PATH=$HOME/rpx/sys/bin:$PATH
export PATH=$HOME/.cabal/bin:$PATH
export PATH=/usr/lib/postgresql/8.4/bin:$PATH
export PATH=/opt/ghc/6.10.4/bin:$PATH

export MANPATH=$HOME/rpx/sys/man:$MANPATH

export HISTCONTROL=ignoreboth
export HISTFILESIZE=10000
export HISTSIZE=10000

if [ "$HOSTNAME" != "destro" ]; then
    export PGUSER=$USER
    export PGDATABASE=accelerator_development
    export PGDATA=$HOME/rpx/sys/var/pgdata
    export PGHOST=/tmp
fi

export PS1='\u@\h\$ '

export EDITOR=emacs
export VISUAL=$EDITOR

if [ "$EMACS" = "t" ]; then
    export PAGER=cat
fi

shopt -s histappend
shopt -s checkwinsize

function rmtmp
{
    find . -name \*~ -print0 | xargs -0 rm -f
}

function now
{
    date +%Y%m%d%H%M%S
}

# buzz
function bzlogin
{
    java -cp $HOME/dotfiles/oacurl-1.0.0.jar com.google.oacurl.Login -buzz
}

function bzcons
{
    java -cp $HOME/dotfiles/oacurl-1.0.0.jar com.google.oacurl.Fetch https://www.googleapis.com/buzz/v1/activities/@me/@consumption?prettyprint=true
}

##############################################################################
