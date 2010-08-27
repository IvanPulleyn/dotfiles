# -*- mode: sh; -*-
##############################################################################

[ -z "$PS1" ] && return

export GIT_AUTHOR_IDENT="Ivan Pulleyn"
export GIT_COMMITTER_IDENT="Ivan Pulleyn"

export PATH=$HOME/rpx/sys/bin:$PATH
export PATH=$HOME/opt/bin:$PATH
export PATH=$HOME/.cabal/bin:$PATH
export PATH=/usr/lib/postgresql/8.4/bin:$PATH
export PATH=/opt/ghc/6.10.4/bin:$PATH
export PATH=$HOME/android-sdk-linux_86/tools:$PATH

export MANPATH=$HOME/rpx/sys/man:$MANPATH

export HISTCONTROL=ignoreboth
export HISTFILESIZE=1000000
export HISTSIZE=1000000

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

alias emacs='emacs -nw'

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

function sfs
{
    USAGE="Usage: sfs [on|off]"

    if [ -z "$1" -o -n "$2" ]; then
	echo "$USAGE"
	return 1
    elif [ "$1" != "on" -a "$1" != "off" ]; then
	echo "$USAGE"
	return 1
    fi

    SFS_HOSTS="`cat ~/.sfs_hosts 2>/dev/null`"

    if [ -z "$SFS_HOSTS" ]; then
	echo "no remote hosts configured"
	return 0
    fi

    MY_UID=`id | sed -e's/^uid=\([0-9]*\).*/\1/'`
    MY_GID=`id | sed -e's/.* gid=\([0-9]*\).*/\1/'`

    for x in $SFS_HOSTS
    do
	if [ "$1" = "on" ]; then
	    mount | grep "^$x" >/dev/null
	    if [ "$?" = "0" ]; then
		echo "$x already mounted"
	    else
		echo "mounting $x"
		mkdir -p $HOME/mnt/$x
		sshfs -C -o idmap=user -o uid=$MY_UID -o gid=$MY_GID -o reconnect $x: $HOME/mnt/$x
	    fi
	else
	    mount | grep "^$x" >/dev/null
	    if [ "$?" = "0" ]; then
		echo "unmounting $x"
		fusermount -u $HOME/mnt/$x
	    else
		echo "$x not mounted"
	    fi
	fi
    done
}

##############################################################################
