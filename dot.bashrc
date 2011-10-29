# -*- mode: sh; -*-
##############################################################################

[ -z "$PS1" ] && return

export PATH=$HOME/dotfiles:$PATH
export PATH=$HOME/local/bin:$PATH
export PATH=$HOME/local/bin:$PATH
export PATH=$HOME/local/bin:$PATH
export PATH=$HOME/rpx/sys/bin:$PATH
export PATH=$HOME/opt/bin:$PATH
export PATH=$HOME/android-sdk-linux_x86/tools:$PATH
export PATH=$PATH:/sbin
export PATH=$PATH:/usr/sbin
export PATH=$PATH:/usr/local/sbin

export MANPATH=$HOME/rpx/sys/man:$MANPATH

export HISTCONTROL=ignoreboth
export HISTFILESIZE=1000000
export HISTSIZE=1000000

if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
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

function fanmax
{
    sudo rmmod thinkpad_acpi
    sudo modprobe thinkpad_acpi fan_control=1
    sudo bash -c 'sudo echo "level disengaged" > /proc/acpi/ibm/fan'
}

function rmtmp
{
    find . -name \*~ -print0 | xargs -0 rm -f
}

function now
{
    date +%Y%m%d%H%M%S
}

function ctime
{
    if [ -z "$1" ]; then
	echo "Usage: ctime timestamp";
	return 1
    fi
    perl -e"use POSIX; print ctime($1);"
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
# local stuff
if [ -f $HOME/.bashrc.local ]; then
    . $HOME/.bashrc.local
fi
