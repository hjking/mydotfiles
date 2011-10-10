
#-------------------------------------------------------------
# Source global definitions (if any)
#-------------------------------------------------------------

#if [ -f /etc/bashrc ]
#then
#    . /etc/bashrc   # --> Read /etc/bashrc, if present.
#fi

#-------------------------------------------------------------
# Variables
#-------------------------------------------------------------
export LANG=en_US.utf8
export HISTCONTROL=ignoredups
export HISTTIMEFORMAT="%H:%M > "
export HISTIGNORE="&:bg:fg:ll:h"
export HISTSIZE=100
export HISTFILESIZE=200
export HOSTFILE=$HOME/.hosts    # Put list of remote hosts in ~/.hosts ...
export EDITOR=vim
#export PATH=$PATH:$HOME/bin:$HOME/bin/bin:/usr/bin:/usr/local/bin:/bin:.
export PATH=$PATH:$HOME/bin:$HOME/bin/bin:$HOME/bin/mussh:/usr/bin:/bin:.
export PAGER='less -Em'
export LESSCHARSET='latin1'
export LESSOPEN='|/usr/bin/lesspipe.sh %s 2>&-'
   # Use this if lesspipe.sh exists
export LESS='-i -N -w  -z-4 -g -e -M -X -F -R -P%t?f%f \
:stdin .?pb%pb\%:?lbLine %lb:?bbByte %bb:-...'
export MAILCHECK=0
export TIMEFORMAT=$'\nreal %3R\tuser %3U\tsys %3S\tpcpu %P\n'
export NOCOLOR=1
export PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}\007"; echo -en "\033[0;35mBash\033[36m["`date +%F\ %T`"] "; echo -e "\033[4;37m"`pwd`"\033[0m"'
#export PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}\007"'
# use vim as a man-page viewer
export MANPAGER="col -b | view -c 'set ft=man nomod nolist' -"

#-------------------------------------------------------------
# Automatic setting of $DISPLAY (if not set already).
# This works for linux - your mileage may vary. ...
#-------------------------------------------------------------

function get_xserver
{
    case $TERM in
      xterm )
          XSERVER=$(who am i | awk '{print $NF}' | tr -d ')''(' )
          # Ane-Pieter Wieringa suggests the following alternative:
          # I_AM=$(who am i)
          # SERVER=${I_AM#*(}
          # SERVER=${SERVER%*)}
          XSERVER=${XSERVER%%:*}
          ;;
      aterm | rxvt)
        # Find some code that works here. ...
          ;;
    esac
}

if [ -z ${DISPLAY:=""} ]; then
    get_xserver
    if [[ -z ${XSERVER}  || ${XSERVER} == $(hostname) || \
      ${XSERVER} == "unix" ]]; then
        DISPLAY=":0.0"          # Display on local host.
    else
        DISPLAY=${XSERVER}:0.0  # Display on remote host.
    fi
fi

export DISPLAY

#-------------------------------------------------------------
# Some settings
#-------------------------------------------------------------

umask 077
OS=$(uname)
ulimit -S -c 0      # Don't want any coredumps
set -o notify       # notify when jobs running background terminated
set -o noclobber    # prevents catting over file
set -o ignoreeof    # can not C-d out of shell
set -o nounset      # attempt to use undefined variable outputs error message and forces exit
#set -o xtrace      # Useful for debuging.


# Enable options:
shopt -s cdspell        # auto fix cd / spelling mistakes
shopt -s cdable_vars
shopt -s checkhash
#shopt -s checkjobs
shopt -s checkwinsize
shopt -s cmdhist
#shopt -s dirspell
shopt -s extglob        # Necessary for programmable completion.
shopt -s histappend
shopt -s histreedit
shopt -s histverify
shopt -s no_empty_cmd_completion
shopt -s nocaseglob
shopt -s sourcepath

# Disable options:
shopt -u mailwarn

#-------------------------------------------------------------
# Greeting, motd etc...
#-------------------------------------------------------------

fg_red='\e[0;31m'
fg_RED='\e[1;31m'
fg_green='\e[0;32m'
fg_GREEN='\e[1;32m'
fg_yellow='\e[0;33m'
fg_YELLOW='\e[1;33m'
fg_blue='\e[0;34m'
fg_BLUE='\e[1;34m'
fg_purple='\e[0;35m'
fg_PURPLE='\e[1;35m'
fg_cyan='\e[0;36m'
fg_CYAN='\e[1;36m'
fg_white='\e[0;37m'
fg_WHITE='\e[1;37m'
fg_YELLOW_bg_RED='\e[1;33;41m'
NC='\e[0m'              # No Color

user_number=`who -q | sed -n "1p" | xargs -n1 | sort | uniq | wc -l`
user_list=`who -q | sed -n "1p" | xargs -n1 | sort | uniq`

# Looks best on a terminal with black background.....
echo -e "${fg_CYAN}This is BASH ${fg_RED}${BASH_VERSION%.*}\
${fg_CYAN} - DISPLAY on ${fg_RED}$DISPLAY${NC}"
echo -e "There are ${fg_YELLOW}$user_number${NC} users logging to ${fg_GREEN}$(hostname)${NC}"
echo -e "$user_list \n"
#date
# if [ -x /usr/games/fortune ]; then
#     /usr/games/fortune -s     # Makes our day a bit more fun.... :-)
# fi

function _exit        # Function to run upon exit of shell.
{
    echo -e "${fg_RED}Good Bye!${NC}"
}
trap _exit EXIT


#-------------------------------------------------------------
# Shell Prompt
#-------------------------------------------------------------

if [[ "${DISPLAY%%:0*}" != "" ]]; then
    HILIT=${fg_red}   # remote machine: prompt will be partly red
else
    HILIT=${fg_cyan}  # local machine: prompt will be partly cyan
fi

#  --> Replace instances of \W with \w in prompt functions below
#+ --> to get display of full path name.

function fastprompt
{
    case $TERM in
        *term* | rxvt )
#            PS1="${HILIT}[\h]$NC \W > \[\033]0;\${TERM} [\u@\h] \w\007\]" ;;
#            PS1="${fg_YELLOW_bg_RED}[\h]$NC>>${fg_green} \[\033]0;\${TERM} [\u@\h] \w\007\]" ;;
#            PS1="${fg_purple}Bash${fg_YELLOW_bg_RED}[\h]$NC>>${fg_green} "
#            PS1="${fg_YELLOW_bg_RED}[\u]${fg_purple}\#$NC>>${fg_green} "
            PS1="${fg_YELLOW_bg_RED}[\h]${fg_purple}\#$NC>>${fg_green} "
            # TITLEBAR='\[\e]0;\u@\h:\w\a\]'
            # PS1="${TITLEBAR}${PS1}"
            ;;
        linux )
#            PS1="${fg_YELLOW_bg_RED}[\u]$NC >> "
            PS1="${fg_YELLOW_bg_RED}[\h]$NC >> "
#           PS1="[\A][\u@\H:\w]\$[\#]>"
            ;;
        screen*)
            # Use path as title
            PATHTITLE='\[\ek\W\e\\\]'
            # Use program name as title
            PROGRAMTITLE='\[\ek\e\\\]'
            PS1="${PROGRAMTITLE}${PATHTITLE}${PS1}"
            ;;
        *)
            PS1="[\u] > "
            ;;
    esac
}


_powerprompt()
{
    LOAD=$(uptime|sed -e "s/.*: \([^,]*\).*/\1/" -e "s/ //g")
}

function powerprompt
{
    PROMPT_COMMAND=_powerprompt
    case $TERM in
        *term | rxvt  )
            PS1="${HILIT}[\A - \$LOAD]$NC\n[\u@\h \#] \W > \[\033]0;\${TERM} [\u@\h] \w\007\]"
            ;;
        linux )
            PS1="${HILIT}[\A - \$LOAD]$NC\n[\u@\h \#] \W > "
            ;;
        * )
            PS1="[\A - \$LOAD]\n[\u@\h \#] \W > "
            ;;
    esac
}

#powerprompt     # This is the default prompt -- might be slow.
                # If too slow, use fastprompt instead. ...
fastprompt

#===============================================================
#
# ALIASES AND FUNCTIONS
#
# Arguably, some functions defined here are quite big.
# If you want to make this file smaller, these functions can
# be converted into scripts and removed from here.
#
#===============================================================

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
# -> Prevents accidentally clobbering files.
alias md='mkdir -p'
alias his='history'
alias j='jobs -l'
#alias which='type -a'
alias ..='cd ..'
alias ...='cd ../..'
alias cd.='cd ..'
alias cd..='cd ../..'
alias path='echo -e ${PATH//:/\\n}'
alias libpath='echo -e ${LD_LIBRARY_PATH//:/\\n}'
alias print='/usr/bin/lp -o nobanner -d $LPDEST'
            # Assumes LPDEST is defined (default printer)
alias pjet='enscript -h -G -fCourier9 -d $LPDEST'
            # Pretty-print using enscript

alias du='du -h'       # Makes a more readable output.
alias df='df -Th'

#-------------------------------------------------------------
# The 'ls' family (this assumes you use a recent GNU ls)
#-------------------------------------------------------------
alias ll='ls -lahF --time-style=long-iso --color=always'
alias ls='ls -hF --color=always'  # add colors for filetype recognition
alias la='ls -A'           # show hidden files
alias lx='ls -lXB'         # sort by extension
alias lk='ls -lSr'         # sort by size, biggest last
alias lc='ls -ltcr'        # sort by and show change time, most recent last
alias lu='ls -ltur'        # sort by and show access time, most recent last
alias lt='ls -ltr'         # sort by date, most recent last
alias lm='ls -al |more'    # pipe through 'more'
alias lr='ls -lR'          # recursive ls
alias tree='tree -Csu'     # nice alternative to 'recursive ls'

# If your version of 'ls' doesn't support --group-directories-first try this:
# function ll(){ ls -l "$@"| egrep "^d" ; ls -lXB "$@" 2>&-| \
#                egrep -v "^d|total "; }
alias ln='ln -s'
alias src='source ~/.bash_profile'
alias so='source'
alias virc='vim ~/.bashrc'
alias cls='clear'
alias zz='exit'
alias xx='exit'
alias vi='vim'
alias em='emacs --debug-init $* &'

#-------------------------------------------------------------
# tailoring 'less'
#-------------------------------------------------------------
alias more='less'

#-------------------------------------------------------------
# spelling typos - highly personnal and keyboard-dependent :-)
#-------------------------------------------------------------
alias xs='cd'
alias vf='cd'

#-------------------------------------------------------------
# Screen
#-------------------------------------------------------------
alias sc='screen -AdR -h 1000'

#-------------------------------------------------------------
# ps
#-------------------------------------------------------------
alias ps='ps xf -f'

#-------------------------------------------------------------
# grep
#-------------------------------------------------------------
alias grep='grep -i'

#-------------------------------------------------------------
# chmod
#-------------------------------------------------------------
alias cm755='chmod 755 -R'
alias cm777='chmod 777 -R'
alias cm700='chmod 700 -R'
alias cm600='chmod 600 -R'

#-------------------------------------------------------------
# PJ Location Setting
#-------------------------------------------------------------
alias aes='cd ~/workspace/7AES; ls'
alias amc='cd ~/workspace/6AMC_PLUS; ls'
alias ddci='cd ~/workspace/5DDCI; ls'
alias meteor='cd ~/workspace/8METEOR; ls'
alias lsu='cd ~/workspace/8METEOR/2LSU2IF; ls'
alias input='cd ~/workspace/8METEOR/1INPUT2IF; ls'
alias atom='cd ~/workspace/9Alphard2/1Atom; ls'
alias uran='cd ~/workspace/9Alphard2/2Uran; ls'
alias tiger='cd ~/workspace/9Alphard2/3Tiger; ls'
alias caxi_lmc='cd ~/workspace/9Alphard2/3Tiger/1CAXI_LMC; ls'
alias plb_lmc='cd ~/workspace/9Alphard2/3Tiger/2PLB_LMC; ls'
alias dbox='cd ~/DropBox/; ls'
alias config='cd ~/workspace/CONFIG;; ls'

#-------------------------------------------------------------
# login other servers
#-------------------------------------------------------------
alias art10='rlogin art10'
alias art11='rlogin art11'
alias art20='rlogin art20'
alias art21='rlogin art21'
alias art22='rlogin art22'
alias art23='rlogin art23'
alias art24='rlogin art24'
alias art25='rlogin art25'
alias art26='rlogin art22'
alias art27='rlogin art23'
alias art28='rlogin art24'
alias art29='rlogin art25'
alias fusion1='rlogin fusion1'


#-------------------------------------------------------------
# kterm
#-------------------------------------------------------------
# alias kterm     'kterm -n $HOST -T $HOST -fn 9x15 -sb -sl 3000 &'
alias x9='xterm -bg black -fg white -fn 9x15 -sb -cr green -rvc -sl 30000 &'
alias xt='xterm -bg black -fg white -fn 9x15 -sb -cr green -rvc -sl 30000 &'
alias x8='xterm -bg black -fg white -fn clR8x16 -sb -cr green -sl 30000 &'
alias xr='xterm -bg red4 -fg white -fn 9x15 -cr green -sb -sl 30000 -vb &'
alias xg='xterm -bg darkgreen -fg white -cr yellow2 -sb -fn 9x15 -sl 3000 -vb &'
alias xb='xterm -bg lightblue -fg black -cr red -sb -fn 9x15 -sl 30000 -vb &'
alias xw='xterm -bg white -fg black -cr red -sb -fn 9x15 -sl 30000 -vb &'
alias xd='xterm -bg darkgray -fg black -cr green -sb -fn 9x15 -sl 30000 -vb &'


#-------------------------------------------------------------
# CVS setting
#-------------------------------------------------------------
export CVS_RSH=ssh
#export CVSROOT=:ext:bkn_fndr_cvs_mngr@asiccvs:/home/ASIC/fbkn/CVS_DB
export CVSROOT=/home/hong_jin/workspace/MY_CVS


#-------------------------------------------------------------
# EDA Tools
#-------------------------------------------------------------
# alias 2XVerdi='2XVerdi $* &'

#-------------------------------------------------------------
# EDA Tool License setting
#-------------------------------------------------------------
# export LM_LICENSE_FILE=27000@fusion1:27001@earth:1717@earth:5219@fusion1

if [ "$HOSTNAME" == "ratchet" ]
then
    export LM_LICENSE_FILE=27000@ratchet:/license/license.dat
    export VCS_VERSION=B-2008.12
    export VCS_HOME=/EDA_Tools/vcs/$VCS_VERSION
    export VERDI_HOME=/EDA_Tools/Verdi
    export MODELSIM_HOME=/EDA_Tools/modelsim/se6.5/modeltech
    export DC_HOME=/EDA_Tools/DC/A-2007.12-SP5
elif [ "$HOSTNAME" == "asic-server" ]
then
    export LM_LICENSE_FILE=/usr/synopsys/license/license.dat:/license/license.dat
    export VCS_VERSION=vcs08
    export VCS_HOME=/home/synopsys/$VCS_VERSION
    export VERDI_HOME=/home/asic/Verdi
    export MODELSIM_HOME=/home/asic/modelsim_se65/modeltech
    export DC_HOME=/usr/synopsys/DC_A-2007.12-SP5
elif [ "$HOSTNAME" == "megatron" ]
then
    export LM_LICENSE_FILE=/usr/synopsys/license/license.dat:/license/license.dat
    export VCS_VERSION=B-2008.12
    export VCS_HOME=/EDA_Tools/vcs/$VCS_VERSION
    export VERDI_HOME=/EDA_Tools/Verdi
    export MODELSIM_HOME=/EDA_Tools/modelsim/se6.5/modeltech
    export DC_HOME=/EDA_Tools/DC/A-2007.12-SP5
else
    export LM_LICENSE_FILE=27000@fusion1
    export VCS_VERSION=D-2010.06
    export VCS_HOME=/cadtools/vcs/$VCS_VERSION
    export VERDI_HOME=/cadtools/novas/Novas-201010
fi

#-------------------------------------------------------------
# Modelsim Setting
#-------------------------------------------------------------
export MODELSIM_BIN=$MODELSIM_HOME/linux
export PATH=$PATH:$MODELSIM_BIN

#-------------------------------------------------------------
# Verdi Setting
#-------------------------------------------------------------
export VERDI_BIN=$VERDI_HOME/bin
export PATH=$PATH:$VERDI_BIN
export FSDB_ENV_DISABLE_OLD_FSDB_VER_MSG

#-------------------------------------------------------------
# DC Setting
#-------------------------------------------------------------
export DC_BIN=$DC_HOME/linux/syn/bin
export PATH=$PATH:$DC_BIN

#-------------------------------------------------------------
# SpyGlass Setting
#-------------------------------------------------------------
export SPYGLASS_HOME=/cadtools/spyglass/SpyGlass-4.0.1.2jp2/SPYGLASS_HOME
#-------------------------------------------------------------
# SpyGlass LICEMSE Setting
#-------------------------------------------------------------
export ATRENTA_LICENSE_FILE=1700@earth


#-------------------------------------------------------------
# A few fun ones
#-------------------------------------------------------------

function xtitle      # Adds some text in the terminal frame.
{
    case "$TERM" in
        *term | rxvt)
#            echo -n -e "\033]0;$*\007" ;;
            echo -n -e "\033]0;${USER}@${HOSTNAME}:${PWD}\007"
            ;;
        *)
            ;;
    esac
}

# aliases that use xtitle
alias top='xtitle Processes on ${HOSTNAME} && top'
alias make='xtitle Making $(basename $PWD) ; make'
alias ncftp="xtitle ncFTP ; ncftp"

# .. and functions
#function man
#{
#    for i ; do
#        xtitle The $(basename $1|tr -d .[:digit:]) manual
#        command man -F -a "$i"
#    done
#}

function host_load
{
    THRESHOLD_LOAD=50
    COLOUR_LOW=${fg_GREEN}
    COLOUR_HIGH=${fg_RED}

    if [ $OS = "Linux" ]; then
        ONE=$(uptime | sed -e "s/.*load average: \(.*\...\),\(.*\...\),\(.*\...\)/\1/" -e "s/ //g")
    fi
    if [ $OS = "Darwin" ]; then
        ONE=$(uptime | sed -e "s/.*load averages: \(.*\...\)\(.*\...\)\(.*\...\)/\1/" -e "s/ //g")
    fi
    ONEHUNDRED=$(echo -e "scale=0 \n $ONE/0.01 \nquit \n" | bc)
    if [ $ONEHUNDRED -gt $THRESHOLD_LOAD ]
        then
            HOST_COLOR=$COLOUR_HIGH
        else
            HOST_COLOR=$COLOUR_LOW
    fi
}

#-------------------------------------------------------------
# Make the following commands run in background automatically:
#-------------------------------------------------------------

function te  # Wrapper around xemacs/gnuserv ...
{
    if [ "$(gnuclient -batch -eval t 2>&-)" == "t" ]; then
        gnuclient -q "$@";
    else
        ( xemacs "$@" &);
    fi
}

function soffice { command soffice "$@" & }

function firefox { command firefox "$@" & }

function xpdf { command xpdf "$@" & }

function private
{
    find $HOME -type d -exec chmod 700 {} \;
    find $HOME -type f -exec chmod 600 {} \;
    find $HOME/bin -type f -exec chmod +x {} \;
}

function psg
{
    if [ $# -lt 1 ] || [ $# -gt 1 ]; then
        echo "grep running processes"
        echo "USAGE: psg [process]"
    else
        ps aux | grep USER | grep -v grep
        ps aux | grep -i $1 | grep -v grep
    fi
}

function ds
{
    echo "Size of Directories in MB"
    if [ $# -lt 1 ] || [ $# -gt 2 ]; then
        echo "Please specify a directory! By default, using pwd"
        echo "USAGE: ds [dir]"
        DIR=$(pwd)
        find $DIR -maxdepth 1 -type d -exec du -sm \{\} \; | sort -nr
    else
        find $1 -maxdepth 1 -type d -exec du -sm \{\} \; | sort -nr
    fi
}

#-------------------------------------------------------------
# File & string-related functions:
#-------------------------------------------------------------

# Find a file with a pattern in name:
function ff { find . -type f -iname '*'$*'*' -ls ; }

# Find a file with pattern $1 in name and Execute $2 on it:
function fe
{ find . -type f -iname '*'${1:-}'*' -exec ${2:-file} {} \;  ; }

# Find a pattern in a set of files and highlight them:
# (needs a recent version of egrep)
function fstr
{
    OPTIND=1
    local case=""
    local usage="fstr: find string in files.
Usage: fstr [-i] \"pattern\" [\"filename pattern\"] "
    while getopts :it opt
    do
        case "$opt" in
        i) case="-i " ;;
        *) echo "$usage"; return;;
        esac
    done
    shift $(( $OPTIND - 1 ))
    if [ "$#" -lt 1 ]; then
        echo "$usage"
        return;
    fi
    find . -type f -name "${2:-*}" -print0 | \
    xargs -0 egrep --color=always -sn ${case} "$1" 2>&- | more

}

function cuttail # cut last n lines in file, 10 by default
{
    nlines=${2:-10}
    sed -n -e :a -e "1,${nlines}!{P;N;D;};N;ba" $1
}

function lowercase  # move filenames to lowercase
{
    for file ; do
        filename=${file##*/}
        case "$filename" in
            */*) dirname==${file%/*}
                ;;
            *) dirname=.
                ;;
        esac
        nf=$(echo $filename | tr A-Z a-z)
        newname="${dirname}/${nf}"
        if [ "$nf" != "$filename" ]; then
            mv "$file" "$newname"
            echo "lowercase: $file --> $newname"
        else
            echo "lowercase: $file not changed."
        fi
    done
}


function swap  # Swap 2 filenames around, if they exist
{                #(from Uzi's bashrc).
    local TMPFILE=tmp.$$

    [ $# -ne 2 ] && echo "swap: 2 arguments needed" && return 1
    [ ! -e $1 ] && echo "swap: $1 does not exist" && return 1
    [ ! -e $2 ] && echo "swap: $2 does not exist" && return 1

    mv "$1" $TMPFILE
    mv "$2" "$1"
    mv $TMPFILE "$2"
}

function extract      # Handy Extract Program.
{
     if [ -f $1 ] ; then
         case $1 in
             *.tar.bz2)   tar xvjf $1     ;;
             *.tar.gz)    tar xvzf $1     ;;
             *.bz2)       bunzip2 $1      ;;
             *.rar)       unrar x $1      ;;
             *.gz)        gunzip $1       ;;
             *.tar)       tar xvf $1      ;;
             *.tbz2)      tar xvjf $1     ;;
             *.tgz)       tar xvzf $1     ;;
             *.zip)       unzip $1        ;;
             *.Z)         uncompress $1   ;;
             *.7z)        7z x $1         ;;
             *)           echo "'$1' cannot be extracted via >>extract<<" ;;
         esac
     else
         echo "'$1' is not a valid file"
     fi
}

function tarview
{
    echo -n "Display contents of $1 "
    if [ ${1##*.} = tar ]
    then
        echo "(uncompressed tar)"
        tar tvf $1
    elif [ ${1##*.} == gz ]
    then
        echo "(gzip-compressed tar)"
        tar tzvf $1
    elif [ ${1##*.} = bz2 ]
    then
        echo "(bzip2-compressed tar)"
        cat $1 | bzip2 -d | tar tvf -
    fi
}

rot13 ()
{              # For some reason, rot13 pops up everywhere
    if [ $# -eq 0 ]; then
        tr '[a-m][n-z][A-M][N-Z]' '[n-z][a-m][N-Z][A-M]'
    else
        echo $* | tr '[a-m][n-z][A-M][N-Z]' '[n-z][a-m][N-Z][A-M]'
    fi
}

# ls && number of entrys
function lf
{
    ls --color=tty --classify $*
    echo "$(ls -l $* | wc -l) files"
}

#-------------------------------------------------------------
# Process/system related functions:
#-------------------------------------------------------------

function my_ps { ps $@ -u $USER -o pid,%cpu,%mem,bsdtime,command ; }

function pp { my_ps f | awk '!/awk/ && $0~var' var=${1:-".*"} ; }


function killps                 # Kill by process name.
{
    local pid pname sig="-TERM"   # Default signal.
    if [ "$#" -lt 1 ] || [ "$#" -gt 2 ]; then
        echo "Usage: killps [-SIGNAL] pattern"
        return;
    fi
    if [ $# = 2 ]; then sig=$1 ; fi
    for pid in $(my_ps| awk '!/awk/ && $0~pat { print $1 }' pat=${!#} ) ; do
        pname=$(my_ps | awk '$1~var { print $5 }' var=$pid )
        if ask "Kill process $pid <$pname> with signal $sig?"
            then kill $sig $pid
        fi
    done
}

function my_ip # Get IP adresses.
{
    MY_IP=$(/sbin/ifconfig ppp0 | awk '/inet/ { print $2 } ' | sed -e s/addr://)
    MY_ISP=$(/sbin/ifconfig ppp0 | awk '/P-t-P/ { print $3 } ' | sed -e s/P-t-P://)
}

function sys_info   # Get current host related info.
{
    echo -e "\n${fg_RED}You are logged on:$NC " ; hostname
    echo -e "\n${fg_RED}Additionnal information:$NC " ; uname -a
#    echo -e "\n${fg_RED}Users logged on:$NC " ; w -h
    echo -e "\n${fg_RED}Current date :$NC " ; date
    echo -e "\n${fg_RED}Machine stats :$NC " ; uptime
    echo -e "\n${fg_RED}Memory stats :$NC " ; free -m
    echo -e "\n${fg_RED}Disk space:$NC " ; df -h

#    my_ip 2>&- ;
#    echo -e "\n${fg_RED}Local IP Address :$NC" ; echo ${MY_IP:-"Not connected"}
#    echo -e "\n${fg_RED}ISP Address :$NC" ; echo ${MY_ISP:-"Not connected"}
#    echo -e "\n${fg_RED}Open connections :$NC "; netstat -pan --inet;
    echo
}

#-------------------------------------------------------------
# Misc utilities:
#-------------------------------------------------------------

function repeat       # Repeat n times command.
{
    if [ $# -lt 1 ]; then
      echo "Repeat a command N times"
      echo "USAGE: repeat [integer] [CMD]"
    else
      local i max
      max=$1; shift;
      for ((i=1; i <= max ; i++)); do  # --> C-like syntax
          eval "$@";
      done
    fi
}


function ask          # See 'killps' for example of use.
{
    echo -n "$@" '[y/n] ' ; read ans
    case "$ans" in
        y*|Y*)  return 0
                ;;
        *)      return 1
                ;;
    esac
}

function corename   # Get name of app that created a corefile.
{
    for file ; do
        echo -n $file : ; gdb --core=$file --batch | head -1
    done
}


#=========================================================================
# PROGRAMMABLE COMPLETION - ONLY SINCE BASH-2.04
# Most are taken from the bash 2.05 documentation and from Ian McDonald's
# 'Bash completion' package (http://www.caliban.org/bash/#completion).
# You will in fact need bash more recent than 3.0 for some features.
#=========================================================================

if [ "${BASH_VERSION%.*}" \< "3.0" ]; then
    echo "You will need to upgrade to version 3.0 \
for full programmable completion features."
    return
fi

#set +o nounset          # otherwise some completions will fail.

complete -A hostname   rsh rcp telnet rlogin r ftp ping disk
complete -A export     printenv
complete -A variable   export local readonly unset
complete -A enabled    builtin
complete -A alias      alias unalias
complete -A function   function
complete -A user       su mail finger

complete -A helptopic  help     # Currently, same as builtins.
complete -A shopt      shopt
complete -A stopped -P '%' bg
complete -A job -P '%'     fg jobs disown

complete -A directory  mkdir rmdir
complete -A directory   -o default cd

# Compression
complete -f -o default -X '*.+(zip|ZIP)'  zip
complete -f -o default -X '!*.+(zip|ZIP)' unzip
complete -f -o default -X '*.+(z|Z)'      compress
complete -f -o default -X '!*.+(z|Z)'     uncompress
complete -f -o default -X '*.+(gz|GZ)'    gzip
complete -f -o default -X '!*.+(gz|GZ)'   gunzip
complete -f -o default -X '*.+(bz2|BZ2)'  bzip2
complete -f -o default -X '!*.+(bz2|BZ2)' bunzip2
complete -f -o default -X '!*.+(zip|ZIP|z|Z|gz|GZ|bz2|BZ2)' extract


# Documents - Postscript,pdf,dvi.....
complete -f -o default -X '!*.+(ps|PS)'  gs ghostview ps2pdf ps2ascii
complete -f -o default -X '!*.+(dvi|DVI)' dvips dvipdf xdvi dviselect dvitype
complete -f -o default -X '!*.+(pdf|PDF)' acroread pdf2ps
complete -f -o default -X \
'!*.@(@(?(e)ps|?(E)PS|pdf|PDF)?(.gz|.GZ|.bz2|.BZ2|.Z))' gv ggv
complete -f -o default -X '!*.texi*' makeinfo texi2dvi texi2html texi2pdf
complete -f -o default -X '!*.tex' tex latex slitex
complete -f -o default -X '!*.lyx' lyx
complete -f -o default -X '!*.+(htm*|HTM*)' lynx html2ps
complete -f -o default -X \
'!*.+(doc|DOC|xls|XLS|ppt|PPT|sx?|SX?|csv|CSV|od?|OD?|ott|OTT)' soffice

# Multimedia
complete -f -o default -X \
'!*.+(gif|GIF|jp*g|JP*G|bmp|BMP|xpm|XPM|png|PNG)' xv gimp ee gqview
complete -f -o default -X '!*.+(mp3|MP3)' mpg123 mpg321
complete -f -o default -X '!*.+(ogg|OGG)' ogg123
complete -f -o default -X \
'!*.@(mp[23]|MP[23]|ogg|OGG|wav|WAV|pls|m3u|xm|mod|s[3t]m|it|mtm|ult|flac)' xmms
complete -f -o default -X \
'!*.@(mp?(e)g|MP?(E)G|wma|avi|AVI|asf|vob|VOB|bin|dat|vcd|\
ps|pes|fli|viv|rm|ram|yuv|mov|MOV|qt|QT|wmv|mp3|MP3|ogg|OGG|\
ogm|OGM|mp4|MP4|wav|WAV|asx|ASX)' xine

complete -f -o default -X '!*.pl'  perl perl5

# This is a 'universal' completion function - it works when commands have
# a so-called 'long options' mode , ie: 'ls --all' instead of 'ls -a'
# Needs the '-o' option of grep
#  (try the commented-out version if not available).

# First, remove '=' from completion word separators
# (this will allow completions like 'ls --color=auto' to work correctly).

COMP_WORDBREAKS=${COMP_WORDBREAKS/=/}

_get_longopts()
{
    #$1 --help | sed  -e '/--/!d' -e 's/.*--\([^[:space:].,]*\).*/--\1/'| \
#grep ^"$2" |sort -u ;
    $1 --help | grep -o -e "--[^[:space:].,]*" | grep -e "$2" |sort -u
}

_longopts()
{
    local cur
    cur=${COMP_WORDS[COMP_CWORD]}

    case "${cur:-*}" in
       -*)      ;;
        *)      return ;;
    esac

    case "$1" in
      \~*)      eval cmd="$1" ;;
        *)      cmd="$1" ;;
    esac
    COMPREPLY=( $(_get_longopts ${1} ${cur} ) )
}
complete  -o default -F _longopts configure bash
complete  -o default -F _longopts wget id info a2ps ls recode

_tar()
{
    local cur ext regex tar untar

    COMPREPLY=()
    cur=${COMP_WORDS[COMP_CWORD]}

    # If we want an option, return the possible long options.
    case "$cur" in
        -*)     COMPREPLY=( $(_get_longopts $1 $cur ) ); return 0;;
    esac

    if [ $COMP_CWORD -eq 1 ]; then
        COMPREPLY=( $( compgen -W 'c t x u r d A' -- $cur ) )
        return 0
    fi

    case "${COMP_WORDS[1]}" in
        ?(-)c*f)
            COMPREPLY=( $( compgen -f $cur ) )
            return 0
            ;;
        +([^Izjy])f)
            ext='tar'
            regex=$ext
            ;;
        *z*f)
            ext='tar.gz'
            regex='t\(ar\.\)\(gz\|Z\)'
            ;;
        *[Ijy]*f)
            ext='t?(ar.)bz?(2)'
            regex='t\(ar\.\)bz2\?'
            ;;
        *)
            COMPREPLY=( $( compgen -f $cur ) )
            return 0
            ;;

    esac

    if [[ "$COMP_LINE" == tar*.$ext' '* ]]; then
        # Complete on files in tar file.
        # Get name of tar file from command line.
        tar=$( echo "$COMP_LINE" | \
               sed -e 's|^.* \([^ ]*'$regex'\) .*$|\1|' )
        # Devise how to untar and list it.
        untar=t${COMP_WORDS[1]//[^Izjyf]/}

        COMPREPLY=( $( compgen -W "$( echo $( tar $untar $tar \
                    2>/dev/null ) )" -- "$cur" ) )
        return 0

    else
        # File completion on relevant files.
        COMPREPLY=( $( compgen -G $cur\*.$ext ) )

    fi

    return 0

}

complete -F _tar -o default tar

_make()
{
    local mdef makef makef_dir="." makef_inc gcmd cur prev i;
    COMPREPLY=();
    cur=${COMP_WORDS[COMP_CWORD]};
    prev=${COMP_WORDS[COMP_CWORD-1]};
    case "$prev" in
        -*f)
            COMPREPLY=($(compgen -f $cur ));
            return 0
            ;;
    esac;
    case "$cur" in
        -*)
            COMPREPLY=($(_get_longopts $1 $cur ));
            return 0
            ;;
    esac;

    # make reads `GNUmakefile', then `makefile', then `Makefile'
    if [ -f ${makef_dir}/GNUmakefile ]; then
        makef=${makef_dir}/GNUmakefile
    elif [ -f ${makef_dir}/makefile ]; then
        makef=${makef_dir}/makefile
    elif [ -f ${makef_dir}/Makefile ]; then
        makef=${makef_dir}/Makefile
    else
        makef=${makef_dir}/*.mk        # Local convention.
    fi


    # Before we scan for targets, see if a Makefile name was
    # specified with -f ...
    for (( i=0; i < ${#COMP_WORDS[@]}; i++ )); do
        if [[ ${COMP_WORDS[i]} == -f ]]; then
           # eval for tilde expansion
           eval makef=${COMP_WORDS[i+1]}
           break
        fi
    done
    [ ! -f $makef ] && return 0

    # deal with included Makefiles
    makef_inc=$( grep -E '^-?include' $makef | \
    sed -e "s,^.* ,"$makef_dir"/," )
    for file in $makef_inc; do
        [ -f $file ] && makef="$makef $file"
    done


    # If we have a partial word to complete, restrict completions to
    # matches of that word.
    if [ -n "$cur" ]; then gcmd='grep "^$cur"' ; else gcmd=cat ; fi

    COMPREPLY=( $( awk -F':' '/^[a-zA-Z0-9][^$#\/\t=]*:([^=]|$)/ \
                                {split($1,A,/ /);for(i in A)print A[i]}' \
                                $makef 2>/dev/null | eval $gcmd  ))

}

complete -F _make -X '+($*|*.[cho])' make gmake pmake

_killall()
{
    local cur prev
    COMPREPLY=()
    cur=${COMP_WORDS[COMP_CWORD]}

    # get a list of processes (the first sed evaluation
    # takes care of swapped out processes, the second
    # takes care of getting the basename of the process)
    COMPREPLY=( $( /usr/bin/ps -u $USER -o comm  | \
        sed -e '1,1d' -e 's#[]\[]##g' -e 's#^.*/##'| \
        awk '{if ($0 ~ /^'$cur'/) print $0}' ))

    return 0
}

complete -F _killall killall killps


# A meta-command completion function for commands like sudo(8), which need to
# first complete on a command, then complete according to that command's own
# completion definition - currently not quite foolproof,
# but still quite useful (By Ian McDonald, modified by me).

_meta_comp()
{
    local cur func cline cspec

    COMPREPLY=()
    cur=${COMP_WORDS[COMP_CWORD]}
    cmdline=${COMP_WORDS[@]}
    if [ $COMP_CWORD = 1 ]; then
         COMPREPLY=( $( compgen -c $cur ) )
    else
        cmd=${COMP_WORDS[1]}            # Find command.
        cspec=$( complete -p ${cmd} )   # Find spec of that command.

        # COMP_CWORD and COMP_WORDS() are not read-only,
        # so we can set them before handing off to regular
        # completion routine:
        # Get current command line minus initial command,
        cline="${COMP_LINE#$1 }"
        # split current command line tokens into array,
        COMP_WORDS=( $cline )
        # set current token number to 1 less than now.
        COMP_CWORD=$(( $COMP_CWORD - 1 ))
        # If current arg is empty, add it to COMP_WORDS array
        # (otherwise that information will be lost).
        if [ -z $cur ]; then COMP_WORDS[COMP_CWORD]=""  ; fi

        if [ "${cspec%%-F *}" != "${cspec}" ]; then
      # if -F then get function:
            func=${cspec#*-F }
            func=${func%% *}
            eval $func $cline   # Evaluate it.
        else
            func=$( echo $cspec | sed -e 's/^complete//' -e 's/[^ ]*$//' )
            COMPREPLY=( $( eval compgen $func $cur ) )
        fi

    fi
}


complete -o default -F _meta_comp nohup \
eval exec trace truss strace sotruss gdb
complete -o default -F _meta_comp command type which man nice time


# copy and go to dir
cpg ()
{
  if [ $# -lt 2 ]; then
    echo "Copy directory, and cd to it"
    echo "USAGE: cpg dir-1 dir-2"
  else
    if [ -d "$2" ];then
        cp $1 $2 && cd $2
    else
        cp $1 $2
    fi
  fi
}

# copy huge size dir
cphg ()
{
    if [ $# -lt 2 ]; then
        echo "Copy huge size directory: "
        echo "Usage: cphg dir-1 dir-2"
    else
        if [ -d "$2" ]; then
            (cd $1 && tar cf - .) | (cd $2 && tar xpvf -)
        else
            mkdir $2
            (cd $1 && tar cf - .) | (cd $2 && tar xpvf -)
        fi
    fi
}

# move and go to dir
mvg ()
{
  if [ $# -lt 2 ]; then
    echo "Move directory, and cd to it"
    echo "USAGE: mvg dir-1 dir-2"
  else
    if [ -d "$2" ];then
        mv $1 $2 && cd $2
    else
        mv $1 $2
    fi
  fi
}

# make directory, then cd to the first directory
mdcd ()
{
  if [ $# -lt 1 ]; then
    echo "Make directory, then cd to the first one"
    echo "USAGE: mdcd dir-1 [dir-2]"
  else
    mkdir $*
    cd $1
  fi
}

# sh-shell:bash
# End of File

# colorful the man page
export LESS_TERMCAP_mb=$'\E[05;34m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;34m'       # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[44;33m'       # begin standout-mode
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;33m'       # begin underline

