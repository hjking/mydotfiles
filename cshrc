#set LANG=zh_CN
setenv LANG ja_JP.eucJP
set history=20
set savehist=10
set autocorrect
set filec
set autolist
set ignoreeof

setenv PRINTER qms

umask 077

#set prompt = "`hostname`:\!> "
set prompt = "`hostname`:=>"

#######################################
# DISPLAY SETTING
#######################################
if ( $?DISPLAY ) then
        env | grep DISPLAY > ~/.display.conf ;
	perl -pi -e 's/=/ /g' ~/.display.conf ;
	perl -pi -e 's/^/setenv /g' ~/.display.conf ;
else
        source ~/.display.conf ;
endif

#######################################
# CVS SETTING
#######################################
setenv CVS_RSH ssh
setenv CVSROOT :ext:bkn_fndr_cvs_mngr@asiccvs:/home/ASIC/fbkn/CVS_DB
#setenv CVSROOT :ext:bkn_fndr_cvs_mngr@10.181.127.75:/home/ASIC/fbkn/CVS_DB
#setenv CVSROOT :ext:km+z000023061@aloe1:/export1/ASIC/emerald/CVS_DB
########################################
# MY CVS
########################################
#setenv CVSROOT ~/MY_CVS/CVS_DB

#######################################
# Alias
#######################################
alias vi vim
alias view 'vim -R'
alias cd.	'cd ..'
alias cd..	'cd ../..'
alias cd...	'cd ../../..'
alias ll	'ls -lahF'
alias la	'ls -aF'
alias cls 	'clear; pwd'
alias zz 	exit
alias xx	exit
alias cp	'cp -r'
#alias mv	'mv -r'
alias rm	'rm -r'
alias lo	logout
alias grep	'grep -i'
alias gwave	gtkwave

alias reader '/opt/Adobe/Reader8/bin/acroread &'

alias so	source
alias src	'source ~/.cshrc'

alias art10		'rsh art10'
alias art11		'rsh art11'
alias art20 	'rsh art20'
alias art21 	'rsh art21'
alias art22 	'rsh art22'
alias art23 	'rsh art23'
alias art24 	'rsh art24'
alias art25 	'rsh art25'
alias art26 	'rsh art22'
alias art27 	'rsh art23'
alias art28 	'rsh art24'
alias art29 	'rsh art25'
alias fusion1	'rsh fusion1'

alias emacs		'emacs &'
alias kk		'kterm -n `hostname`:Black -sb -cr green -vb -sl 3000&'
alias kr		'kterm -n `hostname`:Red -bg red4 -fg white -cr green -sb -sl 3000 -vb &'
alias kg		'kterm -n `hostname`:Green -bg darkgreen -fg white -cr yellow2 -sb -sl 3000 -vb &'
alias kb		'kterm -n `hostname`:Blue -bg lightblue -fg black -cr red -sb -sl 3000 -vb &'
alias kw		'kterm -n `hostname`:White -bg white -fg black -cr red -sb -sl 3000 -vb &'
alias kd		'kterm -n `hostname`:Gray -bg darkgray -fg black -cr green -sb -sl 3000 -vb &'
alias 2XVerdi	'2XVerdi &'

##########################################
# DC Setting
##########################################
#setenv SYNOPSYS /cadtools/dc/B-2008.09-SP4
#setenv SYNOPSYS /cadtools/dc/A-2007.12-SP5
#set path = ($path $SYNOPSYS/linux/syn/bin)

# Modelsim setup
#setenv MODELSIM_HOME /cadtools/modelsim6.3g/modeltech
setenv MODELSIM_HOME /cadtools/modelsim6.4/modeltech
set path = ($path $MODELSIM_HOME/linux_x86_64)

#VCS SIMULATION PATH VARIABLES
#setenv LM_LICENSE_FILE 27000@fusion1:27001@earth
setenv LM_LICENSE_FILE 27000@fusion1:27001@earth:1717@earth
setenv VCS_HOME /cadtools/vcs-mx/Y-2006.06-SP2-3
set path = ( $path $VCS_HOME/bin )

# Spyglass setup
setenv SPYGLASS_HOME /cadtools/spyglass/SpyGlass-4.0.1.2jp2/SPYGLASS_HOME
setenv ATRENTA_LICENSE_FILE 1700@earth

##########################################
# Path Setting
##########################################
set path = (. $path)
set path = ($path /usr/X11R6/lib)
