# .cshrc
# 
set history = 100
set savehist = 100
set autocorrect
set filec
set autolist
set ignoreeof

# SETENV
setenv EDITOR 'vim'
setenv MANPAGER "col -b | view -c 'set ft=man nomod nolist' -"

umask 022

# ALIAS
alias cd..  cd ..
alias cd... cd ../..
alias cd-   cd -
alias vi    vim
alias view  'vim -R'
alias grep  'grep -d skip'
alias la    'ls -AF --color=auto'
alias lla   'ls -AlhF --color=auto --time-style=long-iso'
alias ll    'ls -lhF --color=auto --time-style=long-iso'
alias lm    'ls -lh | more'
#alias ls   'ls -F --color=auto'
alias cp    'cp -ri'
alias mv    'mv -i'
alias rm    'rm -ri'
alias cls   'clear; pwd'
alias xx    'exit'
alias zz    'exit'
alias src   'source ~/.cshrc'
alias so    'source'

# PATH
set path = ($path /usr/local/bin* . lib /usr/bin)

setenv PS1 '[\A][\u@\H:\w]\$[\#]>'


# unique history
set histdup='all'

# auto-complete
set complete='enhance'

# ignore end of characters when auto-complete
set fignore=(.o .out \~)

# display auto list when TAB
set autolist

# C-j C-k
bindkey "^M" complete-word-fwd
bindkey "^K" complete-word-back

#

alias cd 'cd \!*; echo -n "^[]0;${cwd}^Gcsh% "'


#######################################
# CVS SETTING
#######################################
# CVS in KMC
#setenv CVS_RSH ssh
#setenv CVSROOT :ext:bkn_fndr_cvs_mngr@asiccvs:/home/ASIC/fbkn/CVS_DB
#setenv CVSROOT :ext:bkn_fndr_cvs_mngr@10.181.127.75:/home/ASIC/fbkn/CVS_DB
#setenv CVSROOT :ext:km+z000023061@aloe1:/export1/ASIC/emerald/CVS_DB
#
#
alias kk		'kterm -n `hostname`:Black -sb -cr green -vb -sl 3000&'
alias kr		'kterm -n `hostname`:Red -bg red4 -fg white -cr green -sb -sl 3000 -vb &'
alias kg		'kterm -n `hostname`:Green -bg darkgreen -fg white -cr yellow2 -sb -sl 3000 -vb &'
alias kb		'kterm -n `hostname`:Blue -bg lightblue -fg black -cr red -sb -sl 3000 -vb &'
alias kw		'kterm -n `hostname`:White -bg white -fg black -cr red -sb -sl 3000 -vb &'
alias kd		'kterm -n `hostname`:Gray -bg darkgray -fg black -cr green -sb -sl 3000 -vb &'
