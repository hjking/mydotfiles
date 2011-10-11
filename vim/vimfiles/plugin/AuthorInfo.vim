" Maintainer:   Dante <dantezhu@vip.qq.com>
" WebSite:      http://www.vimer.cn
" Last Change:  2009-10-18 00:00
"版权声明的设置{{{
"添加或更新头
let g:AuthorName="Hong Jin"
"let g:AuthorEmail="bestkindy@gmail.com"
let g:AuthorEmail="hong_jin@founder.com"
let g:AuthorHomePage=""
" map <F4> ms:call TitleDet()<cr>'s
map <F4> ms:call TitleDet()<cr>
function AddTitle()
    call append(0,"/*******************************************************************************")
    call append(1,"* (C) Copyright 2010 Founder International Software(Wuhan) Co.,Ltd")
    call append(2,"*")
    call append(3,"* Filename:          ".expand("%:t"))
    call append(4,"* Author:            ".g:AuthorName." (".g:AuthorEmail.")")
    call append(5,"* Created:           ".strftime("%Y-%m-%d %H:%M"))
    call append(6,"* Last Modified:     ".strftime("%Y-%m-%d %H:%M"))
    call append(7,"* Revesion:          0.1")
    call append(8,"* ID:                $Id$")
"    call append(6,"* Last Modified:   TIMESTAMP".strftime("%Y-%m-%d %H:%M"))
    call append(9,"* Reference:         ")
    call append(10,"* Description:      ")
    call append(11,"*")
    call append(12,"* Revision History:")
    call append(13,"* Date          Auther      Revision        Description")
    call append(14,"* ------------------------------------------------------------------------------")
    call append(15,"* ".strftime("%Y-%m-%d")   ."Hong Jin    0.1             1. Initial revision")
    call append(16,"* ------------------------------------------------------------------------------")
    call append(17,"*")
    call append(18,"*******************************************************************************/")
    echohl WarningMsg | echo "Successful in adding the copyright." | echohl None
endf
function UpdateTitle()
    normal m'
    execute '/# *Last Modified:/s@:.*$@\=strftime(":\t%Y-%m-%d %H:%M")@'
    normal ''
    normal mk
    execute '/# *Filename:/s@:.*$@\=":\t\t".expand("%:t")@'
    execute "noh"
    normal 'k
    echohl WarningMsg | echo "Successful in updating the copy right." | echohl None
endfunction
function TitleDet()
    let n=1
    "默认为添加
    while n < 10
        let line = getline(n)
        if line =~ '^\#\s*\S*Last\smodified:\S*.*$'
            call UpdateTitle()
            return
        endif
        let n = n + 1
    endwhile
    call AddTitle()
endfunction
"}}}
