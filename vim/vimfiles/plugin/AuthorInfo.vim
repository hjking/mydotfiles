" Maintainer:   Dante <dantezhu@vip.qq.com>
" WebSite:   	http://www.vimer.cn
" Last Change:  2009-10-18 00:00
"版权声明的设置{{{
"添加或更新头
let g:AuthorName="DanteZhu"
let g:AuthorEmail="dantezhu@vip.qq.com"
let g:AuthorHomePage="http://www.vimer.cn"
map <F4> ms:call TitleDet()<cr>'s
function AddTitle()
	call append(0,"/*=============================================================================")
	call append(1,"#")
	call append(2,"#   Author: 		".g:AuthorName." - ".g:AuthorEmail)
	call append(3,"#")
	call append(4,"#   HomePage: 		".g:AuthorHomePage)
	call append(5,"#")
	call append(6,"#   Last modified:	".strftime("%Y-%m-%d %H:%M"))
	call append(7,"#")
	call append(8,"#   Filename:		".expand("%:t"))
	call append(9,"#")
	call append(10,"#   Description: 	")
	call append(11,"#")
	call append(12,"=============================================================================*/")
	echohl WarningMsg | echo "Successful in adding the copyright." | echohl None
endf
function UpdateTitle()
	normal m'
	execute '/# *Last modified:/s@:.*$@\=strftime(":\t%Y-%m-%d %H:%M")@'
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
