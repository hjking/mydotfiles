" Vim Plugin for Verilog Code Automactic Generation&Check
" Language:     Verilog
" Maintainer:   Gavin Ge <arrowroothover@hotmail.com>
" Version:      1.55
" Last Update:  Wed. Sept 17 2008
" For version 7.x or above

if version < 700
   finish
endif
if exists("b:vlog_plugin")
   finish
endif
let b:vlog_plugin = 1

"iabbrev <= <= #`FFD

if exists("b:vlog_company") == 0
   let b:vlog_company = "Founder International Software (Wuhan)"
endif
if exists("b:vlog_max_col") == 0
   let b:vlog_max_col = 40
endif
if exists("b:vlog_arg_margin") == 0
   let b:vlog_arg_margin = "    "
endif
if exists("b:vlog_inst_margin") == 0
   let b:vlog_inst_margin = "        "
endif
if exists("b:verilog_indent_width")
   let b:vlog_ind = b:verilog_indent_width
else
   let b:vlog_ind = 4
endif

amenu Veri&log.&Header          :call AddHeader()<CR>
amenu Veri&log.&Comment         :call AddComment()<CR>
amenu Veri&log.-Automatic-      :
amenu Veri&log.Auto&Argument    :call AutoArg()<CR>
amenu Veri&log.Auto&Instance    :call AutoInst()<CR>
amenu Veri&log.Auto&Define      :call AutoDef()<CR>
amenu Veri&log.Auto&Sense       :call AutoSense()<CR>
amenu Veri&log.Auto&Reg         :call AutoReg()<CR>
amenu Veri&log.-QuickCheck-	:
amenu Veri&log.Report\ No\ Loading/Driving\ nets :call AutoCheck()<CR>
amenu Veri&log.-Kill-           :
amenu Veri&log.&KillAuto        :call KillAuto()<CR>
amenu Veri&log.KillAutoArg      :call KillAutoArg()<CR>
amenu Veri&log.KillAutoInst     :call KillAutoInst()<CR>
amenu Veri&log.KillAutoDef      :call KillAutoDef()<CR>
amenu Veri&log.KillAutoSense    :call KillAutoSense()<CR>
amenu Veri&log.KillAutoReg      :call KillAutoReg()<CR>
amenu Veri&log.-Add\ Always\ Block-                              :
amenu Veri&log.Always\ with\ posedge\ clock\ and\ posedge\ reset :call AddAlways("posedge", "posedge")<CR>
amenu Veri&log.Always\ with\ posedge\ clock\ and\ negedge\ reset :call AddAlways("posedge", "negedge")<CR>
amenu Veri&log.Always\ with\ negedge\ clock\ and\ posedge\ reset :call AddAlways("negedge", "posedge")<CR>
amenu Veri&log.Always\ with\ negedge\ clock\ and\ negedge\ reset :call AddAlways("negedge", "negedge")<CR>
amenu Veri&log.Always\ with\ posedge\ clock :call AddAlways("posedge", "")<CR>
amenu Veri&log.Always\ with\ negedge\ clock :call AddAlways("negedge", "")<CR>
amenu Veri&log.Combinational\ Always :call AddAlways("", "")<CR>

command Alpp :call AddAlways("posedge", "posedge")
command Alpn :call AddAlways("posedge", "negedge")
command Alnp :call AddAlways("negedge", "posedge")
command Alnn :call AddAlways("negedge", "negedge")
command Alp :call AddAlways("posedge", "")
command Aln :call AddAlways("negedge", "")
command Al :call AddAlways("", "")

"===============================================================
"        Add File Header
"===============================================================
function AddHeader()
  call append(0,  "//")
  call append(1,  "// Created by         :".b:vlog_company)
  call append(2,  "// Filename           :".expand("%"))
  call append(3,  "// Author             :".$USER."(RDC)")
  call append(4,  "// Created On         :".strftime("%Y-%m-%d %H:%M"))
  call append(5,  "// Last Modified      : ")
  call append(6,  "// Update Count       :".strftime("%Y-%m-%d %H:%M"))
  call append(7,  "// Description        :")
  call append(8,  "//                     ")
  call append(9,  "//                     ")
  call append(10, "//=======================================================================")
endfunction

"===============================================================
"        Add Comment Lines
"===============================================================
function AddComment()
   let curr_line = line(".")
   call append(curr_line,  "//===========================================")
   call append(curr_line+1,"//       ")
   call append(curr_line+2,"//===========================================")
endfunction

"===============================================================
"        Add an always statement
"===============================================================
function AddAlways(clk_edge, rst_edge)
   for line in getline(1, line("$"))
      if line =~ '^\s*\<input\>.*//\s*\<clock\>\s*$'
         let line = substitute(line, '^\s*\<input\>\s*', "", "")
         let clk = substitute(line, '\s*\(;\|,\).*$', "", "")
      elseif line =~ '^\s*\<input\>.*//\s*\<reset\>\s*$'
         let line = substitute(line, '^\s*\<\input\>\s*', "", "")
         let rst = substitute(line, '\s*\(;\|,\).*$', "", "")
      endif
   endfor
   let curr_line = line(".")
   if a:clk_edge == "posedge" && a:rst_edge == "posedge"
      call append(curr_line,   "always @(posedge ".clk." or posedge ".rst.") begin")
      call append(curr_line+1, repeat(" ", b:vlog_ind)."if (".rst.") begin")
      call append(curr_line+2, repeat(" ", b:vlog_ind)."end")
      call append(curr_line+3, repeat(" ", b:vlog_ind)."else begin")
      call append(curr_line+4, repeat(" ", b:vlog_ind)."end")
      call append(curr_line+5, "end")
   elseif a:clk_edge == "negedge" && a:rst_edge == "posedge"
      call append(curr_line,   "always @(negedge ".clk." or posedge ".rst.") begin")
      call append(curr_line+1, repeat(" ", b:vlog_ind)."if (".rst.") begin")
      call append(curr_line+2, repeat(" ", b:vlog_ind)."end")
      call append(curr_line+3, repeat(" ", b:vlog_ind)."else begin")
      call append(curr_line+4, repeat(" ", b:vlog_ind)."end")
      call append(curr_line+5, "end")
   elseif a:clk_edge == "posedge" && a:rst_edge == "negedge"
      call append(curr_line,   "always @(posedge ".clk." or negedge ".rst.") begin")
      call append(curr_line+1, repeat(" ", b:vlog_ind)."if (!".rst.") begin")
      call append(curr_line+2, repeat(" ", b:vlog_ind)."end")
      call append(curr_line+3, repeat(" ", b:vlog_ind)."else begin")
      call append(curr_line+4, repeat(" ", b:vlog_ind)."end")
      call append(curr_line+5, "end")
   elseif a:clk_edge == "negedge" && a:rst_edge == "negedge"
      call append(curr_line,   "always @(negedge ".clk." or negedge ".rst.") begin")
      call append(curr_line+1, repeat(" ", b:vlog_ind)."if (!".rst.") begin")
      call append(curr_line+2, repeat(" ", b:vlog_ind)."end")
      call append(curr_line+3, repeat(" ", b:vlog_ind)."else begin")
      call append(curr_line+4, repeat(" ", b:vlog_ind)."end")
      call append(curr_line+5, "end")
   elseif a:clk_edge == "posedge" && a:rst_edge == ""
      call append(curr_line,   "always @(posedge ".clk.") begin")
      call append(curr_line+1, "end")
   elseif a:clk_edge == "negedge" && a:rst_edge == ""
      call append(curr_line,   "always @(negedge ".clk.") begin")
      call append(curr_line+1, "end")
   else
      call append(curr_line,   "always @(*) begin")
      call append(curr_line+1, "end")
   endif
endfunction

"===============================================================
"        Update Current Buffer
"===============================================================
function UpdateBuf(new_lines)
   if len(a:new_lines) < line("$")
      for line_index in range(1, line("$"), 1)
         if line_index > len(a:new_lines)
            call setline(line_index, "")
         else
            call setline(line_index, a:new_lines[line_index-1])
         endif
      endfor
   else
      for line_index in range(1, len(a:new_lines), 1)
         call setline(line_index, a:new_lines[line_index-1])
      endfor
   endif
endfunction

"===============================================================
"        Get Instance Name and its Path from Comments
"===============================================================
function GetInsts()
   let insts = {}
   for line in getline(1, line("$"))
      if line =~ '^\s*//\s*\<Instance\>:'
         let line = substitute(line, '^\s*//\s*\<Instance\>:\s*', "", "")
         let line = substitute(line, '\s*$', "", "")
         let insts_name = substitute(line, '^.*/', "", "")
         let insts_name = substitute(insts_name, '\.v$', "", "")
         call extend(insts, {insts_name : line})
      endif
   endfor
   return insts
endfunction

"===============================================================
"       Remove Comments and Functions from Current Buffer
"===============================================================
function Filter(lines)
   let aft_filter = []
   let line_index = 1
   while line_index <= len(a:lines)
      let line = a:lines[line_index-1]
      let line = substitute(line, '//.*$', "", "")
      if line =~ '^.*/\*' && line !~ '\*/.*$'
         let line = substitute(line, '/\*.*$', "", "")
         call add(aft_filter, line)
         let line_index = line_index + 1
         let line = a:lines[line_index-1]
         while line !~ '\*/.*$'
            let line_index = line_index + 1
            let line = a:lines[line_index-1]
         endwhile
      elseif line =~ '^\s*\<function\>'
         let line_index = line_index + 1
         let line = a:lines[line_index-1]
         while line !~ '^\s*\<endfunction\>'
            let line_index = line_index + 1
            let line = a:lines[line_index-1]
         endwhile
      elseif line =~ '^\s*\<endmodule\>'
         call add(aft_filter, line)
         break
      else
         if line !~ '^.*/\*'
            let line = substitute(line, '^.*\*/', "", "")
         endif
         let line = substitute(line, '^\s*\<endfunction\>', "", "")
         let line_index = line_index + 1
         if line != ""
            call add(aft_filter, line)
         endif
      endif
   endwhile
   return aft_filter
endfunction

"===============================================================
"        Get Inputs and Ouputs from a Instance
"===============================================================
function GetIO(lines, inst_input, inst_output)
   let max_len = []
   let prefix_max_len = 0
   let suffix_max_len = 0
   for line_inst in a:lines
      if line_inst =~ '^\s*\<input\>.*\['
         let line_inst = substitute(line_inst, '^\s*\<input\>', "", "")
         let line_inst = substitute(line_inst, '\s*[;,)][^,;]*$', "", "")
         let line_inst = substitute(line_inst, ',\|;', " ", "g")
         let line_names = matchstr(line_inst, '\]\s*\w\+$')
         let line_names = substitute(line_names, '^\]\s*', "", "")
         let port_names = split(line_names)
         let line_inst = substitute(line_inst, '\].*$', "]", "")
         let port_width = substitute(line_inst, '^.*\[', "[", "")
         for port_name in port_names
            if prefix_max_len < len(port_name)
               let prefix_max_len = len(port_name)
            endif
            if suffix_max_len < (len(port_name) + len(port_width))
               let suffix_max_len = len(port_name) + len(port_width)
            endif
            call extend(a:inst_input, {port_name : port_width}, "force")
         endfor
      elseif line_inst =~ '^\s*\<input\>'
         let line_inst = substitute(line_inst, '^\s*\<input\>', "", "")
         let line_inst = substitute(line_inst, '\s*[;,)][^,;]*$', "", "")
         let line_inst = substitute(line_inst, ',\|;', " ", "g")
         "let line_names = matchstr(line_inst, '\s\w\+$')
         let line_names = substitute(line_inst, '^\s', "", "")
         let port_names = split(line_names)
         for port_name in port_names
            if prefix_max_len < len(port_name)
               let prefix_max_len = len(port_name)
            endif
            if suffix_max_len < len(port_name)
               let suffix_max_len = len(port_name)
            endif
            call extend(a:inst_input, {port_name : ''}, "force")
         endfor
      elseif line_inst =~ '^\s*\(\<output\>\|\<inout\>\)\>.*\['
         let line_inst = substitute(line_inst, '^\s*\(\<output\>\|\<inout\>\)', "", "")
         let line_inst = substitute(line_inst, '\s*[;,)][^,;]*$', "", "")
         let line_inst = substitute(line_inst, ',\|;', " ", "g")
         let line_names = matchstr(line_inst, '\].*$')
         let line_names = substitute(line_names, '^\]\s*', "", "")
         let port_names = split(line_names)
         let line_inst = substitute(line_inst, '\]\s*\w\+$', "]", "")
         let port_width = substitute(line_inst, '^.*\[', "[", "")
         for port_name in port_names
            if prefix_max_len < len(port_name)
               let prefix_max_len = len(port_name)
            endif
            if suffix_max_len < (len(port_name) + len(port_width))
               let suffix_max_len = len(port_name) + len(port_width)
            endif
            call extend(a:inst_output, {port_name : port_width}, "force")
         endfor
      elseif line_inst =~ '^\s*\(\<output\>\|\<inout\>\)'
         let line_inst = substitute(line_inst, '^\s*\(\<output\>\|\<inout\>\)', "", "")
         let line_inst = substitute(line_inst, '\s*[;,)][^,;]*$', "", "")
         let line_inst = substitute(line_inst, ',\|;', " ", "g")
         "let line_names = matchstr(line_inst, '\s\w\+$')
         let line_names = substitute(line_inst, '^\s*', "", "")
         let port_names = split(line_names)
         for port_name in port_names
            if prefix_max_len < len(port_name)
               let prefix_max_len = len(port_name)
            endif
            if suffix_max_len < len(port_name)
               let suffix_max_len = len(port_name)
            endif
            call extend(a:inst_output, {port_name : ''}, "force")
         endfor
      endif
   endfor
   call add(max_len, prefix_max_len)
   call add(max_len, suffix_max_len)
   return max_len
endfunction
"===============================================================
"        Automatic Argument Generation
"===============================================================
function KillAutoArg()
   let aft_kill = []
   let line_index = 1
   let line = ""
   while line_index <= line("$") 
      let line = getline(line_index)
      if line =~ '^\s*\<module\>' && line =~ '\<\(autoarg\|AUTOARG\)\>\*/\s*$'
         call add(aft_kill, line.");")
         let line_index = line_index + 1
         while line !~ ');\s*$' && line_index < line("$") && line !~ '^\s*\<endmodule\>'
            let line_index = line_index + 1
            let line = getline(line_index)
         endwhile
         let line_index = line_index + 1
      elseif line =~ '^\s*\<endmodule\>'
         call add(aft_kill, line)
         break
      else
         call add(aft_kill, line)
         let line_index = line_index + 1
      endif
   endwhile
   call UpdateBuf(aft_kill)
endfunction

function AutoArg()
   let total_line = line("$")
   let inputs = []
   let outputs = []
   let line_index = 1
   call KillAutoArg()
   let lines = Filter(getline(1, line("$")))
   for line in lines
      if line =~ '^\s*\<input\>'
         let line = substitute(line, '^\s*\<input\>\s*\(\[.*:.*\]\)*\s*', "", "")
         let line = substitute(line, ';.*$', "", "")
         call add(inputs, line)
      elseif line =~ '^\s*\<output\>'
         let line = substitute(line, '^\s*\<output\>\s*\(\[.*:.*\]\)*\s*', "", "")
         let line = substitute(line, ';.*$', "", "")
         call add(outputs, line)
      endif
   endfor
   let line_index = 1
   let aft_arg = []
   for line in getline(1, total_line)
       if line =~ '/\*.*\<\(autoarg\|AUTOARG\)\>.*'
          let line = substitute(line, ').*', "", "")
          call add(aft_arg, line)
          call add(aft_arg, b:vlog_arg_margin."//Inputs")
          let input_col = len(b:vlog_arg_margin)
          let signal_line = b:vlog_arg_margin
          for signal_index in range(len(inputs))
             if input_col > b:vlog_max_col
                call add(aft_arg, signal_line)
                let signal_line = b:vlog_arg_margin. inputs[signal_index] . ", "
                let input_col = 3 + strlen(inputs[signal_index])
             else
                let signal_line = signal_line . inputs[signal_index] . ", "
                let input_col = input_col + strlen(inputs[signal_index])
             endif
          endfor
          call add(aft_arg, signal_line)
          call add(aft_arg, "")
          call add(aft_arg, b:vlog_arg_margin."//Outputs")
          let output_col = len(b:vlog_arg_margin)
          let signal_line = b:vlog_arg_margin
          for signal_index in range(len(outputs)-1)
             if output_col > b:vlog_max_col
                call add(aft_arg, signal_line)
                let signal_line = b:vlog_arg_margin . outputs[signal_index] . ", "
                let output_col = len(b:vlog_arg_margin) + strlen(outputs[signal_index])
             else
                let signal_line = signal_line . outputs[signal_index] . ", "
                let output_col = output_col + strlen(outputs[signal_index])
             endif
          endfor
          let signal_line = signal_line . outputs[len(outputs)-1] . ");"
          call add(aft_arg, signal_line)
       elseif line =~ '^\s*\<endmodule\>'
          call add(aft_arg, line)
          break
       else
          call add(aft_arg, line)
       endif
    endfor
    call UpdateBuf(aft_arg)
endfunction

"===============================================================
"        Automatic Instance Generation
"===============================================================
function CalMargin(max_len, cur_len)
   let margin = ""
   for i in range(1, a:max_len-a:cur_len+1, 1)
      let margin = margin." "
   endfor
   return margin
endfunction

function KillAutoInst()
   let line_index = 1
   let aft_kill = []
   while line_index <= line("$")
      let line = getline(line_index)
      if line =~ '/\*\<autoinst\>\*/\s*$'
         let line = line . ");"
         call add(aft_kill, line)
         let line_index = line_index + 1
         let line = getline(line_index)
         while line !~ ');$' && line_index < line("$")
            let line_index = line_index + 1
            let line = getline(line_index)
         endwhile
         let line_index = line_index + 1
      elseif line =~ '^\s*\<endmodule\>'
         call add(aft_kill, line)
         break
      else
         call add(aft_kill, line)
         let line_index = line_index + 1
      endif
   endwhile
   call UpdateBuf(aft_kill)
endfunction

function AutoInst()
   let aft_inst = []
   let max_len = []
   let insts = GetInsts()
   if insts == {}
      echo "No Instance found!"
      return
   endif
   call KillAutoInst()
   for line in getline(1, line("$"))
      if line =~ '(/\*\<\(autoinst\|AUTOINST\)\>\s*\*/)\s*;.*$'
         let tmp = split(line)
         let inst_name = tmp[0]
         if has_key(insts, inst_name)
            let inst_file = insts[inst_name]
         else
            echo "Has not found the instance: ".inst_name."'s file!"
            return
         endif
         let inst_input = {}
         let inst_output = {}
         let prefix_max_len = 0
         let suffix_max_len = 0
         let lines = Filter(readfile(inst_file))
         let max_len = GetIO(lines, inst_input, inst_output)
         let prefix_max_len = max_len[0]
         let suffix_max_len = max_len[1]
         let line = substitute(line, ');\s*', "", "")
         call add(aft_inst, line)
         call add(aft_inst, b:vlog_inst_margin."//Inputs")
         for ports in keys(inst_input)
            let prefix_margin = CalMargin(prefix_max_len, len(ports))
            let suffix_margin = CalMargin(suffix_max_len, len(ports)+len(inst_input[ports]))
            call add(aft_inst, b:vlog_inst_margin.".".ports.prefix_margin."(".ports.inst_input[ports].suffix_margin."),")
         endfor
         call add(aft_inst, b:vlog_inst_margin."//Outputs")
         for ports in keys(inst_output)
            let prefix_margin = CalMargin(prefix_max_len, len(ports))
            let suffix_margin = CalMargin(suffix_max_len, len(ports)+len(inst_output[ports]))
            call add(aft_inst, b:vlog_inst_margin.".".ports.prefix_margin."(".ports.inst_output[ports].suffix_margin."),")
         endfor
         let line = remove(aft_inst, -1)
         let line = substitute(line, "),", "));", "")
         call add(aft_inst, line)
      elseif line =~ '^\s*\<endmodule\>'
         call add(aft_inst, line)
         break
      else
         call add(aft_inst, line)
      endif
   endfor
   call UpdateBuf(aft_inst)
endfunction

"===============================================================
"        Automatic Signal Definition Generation
"===============================================================
function UserDef(lines)
   let user_def = {}
   for line in a:lines
      if line =~ '^\s*\<output\>\s*\<reg\>' || line =~ '^\s*\<output\>\s*\<signed\>'
         let line = substitute(line, '\s*[;,)].*$', "", "")
         let signal_name = matchstr(line, '\(\]\|\s\)\w\+$')
         let signal_name = substitute(signal_name, '^\(\]\|\s\)', "", "")
         call extend(user_def, {signal_name : ''})
      elseif line =~ '^\s*\(\<wire\>\|\<reg\>\|\<genvar\>\|\<integer\>\)'
         let signal_name = substitute(line, '\s*\[[^\[\]]*\]\s*;', ";", "")
         let signal_name = substitute(signal_name, '\s*;.*$', "", "")
         let signal_name = matchstr(signal_name, '\(\]\|\s\)\w\+$')
         let signal_name = substitute(signal_name, '^\(\]\|\s\)', "", "")
         call extend(user_def, {signal_name : line})
      endif
   endfor
   return user_def
endfunction

function PushSignal(signals, signal_name, signal_msb, signal_width, max_len, user_def)
   if has_key(a:user_def, a:signal_name) == 1
      return a:max_len
   " Signal width comes from the right part of the assignmnet
   elseif a:signal_msb == ""
      if has_key(a:signals, a:signal_name) == 1
         if a:signals[a:signal_name] =~ '\d\+'
            if str2nr(a:signals[a:signal_name], 10) < (a:signal_width-1)
               call extend(a:signals, {a:signal_name : a:signal_width-1}, "force")
            endif
         endif
      else
         call extend(a:signals, {a:signal_name : a:signal_width-1}, "force")
      endif
   " Signal width comes from the left part of the assignment
   " if MSB is a parameter then update value with it
   elseif a:signal_msb !~ '^\d\+$'
      call extend(a:signals, {a:signal_name : a:signal_msb}, "force")
   elseif has_key(a:signals, a:signal_name) != 1
      call extend(a:signals, {a:signal_name : a:signal_msb}, "force")
   " if the Old value is a parameter do not update
   elseif a:signals[a:signal_name] !~ '^[a-zA-z].*'
      if a:signal_msb == 0
         call extend(a:signals, {a:signal_name : 0}, "force")
      elseif str2nr(a:signals[a:signal_name], 10) < a:signal_msb
         call extend(a:signals, {a:signal_name : a:signal_msb}, "force")
      endif
   endif
   if a:max_len < (len(a:signal_msb) + 4)
      return len(a:signal_msb) + 4
   elseif a:max_len < (len(a:signal_width) + 4)
      return len(a:signal_width) + 4
   else
      return a:max_len
   endif
endfunction

function GetInstExpress(insts)
   if a:insts == []
      return '^//'
   endif
   let express = '^\s*\<\('
   for i in range(0, len(a:insts)-2, 1)
      let express = express.a:insts[i].'\|'
   endfor
   let express = express.a:insts[len(a:insts)-1].'\)\>'
   return express
endfunction

function KillAutoDef()
   let aft_kill = []
   let line_index = 1
   while line_index <= line("$")
      let line = getline(line_index)
      if line == "// Define flip-flop registers here"
         let line_index = line_index + 1
         let line = getline(line_index)
         while line != "// End of automatic define"
            let line_index = line_index + 1
            let line = getline(line_index)
         endwhile
         let line_index = line_index + 1
      else
         call add(aft_kill, line)
         let line_index = line_index + 1
      endif
   endwhile
   if len(aft_kill) < line("$")
      for line_index in range(1, line("$"))
         if line_index > len(aft_kill)
            call setline(line_index, "")
         else
            call setline(line_index, aft_kill[line_index-1])
         endif
      endfor
   else
      for line_index in range(1, len(aft_kill), 1)
         call setline(line_index, aft_kill[line_index-1])
      endfor
   endif
endfunction

function AutoDef()
   let ff_reg = {}
   let comb_reg = {}
   let inst_wire = {}
   let wire = {}
   let aft_def = []
   let max_len = 0
   let line_index = 1
   let signal_name = ""
   let signal_msb = ""
   let signal_width = ""
   call KillAutoDef()
   let insts = GetInsts()
   let inst_express = GetInstExpress(keys(insts))
   let lines = Filter(getline(1, line("$")))
   " Find Signals Declared by User
   let user_def = UserDef(lines)
   " Get Flip-flop Reg Signals
   while line_index <= len(lines)
      let line = lines[line_index-1]
      if line =~ '^\s*\<always\>\s*@\s*(\s*\<\(posedge\|negedge\)\>'
         let line_index = line_index + 1
         let line = lines[line_index-1]
         " Break meet another always block, assign statement or instance
         while line !~ '^\s*\<always\>' && line !~ '^\s*\<assign\>' && line !~ '^\<end\>'
                    \&& line !~ '^\s*\<endmodule\>'
            " Remove if(...)
            let line = substitute(line, '\<if\>\s*(.*)', " ", "")
            " Remove ... ?
            let line = substitute(line, '\s\+\w\+\s*?', " ", "g")
            " Remove (...)?
            let line = substitute(line, '([^()]*)\s*?', " ", "g")
            " Remove )
            let line = substitute(line, ")", "", "g")
            if line =~ '.*<=.*'
               let line = matchstr(line, '\s\+\w\+\(\[.*\]\)*\s*<=.*\(;\|:\)')
               let line = substitute(line, '^\s*', "", "")
               let line = substitute(line, '\(;\|:\)$', "", "")
               let signal_name = substitute(line, '\s*<=.*', "", "")
               " Match signal[M:N]
               if signal_name =~ ':.*]$'
                  let signal_msb = substitute(signal_name, '\s*:.*$', "", "")
                  let signal_msb = substitute(signal_msb, '^.*[\s*', "", "")
                  let signal_name = substitute(signal_name, '[.*$', "", "")
                  let max_len = PushSignal(ff_reg, signal_name, signal_msb, "", max_len, user_def)
               " Match signal <= M'hN or #1 M'dN or # `RD M'bN;
               elseif line =~ "^\\s*\\w\\+\\s*<=\\s*\\(#\\s*`*\\w*\\)*\\s\\+\\d\\+'\\(b\\|h\\|d\\).*"
                  let signal_width = substitute(line, "^\\s*\\w\\+\\s*<=\\s*\\(#\\s*`*\\w*\\)*\\s\\+", "", "")
                  let signal_width = substitute(signal_width, "'\\(b\\|h\\|d\\).*", "", "")
                  " delete [N]
                  let signal_name = substitute(signal_name, '[.*$', "", "")
                  let max_len = PushSignal(ff_reg, signal_name, "", signal_width, max_len, user_def)
               " Match signal[N]
               elseif signal_name =~ '\[.\+\]$'
                  let signal_msb = substitute(signal_name, ']$', "", "")
                  let signal_msb = substitute(signal_msb, '^.*[', "", "")
                  let signal_name = substitute(signal_name, '[.*$', "", "")
                  let max_len = PushSignal(ff_reg, signal_name, signal_msb, "", max_len, user_def)
               else
                  let max_len = PushSignal(ff_reg, signal_name, "", 1, max_len, user_def)
               endif
            endif
            let line_index = line_index + 1
            let line = lines[line_index-1]
         endwhile
         let line_index = line_index - 1
      " Get Combinational Reg Signals
      elseif line =~ '^\s*\<always\>'
         let line_index = line_index + 1
         let line = lines[line_index-1]
         while line !~ '^\s*\<always\>' && line !~ '^\s*\<assign\>' && line !~ '^\<end\>'
                   \&& line !~ '^\s*\<endmodule\>'
             " Remove if(...)
            let line = substitute(line, '\<if\>\s*(.*)', " ", "")
            " Remove ... ?
            let line = substitute(line, '\s\+\w\+\s*?', " ", "g")
            " Remove (...)?
            let line = substitute(line, '([^()]*)\s*?', " ", "g")
            " Remove )
            let line = substitute(line, ")", "", "g")
            if line =~ '.*=.*'
               let line = matchstr(line, '\s\+\w\+\(\[.*\]\)*\s*=.*\(;\|:\)*')
               let line = substitute(line, '^\s*', "", "")
               let line = substitute(line, '\(;\|:\)$', "", "")
               let signal_name = substitute(line, '\s*=.*', "", "")
               " Match signal[M:N]
               if signal_name =~ ':.*]$'
                  let signal_msb = substitute(signal_name, '\s*:.*$', "", "")
                  let signal_msb = substitute(signal_msb, '^.*[\s*', "", "")
                  let signal_name = substitute(signal_name, '[.*$', "", "")
                  let max_len = PushSignal(comb_reg, signal_name, signal_msb, "", max_len, user_def)
               " Match signal = M'hN;
               elseif line =~ "^\\s*\\w\\+\\s*=\\s*\\d\\+'\\\(b\\|h\\|d\\).*"
                  let signal_width = substitute(line, '^\s*\w\+\s*=\s*', "", "")
                  let signal_width = substitute(signal_width, "'\\(b\\|h\\|d\\).*", "", "")
                  let signal_name = substitute(signal_name, '[.*$', "", "")
                  let max_len = PushSignal(comb_reg, signal_name, "", signal_width, max_len, user_def)
               " Match signal[N]
               elseif signal_name =~ '\[\w\+\]$'
                  let signal_msb = substitute(signal_name, ']$', "", "")
                  let signal_msb = substitute(signal_msb, '^.*[', "", "")
                  let signal_name = substitute(signal_name, '[.*$', "", "")
                  let max_len = PushSignal(comb_reg, signal_name, signal_msb, "", max_len, user_def)
               else
                  let max_len = PushSignal(comb_reg, signal_name, "", 1, max_len, user_def)
               endif
            endif
            let line_index = line_index + 1
            let line = lines[line_index-1]
         endwhile
         let line_index = line_index - 1
      " Get Wires
      elseif line =~ '^\s*\<assign\>'
         let line = substitute(line, '\s\+\w\+\s*?', " ", "g")
         let line = substitute(line, '(.*)\s*?', " ", "g")
         let signal_name = substitute(line, '^\s*\<assign\>\s*', "", "")
         let signal_name = substitute(signal_name, '\s*=.*', "", "g")
         "Match: signal[M:N]
         if signal_name =~ ':.*]\s*$'
            let signal_msb = substitute(signal_name, '\s*:.*$', "", "")
            let signal_msb = substitute(signal_msb, '^.*[\s*', "", "")
            let signal_name = substitute(signal_name, '[.*$', "", "")
            if has_key(inst_wire, signal_name)
               let max_len = PushSignal(inst_wire, signal_name, signal_msb, "", max_len, user_def)
            else
               let max_len = PushSignal(wire, signal_name, signal_msb, "", max_len, user_def)
            end
         " Match: signal = M'hN;
         elseif line =~ "^\\s*\\<assign\\>\\s*\\w\\+\\s*=\\s*\\d\\+'\\(b\\|h\\|d\\).*"
            let signal_width = substitute(line, '^\s*\<assign\>\s\+\w\+\s*=\s*', "", "")
            let signal_width = substitute(signal_width, "'\\(b\\|h\\|d\\).*", "", "")
            let signal_name = substitute(signal_name, '[.*$', "", "")
            let max_len = PushSignal(wire, signal_name, "", signal_width, max_len, user_def)
         elseif signal_name =~ '\[.\+\]$'
            let signal_msb = substitute(signal_name, ']$', "", "")
            let signal_msb = substitute(signal_msb, '^.*[', "", "")
            let signal_name = substitute(signal_name, '[.*$', "", "")
            if has_key(inst_wire, signal_name)
               let max_len = PushSignal(inst_wire, signal_name, signal_msb, "", max_len, user_def)
            else
               let max_len = PushSignal(wire, signal_name, signal_msb, "", max_len, user_def)
            endif
         else
            let max_len = PushSignal(wire, signal_name, "", 1, max_len, user_def)
         endif
         let line_index = line_index + 1
      " Get Instance Ouputs
      elseif line =~ inst_express && insts != {}
         let inst_name = matchstr(line, '^\s*\w\+')
         let inst_name = substitute(inst_name, '^\s*', "", "")
         if insts == {}
            echo "No Instances have been indicated!"
            return
         elseif has_key(insts, inst_name)
            let inst_lines = readfile(insts[inst_name])
            let inst_inputs = {}
            let inst_outputs = {}
            call GetIO(inst_lines, inst_inputs, inst_outputs)
         else
            echo "Instance ".inst_name."'s file path does not indicate!"
            return
         endif
         " If there is port declaration
         if line =~ '^.*\.'
            let line = substitute(line, '^[^.]*\.', ".", "")
         " Read next line
         else
            let line_index = line_index + 1
            let line = lines[line_index-1]
         endif
         while line !~ '^\s*);.*'
            if line =~ '^.*\.'
               let inst_port = matchstr(line, '^\s*\.\w\+\s*(')
               let inst_port = substitute(inst_port, '^\s*\.', "", "")
               let inst_port = substitute(inst_port, '\s*($', "", "")
               let line = substitute(line, '^\s*\.\w\+\s*(\s*', "", "")
               let inst_net = matchstr(line, '^[^)]*)')
               let inst_net = substitute(inst_net, '\s*)', "", "")
               let line = substitute(line, '^[^)]*),*\s*', "", "")
               if has_key(inst_outputs, inst_port)
                  let net_name = substitute(inst_net, '[.*$', "", "")
                  let inst_net = substitute(inst_net, '^.*[', "[", "")
                  if net_name == inst_net
                     if has_key(wire, net_name)
                        let max_len = PushSignal(wire, net_name, "", 1, max_len, user_def)
                     else
                        let max_len = PushSignal(inst_wire, net_name, "", 1, max_len, user_def)
                     endif
                  else
                     let inst_net = substitute(inst_net, ':.*$', "", "")
                     let inst_net = substitute(inst_net, '].*$', "", "")
                     let inst_net = substitute(inst_net, '^[', "", "")
                     if has_key(wire, net_name)
                        let max_len = PushSignal(wire, net_name, inst_net, "", max_len, user_def)
                     else
                        let max_len = PushSignal(inst_wire, net_name, inst_net, "", max_len, user_def)
                     endif
                  endif
               endif
               if line =~  '^\s*);'
                  break
               elseif line !~ '^.*\.'
                  let line_index = line_index + 1
                  let line = lines[line_index-1]
               endif
            else
               let line_index = line_index + 1
               if line_index == len(lines)
                  break
               endif
               let line = lines[line_index-1]
            endif
         endwhile 
         let line_index = line_index + 1
         let line = lines[line_index-1]
      elseif line =~ '^\s*\<endmodule\>'
         break
      else
         let line_index = line_index + 1
         let line = lines[line_index-1]
      endif
   endwhile
   for line in getline(1, line("$"))
      if line =~ '^\s*/\*\<\(autodefine\|AUTODEFINE\)\>\*/'
         call add(aft_def, line)
         call add(aft_def, "// Define flip-flop registers here")
         for regs in sort(keys(ff_reg))
            let margin = CalMargin(max_len, len(ff_reg[regs]))
            if ff_reg[regs] == "0"
               call add(aft_def, "reg       ".margin.regs.";    //")
            else
               call add(aft_def, "reg  [".ff_reg[regs].":0]".margin.regs.";    //")
            endif
         endfor
         call add(aft_def, "// Define combination registers here")
         for regs in sort(keys(comb_reg))
            let margin = CalMargin(max_len, len(comb_reg[regs]))
            if comb_reg[regs] == "0"
               call add(aft_def, "reg       ".margin.regs.";    //")
            else
               call add(aft_def, "reg  [".comb_reg[regs].":0]".margin.regs.";    //")
            endif
         endfor
         call add(aft_def, "// Define wires here")
         for wires in sort(keys(wire))
            let margin = CalMargin(max_len, len(wire[wires]))
            if wire[wires] == "0"
               call add(aft_def, "wire      ".margin.wires.";    //")
            else
               call add(aft_def, "wire [".wire[wires].":0]".margin.wires.";    //")
            endif
         endfor
         call add(aft_def, "// Define instances' ouput wires here")
         for wires in sort(keys(inst_wire))
            let margin = CalMargin(max_len, len(inst_wire[wires]))
            if inst_wire[wires] == "0"
               call add(aft_def, "wire      ".margin.wires.";    //")
            else
               call add(aft_def, "wire [".inst_wire[wires].":0]".margin.wires.";    //")
            endif
         endfor
         call add(aft_def, "// End of automatic define")
         if user_def != {}
            for defined in sort(keys(user_def))
               if user_def[defined] != ""
                  call add(aft_def, user_def[defined])
               endif
            endfor
         endif
      elseif line !~ '^\s*\<\(wire\|reg\|genvar\|integer\)\>'
         call add(aft_def, line)
      "else
      "   call add(aft_def, line)
      endif
   endfor
   "for line_index in range(1, len(aft_def), 1)
   "   call setline(line_index, aft_def[line_index-1])
   "endfor
   echo len(aft_def)
   call UpdateBuf(aft_def)
endfunction

"===============================================================
"        Automatic Sensitive List Generation
"===============================================================
function KillAutoSense()
   let line_index = 0
   let aft_kill = []
   while line_index <= line("$")
      let line = getline(line_index)
      if line =~ '^\s*\<always\>\s*@\s*(\s*/\*\s*\<autosense\>'
         if line =~ ').*$'
            let line = substitute(line, '^.*)', "", "")
            call add(aft_kill, "always @(/*autosense*/)".line)
         else
            while line !~ ').*$' && line_index <= line("$")
               let line_index = line_index + 1
               let line = getline(line_index)
            endwhile
            if line !~ ').*$'
               echo "No ) found!"
               return
            else
               let line = substitute(line, '^.*)', "", "")
               call add(aft_kill, "always @(/*autosense*/)".line)
            endif
         endif
         let line_index = line_index + 1
      else
         call add(aft_kill, line)
         let line_index = line_index + 1
      endif
   endwhile
   if len(aft_kill) < line("$")
      for line_index in range(1, line("$"), 1)
         if line_index > len(aft_kill)
            call setline(line_index, "")
         else
            call setline(line_index, aft_kill[line_index-1])
         endif
      endfor
   else
      for line_index in range(1, len(aft_kill), 1)
         call setline(line_index, aft_kill[line_index-1])
      endfor
   endif      
endfunction

function GetPars()
   let parameters = []
   let line_index = 0
   while line_index <= line("$")
      let line = getline(line_index)
      if line =~ '^\s*\<parameter\>'
         while line !~ ';.*$'
            let line = substitute(line, '^\s*\(\<parameter\>\)*\s*', "", "")
            let line = substitute(line, '\s*=.*$', "", "")
            if line != ""
               call add(parameters, line)
            endif
            let line_index = line_index + 1
            let line = getline(line_index)
         endwhile
         let line = substitute(line, '^\s*\(\<parameter\>\)*\s*', "", "")
         let line = substitute(line, '\s*=.*$', "", "")
         if line != ""
            call add(parameters, line)
         endif
         let line_index = line_index + 1
         let line = getline(line_index)
      else
         let line_index = line_index + 1
      endif
   endwhile
   if parameters != []
      let express = '\<\('
      for i in range(1, len(parameters)-1, 1)
         let express = express.parameters[i-1].'\|'
      endfor
      let express = express.parameters[len(parameters)-1].'\)\>'
   else
      let express = '^//'
   endif
   return express
endfunction

function GetFuns()
   let funs = []
   let line_index = 0
   let line = ""
   while line_index <= line("$") && line !~ '^\s*\<endmodule\>'
      let line = getline(line_index)
      if line =~ '^\s*\<function\>\s*'
         let line = substitute(line, '^\s*\<function\>\s*', "", "")
         let line = substitute(line, ';.*$', "", "")
         let line = substitute(line, '^\s\+(.*$', "", "")
         if line != ""
            call add(funs, line)
         endif
      endif
      let line_index = line_index + 1
   endwhile
   return funs
endfunction

function AutoSense()
   let line_index = 1
   let vlog_keys = '\(\<if\>\|\<else\>\|\<case\>\|\<casex\>\|\<casez\>\|\<begin\>\|\<end\>\|\<endcase\>\|\<default\>\)'
   let vlog_opts = '[=<>!~\^\-+*?:|&{}%(;),\[\]]'
   let vlog_const0 = "\\s\\+\\d\\+'[bBdDhHoO][0-9a-fA-F_xzXZ]\\+"
   let vlog_const1 = '\s\+\d\+'
   let vlog_pars = []
   let vlog_funs = []
   let inputs = []
   let sense_list = []
   call KillAutoSense()
   let insts = GetInsts()
   let inst_express = GetInstExpress(keys(insts))
   let lines = Filter(getline(1, line("$")))
   let pars_express = GetPars()
   while line_index <= len(lines)
      let line = lines[line_index-1]
      if line =~ '^\s*\<always\>\s*@\s*(\s*/\*\s*\<autosense\>\s*\*/)'
         let inputs = []
         let line_index = line_index + 1
         let line = lines[line_index-1]
         while line !~ '^\s*\<always\>' && line !~ '^\s*\<assign\>' && line !~ inst_express
                   \  && line !~ '^\s*\<endmodule\>'
	    if line =~ '^[^(]*='
	       let left_part = matchstr(line, '^[^(]*=')
	       let left_part = substitute(left_part, '\s*=$', "", "")
	       let left_part = matchstr(left_part, '\[\s*\w*\s*\]')
	       let left_part = substitute(left_part, '[\[\]]', "", "g")
	       let left_part = substitute(left_part, pars_express, "", "g")
	       let left_part = substitute(left_part, '\s', "", "g")
	       if left_part =~ '\w\+'
		  call add(inputs, left_part)
	       endif
	       let line = substitute(line, '^[^(]*=\s*', " ", "")
	    endif
            " Remove [*]
            let line = substitute(line, '\[[^\[\]]*\]', "", "g")
            " Remove Verilog Defines, Constants, Key Words and Operators
	    let line = substitute(line, pars_express, " ", "g")
            let line = substitute(line, vlog_opts, " ", "g")            
            let line = substitute(line, vlog_const0, " ", "g")
            let line = substitute(line, vlog_const1, " ", "g")
            let line = substitute(line, "\\s\\+`\\w\\+", " ", "g")
            let line = substitute(line, vlog_keys, "", "g")
            " Get Inputs
            let input_items = split(line)
            if input_items != []
               for input_item in input_items
                  if count(inputs, input_item) == 0
                     call add(inputs, input_item)
                  endif
               endfor
            endif
            let line_index = line_index + 1
            let line = lines[line_index-1]
         endwhile
         let line_index = line_index - 1
         call extend(sense_list, inputs)
         call add(sense_list, "|")
      else
         let line_index = line_index + 1
      endif
   endwhile
   let line_index = 1
   let aft_sense = []
   let list_line = ""
   let margin = "        "
   let list_index = 0
   while line_index <= line("$")
      let line = getline(line_index)
      if line =~ '^\s*\<always\>\s*@\s*(\s*/\*\s*\<autosense\>\s*\*/)'
         let tmp = matchstr(line, ').*$')
         let line = substitute(line, ').*$', "", "")
         let list_line = ""
         "call add(aft_sense, line)
         let line_col = 20
         let list_item = sense_list[list_index]
         while list_item != "|"
            if list_line != ""
               let list_line = list_line." or ".list_item
            else
               let list_line = line.list_item
            endif
            let line_col = line_col - len(list_item)
            if line_col < 0
               call add(aft_sense, list_line)
               let list_line = ""
               let line_col = 30
               let line = margin." or "
            endif
            let list_index = list_index + 1
            let list_item = sense_list[list_index]
         endwhile
         let list_index = list_index + 1
         let list_line = list_line.tmp
         call add(aft_sense, margin.list_line)
      else
         call add(aft_sense, line)
      endif
      let line_index = line_index + 1
   endwhile
   let line_index = 1
   if len(aft_sense) < line("$")
      for line_index in range(1, line("$"), 1)
         if line_index > len(aft_sense)
            call setline(line_index, "")
         else
            call setline(line_index, aft_sense[line_index-1])
         endif
      endfor
   else
      for line_index in range(1, len(aft_sense), 1)
         call setline(line_index, aft_sense[line_index-1])
      endfor
   endif
endfunction

"===============================================================
"        Get Drivings
"===============================================================
function GetDriving(lines, driving_nets)
   for line in a:lines
      if line =~ '^\s*\<\(reg\|wire\)\>'
         let net = substitute(line, ';.*$', "", "")
         let net = matchstr(net, '\w*$')
         let net = substitute(net, '^[\s\]]', "", "")
         call add(a:driving_nets, net)
      elseif line =~ '^\s*\<input\>'
         let net = substitute(line, '[,;)].*$', "", "")
         let net = matchstr(net, '\w*$')
         let net = substitute(net, '^[\]\s]', "", "")
         call add(a:driving_nets, net)
      endif
   endfor
endfunction

"===============================================================
"        Get Loadings
"===============================================================
function GetLoading(lines, loading_nets, driving_nets, insts, insts_express, pars_express)
   let line_index = 1
   let vlog_keys = '\(\<if\>\|\<else\>\|\<case\>\|\<casex\>\|\<casez\>\|\<begin\>\|\<or\>'.
                           \'\|\<end\>\|\<endcase\>\|\<default\>\|\<always\>\|\<posedge\>\|\<negedge\>\)'
   let vlog_opts = '[=<>!~\^\-+*?:|&{}%(;),@#\[\]]'
   let vlog_const0 = "\\s\\+\\d\\+'[bBdDhHoO][0-9a-fA-F_xzXZ]\\+"
   let vlog_const1 = '\s\+\d\+'
   while line_index <= len(a:lines)
      let line = a:lines[line_index-1]
      if line =~ '\s*\<always\>\s*@\s*(\s*\<\(posedge\|negedge\)\>'
         let line = substitute(line, vlog_keys, " ", "g")
         let line = substitute(line, vlog_opts, " ", "g")
         for input_item in split(line)
            if count(a:loading_nets, input_item) == 0
               call add(a:loading_nets, input_item)
            endif
         endfor
         let line_index = line_index + 1
         let line = a:lines[line_index-1]
         while line !~ '\s*\<always\>' && line !~ '^\s*\<assign\>' && line !~ '^\<end\>'
                    \&& line !~ '^\s*\<endmodule\>' && line !~ a:insts_express
            let left_part = line
            " Find <= outside ()
            if left_part =~ '^[^(]*<='
               let left_part = matchstr(line, '^[^(]*<=')
               let left_part = matchstr(left_part, '\[.*\]')
               if left_part != ""
                  let left_part = substitute(left_part, a:pars_express, " ", "g")
                  let left_part = substitute(left_part, vlog_const0, " ", "g")
                  let left_part = substitute(left_part, vlog_const1, " ", "g")
                  let left_part = substitute(left_part, "\\s\\+`\\w\\+\\s\\+", " ", "g")
                  let left_part = substitute(left_part, vlog_keys, "", "g")
                  let left_part = substitute(left_part, '\[[^\[\]]*\]', "", "g")
                  if left_part != ""
                     if count(a:loading_nets, left_part) == 0
                        call add(a:loading_nets, left_part)
                     endif
                  endif
               endif
               let line = substitute(line, '^[^(]*<=\s*', " ", "")
            endif
            let line = substitute(line, a:pars_express, "  ", "g")
            " Remove Verilog Defines, Constants, Key Words and Operators
            let line = substitute(line, vlog_opts, "  ", "g")
            let line = substitute(line, vlog_const0, "  ", "g")
            let line = substitute(line, vlog_const1, "  ", "g")
            let line = substitute(line, "\\s\\+`\\w\\+", "  ", "g")
            let line = substitute(line, vlog_keys, "", "g")
            "Get Loading
            let input_items = split(line)
            if input_items != []
               for input_item in input_items
                  if count(a:loading_nets, input_item) == 0
                     call add(a:loading_nets, input_item)
                  endif
               endfor
            endif
            let line_index = line_index + 1
            let line = a:lines[line_index-1]
         endwhile
         let line_index = line_index -1
      elseif line =~ '^\s*\<always\>\s*'
         let inputs = []
         let line_index = line_index + 1
         let line = a:lines[line_index-1]
         while line !~ '^\s*\<always\>' && line !~ '^\s*\<assign\>' && line !~ a:insts_express
                   \  && line !~ '^\s*\<endmodule\>'
            let left_part = line
            " Find <= outside ()
            if left_part =~ '^[^(]*='
               let left_part = matchstr(line, '^[^(]*=')
               let left_part = matchstr(left_part, '\[.*\]')
               if left_part != ""
                  let left_part = substitute(left_part, a:pars_express, "  ", "g")
                  let left_part = substitute(left_part, vlog_const0, "  ", "g")
                  let left_part = substitute(left_part, vlog_const1, "  ", "g")
                  let left_part = substitute(left_part, "\\s\\+`\\w\\+", "  ", "g")
                  let left_part = substitute(left_part, vlog_keys, "  ", "g")
                  let left_part = substitute(left_part, '\[[^\[\]]*\]', "", "g")
                  if left_part != ""
                     if count(a:loading_nets, left_part) == 0
                        call add(a:loading_nets, left_part)
                     endif
                  endif
               endif
               let line = substitute(line, '^[^(]*=\s*', "  ", "")
            endif
            let line = substitute(line, a:pars_express, "  ", "g")
            " Remove Verilog Defines, Constants, Key Words and Operators
            let line = substitute(line, vlog_opts, "  ", "g")
            let line = substitute(line, vlog_const0, "  ", "g")
            let line = substitute(line, vlog_const1, "  ", "g")
            let line = substitute(line, "\\s\\+`\\w\\+", " ", "g")
            let line = substitute(line, vlog_keys, "", "g")
            " Get Inputs
            let input_items = split(line)
            if input_items != []
               for input_item in input_items
                  if count(a:loading_nets, input_item) == 0
                     call add(a:loading_nets, input_item)
                  endif
               endfor
            endif
            let line_index = line_index + 1
            let line = a:lines[line_index-1]
         endwhile
         let line_index = line_index - 1
      elseif line =~ '^\s*\<assign\>\s'
         let line = substitute(line, '^\s*\<assign\>\s*', "", "")
         let left_part = substitute(line, '\s*=.*', "", "")
         let left_part = matchstr(left_part, '\[.*\]')
         if left_part != ""
            let left_part = substitute(left_part, a:pars_express, " ", "g")
            let left_part = substitute(left_part, vlog_const0, " ", "g")
            let left_part = substitute(left_part, vlog_const1, " ", "g")
            let left_part = substitute(left_part, "\\s\\+`\\w\\+\\s\\+", " ", "g")
            let left_part = substitute(left_part, vlog_keys, "", "g")
            let left_part = substitute(left_part, '\[[^\[\]]*\]', "", "g")
            if left_part != ""
               if count(a:loading_nets, left_part) == 0
                  call add(a:loading_nets, left_part)
               endif
            endif
         endif
         let line = substitute(line, '^[^(]*=\s*', " ", "")
         while line !~ ';\.*$'
            let line = substitute(line, a:pars_express, "  ", "g")
            " Remove Verilog Defines, Constants, Key Words and Operators
            let line = substitute(line, vlog_opts, "  ", "g")            
            let line = substitute(line, vlog_const0, "  ", "g")
            let line = substitute(line, vlog_const1, "  ", "g")
            let line = substitute(line, "\\s\\+`\\w\\+\\s\\+", "  ", "g")
            let line = substitute(line, vlog_keys, "", "g")
            " Get Inputs
            let input_items = split(line)
            if input_items != []
               for input_item in input_items
                  if count(a:loading_nets, input_item) == 0
                     call add(a:loading_nets, input_item)
                  endif
               endfor
            endif
            let line_index = line_index + 1
            let line = a:lines[line_index-1]
         endwhile
         let line = substitute(line, a:pars_express, " ", "g")
         " Remove Verilog Defines, Constants, Key Words and Operators
         let line = substitute(line, vlog_opts, " ", "g")            
         let line = substitute(line, vlog_const0, " ", "g")
         let line = substitute(line, vlog_const1, " ", "g")
         let line = substitute(line, "\\s\\+`\\w\\+\\s\\+", " ", "g")
         let line = substitute(line, vlog_keys, "", "g")
         " Get Inputs
         let input_items = split(line)
         if input_items != []
            for input_item in input_items
               if count(a:loading_nets, input_item) == 0
                  call add(a:loading_nets, input_item)
               endif
            endfor
         endif
         let line_index = line_index + 1
         let line = a:lines[line_index-1]
      elseif line =~ a:insts_express && a:insts != {}
         let inst_name = matchstr(line, '^\s*\w\+')
         let inst_name = substitute(inst_name, '^\s*', "", "")
         if a:insts == {}
            echo "No Instance have been indicated!"
            return
         elseif has_key(a:insts, inst_name)
            let inst_lines = readfile(a:insts[inst_name])
            let inst_inputs = {}
            let inst_outputs = {}
            call GetIO(inst_lines, inst_inputs, inst_outputs)
         else
            echo "Instance ".inst_name."'s file path does not indicate!"
            return
         endif
         if line =~ '^.*\.'
            let line = substitute(line, '^[^.]*\.', ".", "")
         else
            let line_index = line_index + 1
            let line = a:lines[line_index-1]
         endif
         while line !~ '^\s*);.*'
            if line =~ '^.*\.'
               let inst_port = matchstr(line, '^\s*\.\w\+\s*(')
               let inst_port = substitute(inst_port, '^\s*\.', "", "")
               let inst_port = substitute(inst_port, '\s*($', "", "")
               let line = substitute(line, '^\s*\.\w\+\s*(\s*', "", "")
               let inst_net = matchstr(line, '^[^)]*)')
               "echo line_net
               let inst_net = substitute(inst_net, '\s*)', "", "")
               let line = substitute(line, '^[^)]*),*\s*', "", "")
               if has_key(inst_inputs, inst_port)
                  let net_name = substitute(inst_net, '[.*$', "", "")
                  if net_name =~ '^\D\+' && count(a:loading_nets, net_name) == 0
                     call add(a:loading_nets, net_name)
                  endif
               endif
	       if has_key(inst_outputs, inst_port)
		  let net_name = substitute(inst_net, '[.*$', "", "")
		  if net_name =~ '^\D\+' && count(a:driving_nets, net_name) == 0
		     call add(a:driving_nets, net_name)
		  endif
	       endif
               if line =~  '^\s*);'
                  break
               elseif line !~ '^.*\.'
                  let line_index = line_index + 1
                  let line = a:lines[line_index-1]
               endif
            else
               let line_index = line_index + 1
               let line = a:lines[line_index-1]
            endif
         endwhile
         let line_index = line_index + 1
         let line = a:lines[line_index-1]
      elseif line =~ '^\s*\<output\>'
         let line = substitute(line, '[,;)].*$', "", "")
         let net_name = matchstr(line, '\w*$')
         if count(a:loading_nets, net_name) == 0
            call add(a:loading_nets, net_name)
         endif
         let line_index = line_index + 1
         let line = a:lines[line_index-1]
      elseif line =~ '^\s*\<endmodule\>'
         break
      else
         let line_index = line_index + 1
         let line = a:lines[line_index-1]
      endif
   endwhile
endfunction


function AutoCheck()
   let insts = GetInsts()
   let insts_express = GetInstExpress(keys(insts))
   let pars_express = GetPars()
   let lines = Filter(getline(1, line("$")))
   let loading_nets = []
   let driving_nets = []
   let fail = 0
   call GetDriving(lines, driving_nets)
   call GetLoading(lines, loading_nets, driving_nets, insts, insts_express, pars_express)
   for net in driving_nets
      if count(loading_nets, net) == 0
         echo "No Loading Net: ".net
	 let fail = 1
      endif
   endfor
   for net in loading_nets
      if count(driving_nets, net) == 0
         echo "No Driving Net: ".net
	 let fail = 1
      endif
   endfor
   if fail == 0
      echo "Pass!"
   endif
endfunction

function GetCfg(lines)
   let pars = {}
   for line in a:lines
      if line =~ '^\s*#\<data_width\>'
	 let line = substitute(line, '^\s*#\<data_width\>\s*', "", "")
	 call extend(pars, {'data_width' : line}, "force")
      elseif line =~ '^\s*#\<addr_width\>'
	 let line = substitute(line, '^\s*#\<addr_width\>\s*', "", "")
	 call extend(pars, {'addr_width': line}, "force")
      elseif line =~ '^\s*#\<clk_edge\>'
	 let line = substitute(line, '^\s*#\<clk_edge\>\s*', "", "")
	 call extend(pars, {'clk_edge' : line}, "force")
      elseif line =~ '^\s*#\<rst_edge\>'
	 let line = substitute(line, '^\s*#\<rst_edge\>\s*', "", "")
	 call extend(pars, {'rst_edge' : line}, "force")
      elseif line =~ '^\s*#\<clk\>'
	 let line = substitute(line, '^\s*#\<clk\>\s*', "", "")
	 call extend(pars, {'clk' : line}, "force")
      elseif line =~ '^\s*#\<rst\>'
	 let line = substitute(line, '^\s*#\<rst\>\s*', "", "")
	 call extend(pars, {'rst' : line}, "force")
      elseif line =~ '^\s*#\<reg_wr\>'
	 let line = substitute(line, '^\s*#\<reg_wr\>\s*', "", "")
	 call extend(pars, {'reg_wr' : line}, "force")
      elseif line =~ '^\s*#\<waddr\>'
	 let line = substitute(line, '^\s*#\<waddr\>\s*', "", "")
	 call extend(pars, {'waddr' : line}, "force")
      elseif line =~ '^\s*#\<raddr\>'
	 let line = substitute(line, '^\s*#\<raddr\>\s*', "", "")
	 call extend(pars, {'raddr' : line}, "force")
      elseif line =~ '^\s*#\<wdata\>'
	 let line = substitute(line, '^\s*#\<wdata\>\s*', "", "")
	 call extend(pars, {'wdata' : line}, "force")
      elseif line =~ '^\s*#\<rdata\>'
	 let line = substitute(line, '^\s*#\<rdata\>\s*', "", "")
	 call extend(pars, {'rdata' : line}, "force")
      elseif line =~ '^\s*#\<ffd\>'
         let line = substitute(line, '^\s*#\<ffd\>\s*', "", "")
         call extend(pars, {'ffd' : line}, "force")
      elseif line =~ '^\s*#\<testmode\>'
         let line = substitute(line, '^\s*#\<testmode\>\s*', "", "")
         call extend(pars, {'testmode' : line}, "force")
      endif
   endfor
   return pars
endfunction

function ClkGateCfg(lines)
   let i = 0
   let clk_gate = []
   while i < len(a:lines)
      let line = a:lines[i]
      if line =~ '^\s*#\<clk_gate\>'
         for v in range(1, 5, 1)
            let line = a:lines[i+v]
            call add(clk_gate, matchstr(line, '^\.*\w*'))
         endfor
         return clk_gate
      endif
      let i = i + 1
   endwhile
   return clk_gate
endfunction

function ReadReg(lines, pars, reg_list, reg_addr, reg_wr, inputs, outputs, reg_rdata, reg_scr, virtual_reg)
   let i = 0
   let rdata = ""
   let scr_rst = []
   let scr_wr = []
   let max_len = 0
   while i < len(a:lines)
      let line = a:lines[i]
      if line =~ '^\s*\*'
         let line = substitute(line, '.*', "\\L\\0", "")
         let reg_name = matchstr(line, '^\s*\*\w*')
         let reg_name = substitute(reg_name, '^\s*\*', "", "")
         call add(a:reg_list, reg_name)
         if len(reg_name) > max_len
            let max_len = len(reg_name)
         endif
         let line = substitute(line, '^\s*\*\w*\s*', "", "")
         call add(a:reg_scr, "//Reg: ".reg_name)
         if line =~ '^v\s*'
            let virtual = 1
         else
            let virtual = 0
         endif
         if line =~ '^g\s*'
            call extend(a:reg_wr, {reg_name : 'g'}, "force")
            call add(a:reg_scr, "always @(".a:pars['clk_edge']." ".reg_name."_clk or ".a:pars['rst_edge']." ".a:pars['rst'].") begin")
            call add(scr_wr, repeat(" ", b:vlog_ind)."else begin")
         elseif line !~ '^v\s*'
            call extend(a:reg_wr, {reg_name : 'w'}, "force")
            call add(a:reg_scr, "always @(".a:pars['clk_edge']." ".a:pars['clk']." or ".a:pars['rst_edge']." ".a:pars['rst'].") begin")
            call add(scr_wr, repeat(" ", b:vlog_ind). "else if (".reg_name."_wr) begin")
         endif
         if virtual == 0
            if a:pars['rst_edge'] == "posedge"
               call add(a:reg_scr, repeat(" ", b:vlog_ind)."if (".a:pars['rst'].") begin")
            else
               call add(a:reg_scr, repeat(" ", b:vlog_ind)."if (!".a:pars['rst'].") begin")
            endif
         endif
         let line = substitute(line, '^\(g\|v\)*\s*\<addr\>\s*=\s*', "", "")
         call extend(a:reg_addr, {reg_name : line}, "force")
         let i = i + 1
         let line = a:lines[i]
         while line !~ '^\s*\*' && i < len(a:lines)
            if line != ""
               let item = split(line)
               if item[1] =~ '.*-.*'
                  let width = eval(item[1]) + 1
               else
                  let width = 1
               endif
               if item[0] =~ '^\d'
                  let width = width."'d"
               else
                  let width = width."'"
               endif
               if item[2] == "reserved" || item[0] == '^\s*w'
                  let rdata = rdata . width . item[0] . ","
               else
                  let rdata = rdata . item[2] . ","
               endif
               if len(item) == 4
                  let io_name = substitute(item[2], '[.*$', "", "")
                  let io_msb = matchstr(item[2], '\[.*\]')
                  if io_msb == ""
                     let io_msb = 0
                  else
                     let io_msb = substitute(io_msb, '^[', "", "")
                     let io_msb = matchstr(io_msb, '\d\+')
                  endif
                  if item[3] == "O"
                     if has_key(a:outputs, io_name)
                        if str2nr(a:outputs[io_name]) < io_msb
                           call extend(a:outputs, {io_name : io_msb}, "force"}
                        endif
                     else
                        call extend(a:outputs, {io_name : io_msb}, "force")
                     endif
                  elseif item[3] == "I"
                     if has_key(a:inputs, io_name)
                        if str2nr(a:inputs[io_name]) < io_msb
                           call extend(a:inputs, {io_name : io_msb}, "force")
                        endif
                     else
                        call extend(a:inputs, {io_name : io_msb}, "force")
                     endif
                  endif
               endif
               let tmp = substitute(item[1], "-", ":", "")
               if item[0] == "w"
                  if width =~ "^1'"
                     call add(a:virtual_reg, item[2]." = ".reg_name."_wr & ".a:pars['wdata']."[".tmp."]")
                  else
                     call add(a:virtual_reg, item[2]." = {".substitute(width, "'.*$", "", "").
                                             \"{".reg_name."_wr}} & ".a:pars['wdata']."[".tmp."]")
                  endif
               elseif item[0] != "r" && item[2] != "reserved"
                  call add(scr_rst, repeat(" ", 2*b:vlog_ind).item[2]." <= ".a:pars['ffd']." ".width.item[0].";")
                  call add(scr_wr, repeat(" ", 2*b:vlog_ind).item[2]." <= ".a:pars['ffd']." ".a:pars['wdata']."[".tmp."];")
               endif
            endif
            if i == (len(a:lines) - 1)
               break
            else
               let i = i + 1
               let line = a:lines[i]
            endif
         endwhile
         if virtual == 0
            call add(scr_rst, repeat(" ", b:vlog_ind)."end")
            call add(scr_wr, repeat(" ", b:vlog_ind)."end")
            call extend(a:reg_scr, scr_rst)
            call extend(a:reg_scr, scr_wr)
            call add(a:reg_scr, "end")
         endif
         let scr_rst = []
         let scr_wr = []
         let rdata = substitute(rdata, ',$', "", "")
         call extend(a:reg_rdata, {reg_name : rdata}, "force")
         let rdata = ""
      else
         let i = i + 1
      endif
   endwhile
   let def_lines = []
   for reg_grp in a:reg_list
      let tmp = substitute(reg_grp, '.*', "\\U\\0", "")
      let tmp = "`define ".tmp."_ADDR ".a:reg_addr[reg_grp]
      call add(def_lines, tmp)
   endfor
   call writefile(def_lines, "reg_defines.v")
   return max_len
endfunction

function KillAutoReg()
   let line_index = 1
   let aft_kill = []
   while line_index <= line("$")
      let line = getline(line_index)
      if line =~ '^\s*/\*\s*\<autoports\>\s*\*/'
         call add(aft_kill, line)
         let line_index = line_index + 1
         let line = getline(line_index)
         while line !~ '^\s*//\s*\<End\>\s\<of\>\s\<automatic\>\s\<ports\>'
            let line_index = line_index + 1
            if line_index == line("$")
               break
            endif
            let line = getline(line_index)
         endwhile
         call add(aft_kill, line)
      elseif line =~ '^\s*/\*\s*\<autoreg\>\s*\*/'
         call add(aft_kill, line)
         let line_index = line_index + 1
         let line = getline(line_index)
         while line !~ '^\s*//\s*\<End\>\s\<of\>\s\<automatic\>\s\<registers\>'
            let line_index = line_index + 1
            if line_index == line("$")
               break
            endif
            let line = getline(line_index)
         endwhile
         call add(aft_kill, line)
      else
         call add(aft_kill, line)
      endif
      let line_index = line_index + 1
   endwhile
   call UpdateBuf(aft_kill)
endfunction

function AutoReg()
   let lines = []
   let aft_reg = []
   let reg_addr = {}
   let pars = {}
   let reg_wr = {}
   let inputs = {}
   let outputs = {}
   let reg_rdata = {}
   let reg_scr = []
   let virtual_reg = []
   let clk_gate = []
   let reg_list = []
   for line in readfile("config.txt")
      if line !~ '^\s*//'
         if line =~ '\s*//.*$'
            let line = substitute(line, '\s*//.*$', "", "")
         endif
         call add(lines, line)
      endif
   endfor
   let pars = GetCfg(lines)
   let clk_gate = ClkGateCfg(lines)
   let max_len = ReadReg(lines, pars, reg_list, reg_addr, reg_wr, inputs, outputs, reg_rdata, reg_scr, virtual_reg)
   for line in getline(1, line("$"))
      if line =~ '^\s*/\*\s*\<autoports\>\s*\*/'
         call add(aft_reg, line)
         for port in sort(keys(inputs))
            if inputs[port] == 0
               call add(aft_reg, "input".repeat(" ", 9).port.";")
            elseif inputs[port] < 10
               call add(aft_reg, "input   [".inputs[port].":0] ".port.";")
            else
               call add(aft_reg, "input  [".inputs[port].":0] ".port.";")
            endif
         endfor
         for port in sort(keys(outputs))
            if outputs[port] == 0
               call add(aft_reg, "output".repeat(" ", 8).port.";")
            elseif outputs[port] < 10
               call add(aft_reg, "output  [".outputs[port].":0] ".port.";")
            else
               call add(aft_reg, "output [".outputs[port].":0] ".port.";")
            endif
         endfor
         call add(aft_reg, "// End of automatic ports")
      elseif line =~ '^\s*/\*\s*\<autoreg\>\s*\*/'
         call add(aft_reg, line)
         let margin = CalMargin(max_len, len(reg_list[0]))
         call add(aft_reg, "parameter ".substitute(reg_list[0], '.*', "\\U\\0", "")."_ADDR".margin." = ".reg_addr[reg_list[0]].",")
         for i in range(1, len(reg_list)-2, 1)
            let margin = CalMargin(max_len, len(reg_list[i]))
            call add(aft_reg, "          ".substitute(reg_list[i], '.*', "\\U\\0", "")."_ADDR".margin." = ".reg_addr[reg_list[i]].",")
         endfor
         let margin = CalMargin(max_len, len(reg_list[len(reg_list)-1]))
         call add(aft_reg, "          ".substitute(reg_list[len(reg_list)-1], '.*', "\\U\\0", "")
                                 \."_ADDR".margin." = ".reg_addr[reg_list[len(reg_list)-1]].";")
         call add(aft_reg, "")
         call add(aft_reg, "// Control registers write signals")
         for reg_grp in reg_list
            let margin = CalMargin(max_len, len(reg_grp))
            call add(aft_reg, "assign ".reg_grp."_wr ".margin."= ".pars['reg_wr']." & (".pars['waddr']." == ".
                                    \substitute(reg_grp, '.*', "\\U\\0", "")."_ADDR);")
         endfor
         call add(aft_reg, "// Clock gating")
         for reg_grp in reg_list
            if has_key(reg_wr, reg_grp) && reg_wr[reg_grp] == "g"
               call add(aft_reg,  clk_gate[0]." u_".reg_grp."_gate (".clk_gate[1]."(".pars['clk']."),"
                                       \.clk_gate[2]."(".reg_grp."_wr),".clk_gate[3]."(".reg_grp."_clk),"
                                       \.clk_gate[4]."(".pars['testmode']."));")
            endif
         endfor
         for reg_line in reg_scr
            call add(aft_reg, reg_line)
         endfor
         for wire_line in virtual_reg
            call add(aft_reg, "assign ".wire_line.";")
         endfor
         call add(aft_reg, "// Controll registers read")
         call add(aft_reg, "always @(*) begin")
         call add(aft_reg, repeat(" ", b:vlog_ind)."case (".pars['raddr'].")")
         for reg_grp in reg_list
            let margin = CalMargin(max_len, len(reg_grp))
            call add(aft_reg, repeat(" ", b:vlog_ind).substitute(reg_grp, '.*', "\\U\\0", "")."_ADDR".margin.": "
                                    \.pars['rdata']." = {".reg_rdata[reg_grp]."};")
         endfor
         call add(aft_reg, repeat(" ", b:vlog_ind)."default: ".pars['rdata']." = ".pars['data_width']."'d0;")
         call add(aft_reg, repeat(" ", b:vlog_ind)."endcase")
         call add(aft_reg, "end")
         call add(aft_reg, "// End of automatic registers")
      else
         call add(aft_reg, line)
      endif
   endfor
   call UpdateBuf(aft_reg)
endfunction

function KillAuto()
   call KillAutoArg()
   call KillAutoInst()
   call KillAutoDef()
   call KillAutoSense()
endfunction
