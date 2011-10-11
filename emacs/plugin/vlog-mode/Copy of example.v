/* example and tutorial file */

// synopsys test line

// psl assert test line

// todo: press TAB on each line below
module example (
  in_port,
            out_port
         );

// todo: type "input 3 in_port;<RET>" and "output 5 out_put;<RET>" below:


reg       [1:0]   sig_test;                     // test signal

// todo: line up lines below, use TAB:
parameter     [2:0] para1  = value1;
parameter            [2:0]para2 = value2;
parameter[2:0]   para3   = value3;

// todo: insert an always block shown below, type with your fingers :)
//       Make sure you typed every letter, <M-s> means type Alt+s.
/*
always @(posedge clk)
if (reset_b)
begin
sig ,, 1b0;
end
else
<M-s> sig_d;
*/

// todo: OK, make another always block, but this time you just need to type
//       <C-c C-c C-a>, type it below:


// todo: Move cursor on `begin', and press <C-c TAB>.  You can see that the
//       whole block is indented:
always @(posedge clk)
     begin
  if (work)
    if (~x && y)
    uv = step;
        else
      uv = step>>1;
        else
          case (rog)
              2'b00   : vsstep_uv = 11'd128;
  2'b11   : vsstep_uv = 11'd128;
      2'b01   : vsstep_uv = 11'd256;
           2'b10   : vsstep_uv = 11'd64;
  default : vsstep_uv = 11'd128;
    endcase
  end
// todo: Try <C-c %> when the cursor is on `begin', `end' (any block beginning
//       or block end) or `else'.

// todo: Indent & align lines below, use TAB:
m_ff_ce #(1,1'b0) u_ff_1 (
 .clk            (clk),  // clock
  .rst_b           (rst_b),  // reset
 .i_ce      (1'b1),// enable
  .i_in1        (in_port), // input
 .o_out1   (out_port));// output


// todo: Put cursor on sig_test, then press F1, its width will be displayed in echo area.
//       Then move cursor on sig, then type <C-c C-d>, emacs will trace its driver, then
//       type <C-c C-n>, emacs will trace the next drive place.
always @(sig)
  if (sig)
     sig_test = sig;
  else
     sig_test = 2'b0;

// todo: Insert a combinative always block, use <C-c C-c a>.
//       Than type something, and then use <C-c C-a> to complete its sensitive list.
//       If you're not satisfied, try to set vlog-auto-sense-abandon-old-list and
//       vlog-auto-sense-refill-old-list.


// todo: Type <C-c C-h> to insert a file header, set vlog-skel-user-name as your name and
//       vlog-skel-company-name as your company name. set vlog-skel-header-string to make
//       your own header string. C-h v vlog-skel-header-string <RET>, or check the source
//       in vlog-skel.el.


// DOC: example.v ends here
