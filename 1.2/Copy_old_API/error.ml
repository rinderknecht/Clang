type position = {
  pos_fname: string;
  pos_lnum: int;
  pos_cnum: int;
  pos_bol: int
}

type message = string
type start = position
type stop = position
type seg = start * stop
type vline = int

exception Lexer of (message * seg * vline)
exception Parser of (message * seg * vline)

let print (kind: string) (msg, (start, stop), vend) =
  let delta = vend - stop.pos_lnum in
  let vstart = start.pos_lnum + delta
in assert (msg <> "");
   prerr_endline
   ("In file \"" ^ start.pos_fname ^ "\", " ^
    (if kind = "" then msg else kind) ^ " error at line "
    ^ string_of_int vstart ^ ", char " 
    ^ string_of_int (start.pos_cnum - start.pos_bol) 
    ^ (if stop.pos_lnum = start.pos_lnum
       then "--" ^ string_of_int (stop.pos_cnum - stop.pos_bol)
       else " to line " ^ string_of_int vend
            ^ ", char "
            ^ string_of_int (stop.pos_cnum - stop.pos_bol))
    ^ (if kind = "" then "." else ":\n" ^ msg))
