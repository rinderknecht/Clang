if Array.length Sys.argv = 2
then
  match open_in Sys.argv.(1) with
    cin ->
      let buffer = Lexing.from_channel cin in
        let open Error
      in (try Parser.main Lexer.get_token buffer with
            Lexer diag   -> print "Lexical" diag
          | Parser diag  -> print "Syntactical" diag
          | Parser.Error -> print "" 
                                  ("Parse", mk_seg buffer, !Lexer.virt_lnum));
         close_in cin
  | exception Sys_error msg -> prerr_endline msg
else prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " [file]")
