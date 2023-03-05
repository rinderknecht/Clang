match Array.length Sys.argv with
  3 -> Copy.trace Sys.argv.(1) Sys.argv.(2)
| _ -> prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " [input] [output]")
