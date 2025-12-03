(* Lecture d'une regex lors de l'entrée ainsi que surveillance du flux éventuellement passé en paramètre *)

(* Lecture de l'entrée et exécution ligne par ligne de la fonction k : string -> unit passée en paramètre *)
let process input k =
  try
    while true do
      let line = Stdlib.input_line input in
      k line
    done
  with End_of_file -> ()

let main () =
  (* Vérification de la présence de la regex *)
  let argc = Array.length Sys.argv in
  if argc < 2 || argc > 3 then begin
    Printf.printf "usage : %s regex [file]\n%!" Sys.argv.(0);
    exit 1
  end;
  if Sys.argv.(1) = "-h" then begin
    let help = Stdlib.open_in "help.txt" in
    process help (Printf.printf "%s\n%!");
    exit 0
  end;
  (* Choix du flux d'entrée; entrée standard par défaut *)
  let input =
    if argc = 3 then begin
      Stdlib.open_in Sys.argv.(2)
    end else
      Stdlib.stdin
  in Printf.printf "Searching %s in %s\n\n%!" Sys.argv.(1) (if argc = 3 then Sys.argv.(2) else "stdin");
  (* process input Automate.search;  *)
  (* À corriger une fois le nom obtenu *)
  if argc = 3 then Stdlib.close_in input

let () = main ()