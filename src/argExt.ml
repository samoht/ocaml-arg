(******************************************************************************)
(*                                                                            *)
(*  Copyright (C) 2008-2010, OCamlCore SARL                                   *)
(*  Copyright (C)      2011, camlunity                                        *)
(*  Copyright (C)      2012, OCamlPro                                         *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(*  TypeRex is distributed in the hope that it will be useful,                *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             *)
(*  GNU General Public License for more details.                              *)
(*                                                                            *)
(******************************************************************************)

(** Parsing of command line arguments  *)

open Format
open FormatExt
open SubCommand

module List = ListLabels

type help_extent =
  | NoSubCommand
  | SubCommand of string
  | AllSubCommands

let progname = Filename.basename Sys.argv.(0)

let usage_msg =
  Printf.sprintf
    "%s [global-options*] subcommand [subcommand-options*]"
    progname

(* Pretty printers. *)

let pp_print_specs fmt specs =
  let help_specs = List.rev_append
    (List.rev_map ~f:(fun (name, _, help) -> (name, help)) specs)
    ["--help", " Display this list of options"]
  in

  let size = List.fold_left
    ~init:0
    ~f:(fun acc (term, _) -> max acc (String.length term))
    help_specs
  in

  pp_print_list (pp_print_output_def size) "" fmt help_specs;
  pp_print_newline fmt ()

let pp_print_cmd fmt { name; help; usage; specs; _ } =
  pp_print_underlined '-' fmt (sprintf "Subcommand %s" name);
  pp_print_endblock fmt ();

  pp_print_string fmt help;
  pp_print_endblock fmt ();

  fprintf fmt ("Usage: %s [global-options*] %s %s") progname name usage;
  pp_print_endblock fmt ();

  match specs with
    | [] -> ()
    | _  ->
      pp_print_para fmt "Options: ";
      pp_print_specs fmt specs

let pp_print_cmds fmt () =
  let size = SubCommand.fold
    ~init:0
    ~f:(fun name _ acc -> max acc (String.length name))
  in

  pp_print_para fmt "Available subcommands:";
  SubCommand.fold ~init:() ~f:(fun name { synopsis; _ } () ->
    ignore (pp_print_output_def size fmt (name, synopsis))
  );
  pp_print_newline fmt ()

let pp_print_help ?man_fun hext fmt specs () =
  match hext with
    | NoSubCommand ->
      (pp_print_string fmt usage_msg;
       pp_print_endblock fmt ();
       pp_print_specs fmt specs;
       pp_print_cmds fmt ())
    | SubCommand name -> (match man_fun with
        | None     -> pp_print_cmd fmt (SubCommand.find name)
        | Some cmd -> cmd name)
    | AllSubCommands ->
      SubCommand.fold
        ~init:()
        ~f:(fun _ cmd () -> pp_print_cmd fmt cmd)


(* Hardcore subcommand parsing; solely taken from OASIS. *)

(* taken from extlib *)
module String = struct
  include String

  exception Invalid_string

  let find str sub =
    let sublen = length sub in
    if sublen = 0 then
      0
    else
      let found = ref 0 in
      let len = length str in
      try
	for i = 0 to len - sublen do
	  let j = ref 0 in
	  while unsafe_get str (i + !j) = unsafe_get sub !j do
	    incr j;
	    if !j = sublen then begin found := i; raise Exit; end;
	  done;
	done;
	raise Invalid_string
      with
	Exit -> !found

  let split str sep =
    let p = find str sep in
    let len = length sep in
    let slen = length str in
    sub str 0 p, sub str (p + len) (slen - p - len)

  let nsplit str sep =
    if str = "" then []
    else if sep = "" then raise Invalid_string
    else (
      let rec nsplit str sep =
        try
	  let s1 , s2 = split str sep in
	  s1 :: nsplit s2 sep
        with
	  Invalid_string -> [str]
      in
      nsplit str sep
    )
end

let parse ?man_fun specs =
  let pos = ref 0 in
  let cmd = ref (SubCommand.make
    ~name:"none"
    ~synopsis:""
    (fun () -> failwith "no subcommand defined"))
  and cmd_args = ref [||] in

  let set_cmd s =
    cmd := SubCommand.find s;
    cmd_args := Array.sub Sys.argv !pos ((Array.length Sys.argv) - !pos);
    pos := !pos + Array.length !cmd_args
  in

  let handle_error exc hext =
    let get_bad txt =
      match String.nsplit txt "\n" with
        | [] -> "Unknown error on the command line"
        | fst :: _ -> fst
    in match exc with
      | Arg.Bad txt ->
        pp_print_help hext err_formatter specs ();
        prerr_endline (get_bad txt);
        exit 2
      | Arg.Help _txt ->
        pp_print_help ?man_fun hext std_formatter specs ();
        exit 0
      | _ ->
        raise exc
  in

  (* Parse global options and set current subcommand. *)
  begin
    try
      Arg.parse_argv
        ~current:pos
        Sys.argv
        (Arg.align specs)
        set_cmd
        usage_msg
    with exc ->
      handle_error exc NoSubCommand
  end;

  (* Parse subcommand's options and execute it. *)
  begin
    try
      Arg.parse_argv
        ~current:(ref 0)
        !cmd_args
        (Arg.align !cmd.specs)
        !cmd.anon
        (sprintf "Subcommand %S options:\n" !cmd.name)
    with exc ->
      handle_error exc (SubCommand !cmd.name)
  end;

  !cmd.main ()
