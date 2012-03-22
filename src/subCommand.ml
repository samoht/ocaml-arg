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

type t = { name  : string
         ; synopsis : string
         ; help  : string
         ; usage : string
         ; specs : (Arg.key * Arg.spec * Arg.doc) list
         ; anon  : string -> unit
         ; main  : unit -> unit
         }

module StringMap = Map.Make(String)

let all = ref StringMap.empty

let bad fmt =
  Printf.kprintf (fun str -> raise (Arg.Bad str)) fmt

let make ~name ~synopsis ?(help="") ?(usage="") main =
  { name; synopsis; help; usage
  ; specs = []
  ; anon = bad "%S: invalid argument"
  ; main }

and register ({ name; _ } as t) =
  all := StringMap.add name t !all

and find name =
  try
    StringMap.find name !all
  with Not_found ->
    bad "subcommand %S does not exist" name

and fold ~f ~init =
  StringMap.fold f !all init
