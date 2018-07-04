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

open Format

module List = ListLabels

let pp_print_string_spaced fmt =
  String.iter (function
    | ' ' | '\n' -> Format.pp_print_space fmt ()
    | c -> Format.pp_print_char fmt c)

let pp_print_list pp_elem sep fmt = function
  | [] -> ()
  | hd :: tl ->
    pp_elem fmt hd;
    List.iter
      ~f:(fun e -> fprintf fmt sep; pp_elem fmt e)
      tl

let pp_print_justified size fmt s =
  let tmp = Bytes.make size ' ' in
  String.blit s 0 tmp 0 (String.length s);
  pp_print_string fmt (Bytes.unsafe_to_string tmp)

let pp_print_underlined c fmt s =
  pp_print_string fmt s;
  pp_print_newline fmt ();
  pp_print_string fmt (String.make (String.length s) c)

let pp_print_endblock fmt () =
  pp_print_newline fmt ();
  pp_print_newline fmt ()

let pp_print_para fmt s =
  pp_open_box fmt 0;
  pp_print_string_spaced fmt s;
  pp_close_box fmt ();
  pp_print_endblock fmt ()

let pp_print_output_def size fmt (term, def) =
  pp_print_string fmt "  ";
  pp_print_justified size fmt term;
  pp_print_string fmt "  ";
  pp_open_box fmt 0;
  pp_print_string_spaced fmt def;
  pp_close_box fmt ();
  pp_print_newline fmt ()
