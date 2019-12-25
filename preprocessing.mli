open Core

val generate_n_grams : int -> 'a list -> 'a list list
type kgram = {
  length : int;
  occupying_lines : Core.Int.Set.t;
  starting_line : int;
  starting_index_in_line : int;
  hash : int;
} [@@deriving sexp]

val list_zip : 'a list -> 'b list -> ('a * 'b) list
val k_grams_with_line_number : string list -> int -> kgram list
val read_file : string -> string list
