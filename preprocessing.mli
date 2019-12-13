open Winnowing
open Unix

(* [k_grams str n] creates a list of strings of length n, starting at each
 * character in [str] up to and including ([str] length - [n])th character.
 *
 * example:
 *  [k_grams "Hello World" 3 =
 *  ["Hel"; "ell"; "llo"; "lo "; "o W"; " Wo"; "Wor"; "orl"; "rld"]]
 *)
val k_grams : string -> int -> string list

(* [hash_file f] returns a list of hashes for n-grams of a file f with default
 * n = 35, where the file has been preprocessed by removing whitespace,
 * removing comments, replacing all variable names and strings with a generic
 * tag, while making sure that keywords and module names remain intact.
 *)
val hash_file : string -> int -> int list

(* [get_file_positions dir dir_name filename positions] rehashes file filename
 * from directory dir, preprocessing it similar to how it would be in hash_file,
 * and returns a list of parts of the files that start at the values in
 * positions once the files have been preprocessed.
 *)
val get_file_positions : string -> int list -> int -> (int * string) list

val read_file: string -> string
