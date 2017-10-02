type t

val size : t -> int

val of_int : int -> t

val of_char : char -> t

val of_string : string -> t

val to_int : t -> int

val to_char : t -> char

val to_ascii : t -> string

val unsafe_s : t -> string

val unsafe_b : string -> t

