type t

val size : t -> int

val of_int : int -> t

val of_char : char -> t

val of_string : string -> t

val to_int : t -> int

val to_char : t -> char

val to_ascii : t -> string

val take : int -> t -> t

val make : int -> t -> t

val flip : t -> t

val flip_bit_at : int -> t -> t

val msbit : t -> t

val lsbit : t -> t

val pad_left : int -> t -> t

val pad_right : int -> t -> t

val concat : t -> t -> t


val normalize : t -> t -> t * t
val unsafe_s : t -> string

val unsafe_b : string -> t

