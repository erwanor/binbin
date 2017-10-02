type t = Binstr of string;;

let empty = Binstr "";;

let unsafe_s binstr =
    match binstr with
    | (Binstr str) -> str
;;

let unsafe_b str = (Binstr str);;

let size binstr = String.length (unsafe_s binstr);;

(** UTILITIES AND HELPERS **) 

let is_bit_on bit = bit = (Binstr "1");;

let is_bit_off bit = bit = (Binstr "0");;

let distance_to_byte b =
    let len = size b in
    if len < 8 then (8 - len)
    else (len mod 8)
;;

let ipow n x = (float_of_int n) ** (float_of_int x) |> int_of_float;;

let char_to_int c = if c = '0' then 0 else 1;;

let char_to_string c = (String.make 1 c);;

let remove_str k s =
    let len = String.length s in
    if len > k then (String.sub s k (len-k))
    else ""
;;
 
let remove_bits i b =
    let len = (size b) in
    if len < i then
        raise (invalid_arg "Illegal operation: cannot be asked to remove more bits than binstring size!\n")
    else if i <= 0 then b
    else (unsafe_b (String.sub (unsafe_s b) i (len-i)))
;;

let char_bit_to_binstr c =
    if c <> '0' && c <> '1' then
        raise (invalid_arg "Invalid character bit!\n")
    else unsafe_b (char_to_string c)
;;

(** IMPLEMENTATION **)
let concat b1 b2 = (unsafe_b ((unsafe_s b1) ^ (unsafe_s b2)));;

let pad_left k b =
    let bs = (unsafe_s b) in
    let rec pad k bs =
        if k <= 0 then bs
        else (pad (k-1) ("0" ^ bs))
    in (unsafe_b (pad k bs))
;;

let byte_pad_left b =
    let padding = distance_to_byte b in
    pad_left padding b
;;

let of_int n =
    let reduce_int i = if i = 0 then 0 else 1 in
    let rec convert pos binstr remainder =
        if remainder = 0 then binstr
        else begin
            let md = remainder mod (ipow 2 (pos+1)) in
            let current = (unsafe_b (string_of_int (reduce_int md))) in
            let updated = (concat current binstr) in
            convert (pos+1) updated (remainder-md)
        end
    in
    if n = 0 then (unsafe_b "0")
    else (convert 0 empty n)
;;

let of_char c =
    let code = Char.code c in
    of_int code
;;

let of_string s =
    let rec convert pos binstr remainder =
        if remainder = "" then binstr
        else begin
            let c = remainder.[0] in
            let c_binstr = byte_pad_left (of_char c) in
            let updated = (concat c_binstr binstr) in
            let r = remove_str 1 remainder in
            convert (pos+1) updated r
        end
    in (convert 0 empty s)
;;

let to_int b =
    let str = (unsafe_s b) in
    let rec convert total remainder =
        let len = (String.length remainder) in
        if len = 0 then total
        else begin
            let coeff = (char_to_int remainder.[0]) in
            let n = (ipow 2 (len-1)) * coeff in
            let r = remove_str 1 remainder in
            convert (n+total) r
        end
    in convert 0 str
;;
