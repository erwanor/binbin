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

let pad_right k b =
    let bs = (unsafe_s b) in
    let rec pad k bs =
        if k <= 0 then bs
        else (pad (k-1) (bs ^ "0"))
    in (unsafe_b (pad k bs))
;;

let byte_pad_right b =
    let padding = distance_to_byte b in
    pad_right padding b
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

let to_char b =
    let code = to_int b in
    (Char.chr code)
;;

let to_ascii b =
    let s = (unsafe_s (byte_pad_left b)) in
    let rec convert str binstr =
        let len_str = (String.length binstr) in
        if len_str = 0 then str
        else begin
            let byte = (String.sub binstr 0 8) in
            let remainder = remove_str 8 binstr in
            let binstr_byte = (to_int (unsafe_b byte)) in
            let c = (Char.chr binstr_byte) in
            let cs = (char_to_string c) in
            convert (cs ^ str) remainder
        end
    in convert "" s
;;


let normalize b1 b2 =
    let delta = (size b1) - (size b2) in
    if delta = 0 then (b1, b2)
    else if delta > 0 then (b1, (pad_left delta b2))
    else ((pad_left (abs delta) b1), b2)
;;

let take i b =
    if (size b) < i then
        raise (invalid_arg "Out-of-bound: cannot take bit at provided index!\n")
    else
        let s = unsafe_s b in
        (char_bit_to_binstr s.[i-1])
;;

let foldr f b acc =
    let rec traverse pos f b acc =
        if pos = 0 then acc
        else (traverse (pos-1) f b (f (take pos b) acc))
    in traverse (size b) f b acc
;;

let foldl f acc b =
    let rec traverse pos f acc b =
        if pos = (size b) then acc
        else (traverse (pos+1) f (f acc (take pos b)) b)
    in traverse 0 f acc b
;;

let make i b =
    let rec build pos i result model =
        if pos >= i then result
        else begin
            let model_pos = (pos mod (size model))+1 in
            let current = take model_pos model in
            build (pos+1) i (concat result current) model
        end
    in build 0 i empty b
;;

let msbit b =
    if (size b) = 0 then
        raise (invalid_arg "Empty binary string!\n")
    else (take 1 b)
;;

let lsbit b =
    if (size b) = 0 then
        raise (invalid_arg "Empty binary string!\n")
    else (take (size b) b)
;;
   
let mapi f b =
    let rec traverse f pos acc b =
        if (size b) = 0 then acc
        else begin
            let bit = msbit b in
            let result = f pos bit in
            let remainder = remove_bits 1 b in
            traverse f (pos+1) (concat acc result) remainder
        end
    in traverse f 0 empty b
;;

let map f b =
    let rec traverse f acc b =
        if (size b) = 0 then acc
        else begin
            let bit = msbit b in
            let result = f bit in
            let remainder = remove_bits 1 b in
            traverse f (concat acc result) remainder
        end
    in traverse f empty b
;;

let dmap f b1 b2 =
    if (size b1) <> (size b2) then
        raise (invalid_arg "Cannot map over two Binstrings of unequal lengths!\n");
    let rec traverse f acc b1 b2 =
        if (size b1) = 0 then acc
        else begin
            let msb1 = msbit b1 in
            let msb2 = msbit b2 in
            let result = f msb1 msb2 in
            let r1 = remove_bits 1 b1 in
            let r2 = remove_bits 1 b2 in
            traverse f (concat acc result) r1 r2
        end
    in traverse f empty b1 b2
;;

let logical_operation op =
    fun bit1 bit2 ->
        let s1 = (unsafe_s bit1) in
        let s2 = (unsafe_s bit2) in
        let exec = op (char_to_int s1.[0]) (char_to_int s2.[0]) in
        (unsafe_b (string_of_int exec))
;;

let b_xor b1 b2 =
    let padded_b1, padded_b2 = normalize b1 b2 in
    let xoring = logical_operation (lxor) in
    dmap xoring padded_b1 padded_b2 
;;

let b_or b1 b2 =
    let padded_b1, padded_b2 = normalize b1 b2 in
    let oring = logical_operation (lor) in
    dmap oring padded_b1 padded_b2
;;

let b_and b1 b2 =
    let padded_b1, padded_b2 = normalize b1 b2 in
    let anding = logical_operation (land) in
    dmap anding padded_b1 padded_b2
;;

let flip b =
    let upper_bound = (make (size b) (unsafe_b "1")) in
    b_xor b upper_bound
;;

let flip_bit_at target b =
    if target > (size b) then
        raise (invalid_arg "Out-of-bound: bit at position cannot be flipped!\n");
    let flipper = fun index bit ->
        if target = (index+1) then (flip bit)
        else bit
    in mapi flipper b
;;

let reverse b =
    let rec rev result pos b =
        if pos = 0 then result
        else rev (concat result (take pos b)) (pos - 1) b
    in rev empty (size b) b
;;
