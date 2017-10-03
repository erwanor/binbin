open OUnit2;;
open Binbin;;

let empty = (unsafe_b "");;
let zero_bit = (unsafe_b "0");;
let zero_byte = (unsafe_b "00000000");;
let one_bit = (unsafe_b "1");;
let one_byte = (unsafe_b "11111111");;
let two_fifty_six_b = (unsafe_b "100000000");;
let a_thousand_b = (unsafe_b "1111101000");;
let freedom = "freedom";;
let freedom_b = (unsafe_b "01100110011100100110010101100101011001000110111101101101");;
let quick_brown_fox = "the quick brown fox!";;
let quick_brown_fox_b = (unsafe_b "0111010001101000011001010010000001110001011101010110100101100011011010110010000001100010011100100110111101110111011011100010000001100110011011110111100000100001");;
let f_pattern_b = (unsafe_b "110");;
let s_pattern_b = (unsafe_b "1001");;

let test_fixture = "Binbin" >:::
    [
        "size" >:: ( fun test_ctx ->
            assert_equal (size empty) 0;
            assert_equal (size zero_bit) 1;
            assert_equal (size one_bit) 1;
            assert_equal (size zero_byte) 8;
        );

        "concat" >:: ( fun test_ctx ->
            assert_equal (concat empty empty) empty;
            assert_equal (concat zero_bit one_bit) (unsafe_b "01");
            assert_equal (concat one_bit zero_bit) (unsafe_b "10");
            assert_equal (concat zero_byte one_bit) (unsafe_b "000000001");
            assert_equal (concat one_bit (concat zero_bit (concat zero_bit (concat zero_bit (concat zero_bit (concat zero_bit (concat zero_bit (concat zero_bit zero_bit)))))))) two_fifty_six_b
        );

        "of_int" >:: ( fun test_ctx ->
            (** Should add support for unsigned ints *)
            assert_equal (of_int 0) (unsafe_b "0");
            assert_equal (of_int 1) (unsafe_b "1");
            assert_equal (of_int 2) (unsafe_b "10");
            assert_equal (of_int 256) two_fifty_six_b; 
            assert_equal (of_int 1000) a_thousand_b; 
        );

        "of_char" >:: ( fun test_ctx ->
            let rec test_all_chars i =
                if i = 256 then ()
                else
                    let c = Char.chr i in
                    assert_equal (of_char c) (of_int i);
                    test_all_chars (i+1)
            in test_all_chars 0;
        );

        "of_string" >:: ( fun test_ctx ->
            assert_equal (of_string "") empty;
            assert_equal (of_string "a") (concat zero_bit (of_char 'a'));
            assert_equal (of_string "1") (concat (unsafe_b "00") (of_char '1'));
            assert_equal (of_string "1111") (unsafe_b "00110001001100010011000100110001");
            assert_equal (of_string freedom) freedom_b;
            assert_equal (of_string quick_brown_fox) quick_brown_fox_b;
        );

        "to_int" >:: ( fun test_ctx ->
            assert_equal (to_int zero_bit) 0;
            assert_equal (to_int zero_byte) 0;
            assert_equal (to_int one_bit) 1;
            assert_equal (to_int one_byte) 255;
            assert_equal (to_int two_fifty_six_b) 256;
			assert_equal (to_int a_thousand_b) 1000;
		);
        
        "to_char" >:: ( fun test_ctx ->
            assert_equal (to_char zero_bit) (Char.chr 0);
            assert_equal (to_char zero_byte) (Char.chr 0);
            assert_equal (to_char one_bit) (Char.chr 1);
            assert_equal (to_char one_byte) (Char.chr 255);
            (** Should raise our own exception *)
            let invalid_cast = fun () -> (to_char two_fifty_six_b) in
            assert_raises (Invalid_argument "Char.chr") invalid_cast;
        );

        "to_ascii" >:: ( fun test_ctx ->
            assert_equal (to_ascii empty) "";
            assert_equal (to_ascii quick_brown_fox_b) quick_brown_fox;
            assert_equal (to_ascii freedom_b) freedom;
        );

        "take" >:: ( fun test_ctx ->
            assert_equal (take 0 empty) empty;
            assert_equal (take 0 two_fifty_six_b) empty;
            assert_equal (take 1 zero_bit) zero_bit;
            assert_equal (take 1 one_bit) one_bit;
            assert_equal (take 8 zero_byte) zero_bit;
            assert_equal (take 8 one_byte) one_bit;
            assert_equal (take 6 a_thousand_b) zero_bit;
            assert_equal (take 7 a_thousand_b) one_bit;
        );

        "make" >:: (fun test_ctx ->
            assert_equal (make 0 empty) empty;
            assert_equal (make 0 zero_bit) empty;
            assert_equal (make 0 zero_byte) empty;
            assert_equal (make 8 empty) empty;
            assert_equal (make 8 zero_bit) zero_byte;
            assert_equal (make 8 one_bit) one_byte;
            assert_equal (make 8 zero_byte) zero_byte;
            assert_equal (make 8 one_byte) one_byte;
            assert_equal (make 2 f_pattern_b) (unsafe_b "11");
            assert_equal (make 3 f_pattern_b) f_pattern_b; 
            assert_equal (make 9 f_pattern_b) (unsafe_b "110110110");
            assert_equal (make 11 f_pattern_b) (unsafe_b "11011011011");
            assert_equal (make 3 s_pattern_b) (unsafe_b "100");
            assert_equal (make 4 s_pattern_b) s_pattern_b;
            assert_equal (make 12 s_pattern_b) (unsafe_b "100110011001");
            assert_equal (make 15 s_pattern_b) (unsafe_b "100110011001100");
        );

        "flip" >:: (fun test_ctx ->
            assert_equal (flip empty) empty;
            assert_equal (flip zero_bit) one_bit;
            assert_equal (flip one_bit) zero_bit;
            assert_equal (flip zero_byte) one_byte;
            assert_equal (flip one_byte) zero_byte;
            (** we need a deep equal *)
            assert_equal (flip two_fifty_six_b) (unsafe_b "011111111");
        );
	]

let _ = run_test_tt_main test_fixture
