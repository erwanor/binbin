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
            assert_equal (of_int 0) (unsafe_b "0");
            assert_equal (of_int 1) (unsafe_b "1");
            assert_equal (of_int 2) (unsafe_b "10");
            assert_equal (of_int 256) two_fifty_six_b; 
            assert_equal (of_int 1000) a_thousand_b; 
        );
	]

let _ = run_test_tt_main test_fixture
