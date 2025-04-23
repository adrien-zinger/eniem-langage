cargo build --release
echo "--- function_types ---"
./target/release/eniem tests/function_types.n
echo -e "--- printf --- \nOutput:"
./target/release/eniem tests/test_printf.n
echo -e "--- printf2 --- \nOutput:"
./target/release/eniem tests/test_printf2.n
