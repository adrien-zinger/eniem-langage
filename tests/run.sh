echo "--- function_types ---"
./target/release/eniem tests/function_types.n
echo -e "--- printf --- \nOutput:"
./target/release/eniem tests/"printf.n"
echo -e "--- printf2 --- \nOutput:"
./target/release/eniem tests/"printf2.n"
echo -e "--- printf3 --- \nOutput:"
./target/release/eniem tests/"printf3.n"
echo -e "--- references --- \nOutput:"
./target/release/eniem tests/references.n
echo -e "--- add --- \nOutput:"
./target/release/eniem tests/add.n
echo -e "--- add mult --- \nOutput:"
./target/release/eniem tests/add_mult.n
echo -e "--- identity --- \nOutput:"
./target/release/eniem tests/identity.n
echo -e "--- assignation 1 (world expected) --- \nOutput:"
./target/release/eniem tests/var_assignation.n
echo -e "--- assignation 2 (hi hi hi) --- \nOutput:"
./target/release/eniem tests/ref_assign.n
