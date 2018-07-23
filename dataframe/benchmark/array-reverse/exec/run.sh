echo Go
../../util/xtime.rb ./array_reverse_go 10 5
echo C
../../util/xtime.rb ./array_reverse_c 10 5
#echo Julia
#../xtime.rb julia test.jl
#echo Scala
#../xtime.rb scala Base64
# echo Java
# ../util/xtime.rb java Main
echo Javascript Node
../../util/xtime.rb node ../js/main.js 10 5
echo Javascript Builtin Node
../../util/xtime.rb node ../js/main_builtin.js 10 5
echo Python PyPy
../../util/xtime.rb pypy ../python/main2.py 10 5
echo Python PyPy
../../util/xtime.rb pypy ../python/main2_builtin.py 10 5
echo Python PyPy
../../util/xtime.rb pypy ../python/main3.py 10 5
echo Python PyPy
../../util/xtime.rb pypy ../python/main3_builtin.py 10 5
echo Python
../../util/xtime.rb python ../python/main2.py 10 5
echo Python
../../util/xtime.rb python ../python/main2_builtin.py 10 5
echo Python3
../../util/xtime.rb python3 ../python/main3.py 10 5
echo Python3
../../util/xtime.rb python3 ../python/main3_builtin.py 10 5
echo Ruby
../../util/xtime.rb ruby ../ruby/main.rb 10 5
echo Php
../../util/xtime.rb php ../php/main-user.php 10 5
echo Php Builtin
../../util/xtime.rb php ../php/main-builtin.php 10 5
echo Rust
../../util/xtime.rb ./array_reverse_rs 10 5
echo Rust Builtin
../../util/xtime.rb ./array_reverse_builtin_rs 10 5
