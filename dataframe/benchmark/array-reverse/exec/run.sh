#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "Illegal number of parameters"
    exit 1
fi

let size=$1
let iterations=$2

echo Go
../../util/xtime.rb ./array_reverse_go $size $iterations
echo C
../../util/xtime.rb ./array_reverse_c $size $iterations
#echo Julia
#../xtime.rb julia test.jl
#echo Scala
#../xtime.rb scala Base64
cd ../java
echo Java
../util/xtime.rb java Main $size $iterations
echo ../exec
echo Javascript Node
../../util/xtime.rb node ../js/main.js $size $iterations
echo Javascript Builtin Node
../../util/xtime.rb node ../js/main_builtin.js $size $iterations
echo Python PyPy
../../util/xtime.rb pypy ../python/main2.py $size $iterations
echo Python PyPy
../../util/xtime.rb pypy ../python/main2_builtin.py $size $iterations
echo Python PyPy
../../util/xtime.rb pypy ../python/main3.py $size $iterations
echo Python PyPy
../../util/xtime.rb pypy ../python/main3_builtin.py $size $iterations
echo Python
../../util/xtime.rb python ../python/main2.py $size $iterations
echo Python
../../util/xtime.rb python ../python/main2_builtin.py $size $iterations
echo Python3
../../util/xtime.rb python3 ../python/main3.py $size $iterations
echo Python3
../../util/xtime.rb python3 ../python/main3_builtin.py $size $iterations
echo Ruby
../../util/xtime.rb ruby ../ruby/main.rb $size $iterations
echo Php
../../util/xtime.rb php ../php/main-user.php $size $iterations
echo Php Builtin
../../util/xtime.rb php ../php/main-builtin.php $size $iterations
echo Rust
../../util/xtime.rb ./array_reverse_rs $size $iterations
echo Rust Builtin
../../util/xtime.rb ./array_reverse_builtin_rs $size $iterations
echo Not-Typed Racket
../../util/xtime.rb racket ../racket/not-typed/main.rkt $size $iterations
echo Typed Racket
../../util/xtime.rb racket ../racket/typed/main.rkt $size $iterations
