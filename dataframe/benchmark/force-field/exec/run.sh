#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "Illegal number of parameters"
    exit 1
fi

let time_delta=$1
let count=$2

echo Go
../../util/xtime.rb ./force_field_go $time_delta $count
#echo Julia
#../xtime.rb julia test.jl
#echo Scala
#../xtime.rb scala Base64
cd ../java
echo Java
../../util/xtime.rb java Main $time_delta $count
cd ../exec
echo Javascript Node
../../util/xtime.rb node ../js/main.js $time_delta $count
echo Python PyPy
../../util/xtime.rb pypy ../python/main.py $time_delta $count
echo Python
../../util/xtime.rb python ../python/main.py $time_delta $count
echo Python3
../../util/xtime.rb python3 ../python/main.py $time_delta $count
echo Ruby
../../util/xtime.rb ruby ../ruby/main.rb $time_delta $count
echo Php
../../util/xtime.rb php ../php/main.php $time_delta $count
echo Rust
../../util/xtime.rb ./force_field_rs $time_delta $count
echo Not-Typed Racket
../../util/xtime.rb racket ../racket/not-typed/main.rkt $time_delta $count
echo Typed Racket
../../util/xtime.rb racket ../racket/typed/main.rkt $time_delta $count