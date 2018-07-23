#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "Illegal number of parameters"
    exit 1
fi

let start=$1
let end=$2

echo Go
../../util/xtime.rb ./roll_avg_go $start $end
echo C
../../util/xtime.rb ./roll_avg_c $start $end
#echo Julia
#../xtime.rb julia test.jl
#echo Scala
#../xtime.rb scala Base64
cd ../java
echo Java
../../util/xtime.rb java Main $start $end
cd ../exec
echo Javascript Node
../../util/xtime.rb node ../js/main.js $start $end
echo Python PyPy
../../util/xtime.rb pypy ../python/main.py $start $end
echo Python
../../util/xtime.rb python ../python/main.py $start $end
echo Python3
../../util/xtime.rb python3 ../python/main.py $start $end
echo Ruby
../../util/xtime.rb ruby ../ruby/main.rb $start $end
echo Php
../../util/xtime.rb php ../php/main.php $start $end
echo Rust
../../util/xtime.rb ./roll_avg_rs $start $end
echo Not-Typed Racket
../../util/xtime.rb racket ../racket/not-typed/main.rkt $start $end
echo Typed Racket
../../util/xtime.rb racket ../racket/typed/main.rkt $start $end