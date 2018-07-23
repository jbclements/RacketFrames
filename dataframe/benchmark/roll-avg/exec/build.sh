#!/bin/bash

# go
go build -o roll_avg_go ../go/main.go
# c
gcc -O3 -std=c99 -o roll_avg_c ../c/main.c
# scala
# scalac -optimize main.scala
# java
cd ../java
javac Main.java
cd ../exec
#julia -e 'Pkg.add("Codecs")'
# c#
# mcs -debug- -optimize+ ../ms/main.cs
# rust
rustc -o roll_avg_rs ../rust/main.rs
# racket untyped
raco make ../racket/not-typed/main.rkt
# racket typed
raco make ../racket/typed/main.rkt
# swift
#swiftc -o array_reverse_swift ../swift/main.swift
