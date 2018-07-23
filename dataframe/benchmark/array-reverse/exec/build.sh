# go
go build -o array_reverse_go ../go/main.go
# c
gcc -O3 -std=c99 -o array_reverse_c ../c/main.c
# scala
# scalac -optimize main.scala
# java
javac ../java/Main.java
#julia -e 'Pkg.add("Codecs")'
# c#
# mcs -debug- -optimize+ ../ms/main.cs
# rust
rustc -o array_reverse_rs ../rust/main.rs
# rust
rustc -o array_reverse_builtin_rs ../rust/main_builtin.rs
# racket untyped
raco make ../racket/not-typed/main.rkt
# racket typed
raco make ../racket/typed/main.rkt
# swift
#swiftc -o array_reverse_swift ../swift/main.swift
