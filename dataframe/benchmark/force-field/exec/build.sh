#!/bin/bash

# go
go build -o force_field_go ../go/main.go
# scala
# scalac -optimize main.scala
# java
cd ../java
make
cd ../exec
# c++
cd ../c++
make
cd ../exec
#julia -e 'Pkg.add("Codecs")'
# c#
# mcs -debug- -optimize+ ../ms/main.cs
# rust
rustc -o force_field_rs ../rust/main.rs
# racket untyped
cd ../racket/not-typed
raco make main.rkt
cd ../../exec
# racket typed
cd ../racket/typed
raco make main.rkt
cd ../../exec
# swift
#swiftc -o force_field_swift ../swift/force-field/force-field/main.swift
