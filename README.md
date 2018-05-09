# RacketFrames

This is RacketFrames. DataFrames from the popular Pandas library implemented in Racket.

## Getting Started

In this day and age procedural and functional programmers alike are working with large amounts of data. Many analytical libraries have been developed for many domains to work with such data. Python/Pandas, R or even the command prompt can be used to accomplish theses taks, but this option is especially good if you are 1) using Racket already 2) may need to integrate your solution with other Racket applications in the future or 3) just plain love functional programming.

You can answer this question and many more with RacketFrames.

### Prerequisites

[Racket Version 6](http://racket-lang.org/download/)

[Pipe](https://github.com/RayRacine/pipe.git) Also available from Racket catalog.

[Grip](https://github.com/RayRacine/grip.git) Also available from Racket catalog.

[RacketFrames](https://github.com/bommysk/RacketFrames.git)

### Installing

Step by step instructions on getting up and running.

```
Download Racket Version 6.8+ for your OS from the provided prerequisite link.
```

Add the pipe, grip and RacketFrame packages using [DrRacket](http://docs.racket-lang.org/quick/index.html) which is installed when you download Racket. The pipe and grip packages are available from the catalog, and can be installed that way as well.

Example installation using git url in package manager.

![Alt text](images/pipe.png?raw=true "Title")

![Alt text](images/grip.png?raw=true "Title")

![Alt text](images/racket_frames.png?raw=true "Title")

## Running the tests

Currently the tests are integrated into the files themselves. A full test suite is in the works.

### Benchmarks

One of the purposes of this work is to compare performance with other DataFrame implementations. Currently benchmarks are being implemented for comparison against Pandas.

Benchmarks are located in the benchmark directory and separated into different directories by the type of benchmark. They are shells scripts that print the performance of the benchmark against that of Pandas.

Example:
```
sh benchmark/join_merge/join_merge.sh
```

## Authors

* **Shubham Kahal**

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

A huge thank you to [Ray Racine](https://github.com/RayRacine) who's repo [Munger](https://github.com/RayRacine/munger) was used as the starting point of this project. This project aims to add further functionality, test cases and performance benchmarks to this existing library.

