# euler

[Project Euler](http://projecteuler.net) solutions in Fortran

`Makefile` defines the following targets:

## new

```sh
$ make new [n=<N>]
```

Creates new solution for problem N from `template.f08`. If variable `n` is
omitted, creates solution for the next unsolved problem.

## run

```sh
$ make run [n=<N>]
```

Executes solution for the problem N. If variable `n` is omitted, executes
solution for the last solved problem.

## test

```sh
$ make test [n=<N>]
```

Currently doest nothing. Intention was to run tests when they are available.
Maybe in future will be merged into `run` target.
