program euler010
  implicit none

  real :: start, finish
  integer :: i
  integer(kind=8) :: n

  call cpu_time(start)
  call solution()
  call cpu_time(finish)

  print '("Elapsed time: ", F15.11, " seconds")', finish - start
contains
  subroutine solution()
    use euler_mod, only: primes_below

    implicit none

    print *, sum(primes_below(2000000_8))
  end subroutine
end program
