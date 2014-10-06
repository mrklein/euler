program euler007
  implicit none

  real :: start, finish

  call cpu_time(start)
  call solution()
  call cpu_time(finish)

  print '("Elapsed time: ", F9.6, " seconds")', finish - start
contains
  subroutine solution()
    use euler_mod

    implicit none

    integer :: i, c

    i = 2
    c = 0

    do while (c < 10001)
      if (is_prime(i)) c = c + 1
      i = i + 1
    end do

    print *, i - 1
  end subroutine
end program
