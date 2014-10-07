program euler010
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
    
    integer(kind=4) :: i
    integer(kind=8) :: r

    r = 0_8

    do i = 2, 1999999
      if (is_prime(i)) r = r + i
    end do

    print *, r
  end subroutine
end program
