program euler009
  implicit none

  real :: start, finish
  integer(kind=8), parameter :: N = 1000
  integer(kind=8) :: i, r

  call cpu_time(start)
  do i = 1, N
    call solution(r)
  end do
  call cpu_time(finish)

  print *, r
  print '("Elapsed time: ", F15.11, " usec")', 1e6*(finish - start)/N
contains
  subroutine solution(r)
    use euler_mod

    implicit none

    integer(kind=8), intent(out) :: r
    integer :: a, b, c

    outer: do a = 1, 998
      do b = a + 1, 999
        c = 1000 - a - b
        if (c < b) cycle
        if (a*a + b*b == c*c) then
          r = a*b*c
          exit outer
        end if
      end do
    end do outer
    
  end subroutine
end program
