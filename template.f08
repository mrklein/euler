program euler%N%
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
    
    ! TODO
  end subroutine
end program
