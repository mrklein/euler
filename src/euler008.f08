program euler008
  implicit none

  real :: start, finish

  call cpu_time(start)
  call solution()
  call cpu_time(finish)

  print '("Elapsed time: ", F9.6, " seconds")', finish - start
contains
  subroutine read_number(arr)
    implicit none

    integer(kind=8), dimension(1000), intent(out) :: arr

    character(len=50) :: buf
    integer :: l, i, idx

    open(10, file='data/euler008.txt')
    idx = 1
    do l = 1,20
      read(10, fmt='(A50)') buf
      do i = 1, 50
        arr(idx) = iachar(buf(i:i)) - iachar('0')
        idx = idx + 1
      end do
    end do
    close(10)
  end subroutine

  subroutine solution()
    use euler_mod

    implicit none

    integer(kind=8), dimension(1000) :: n
    integer :: i
    integer(kind=8) :: r = 0_8

    call read_number(n)

    do i = 1, 1000 - 12
      r = max(r, product(n(i:i + 12)))
    end do

    print *, r
  end subroutine
end program
