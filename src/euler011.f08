program euler011
  implicit none

  real :: start, finish

  call cpu_time(start)
  call solution()
  call cpu_time(finish)

  print '("Elapsed time: ", F9.6, " seconds")', finish - start
contains
  subroutine read_table(tbl)
    integer, dimension(20, 20), intent(out) :: tbl
    integer :: i, j

    open(10, file='data/euler011.txt')
    do i = 1, 20
      read(10, fmt='(20I3)') tbl(:, i)
    end do
    close(10)
  end subroutine

  pure function get_value(tbl, i, j) result (r)
    integer, dimension(20, 20), intent(in) :: tbl
    integer, intent(in) :: i, j
    integer :: r

    if (i < 1 .or. j < 1 .or. i > 20 .or. j > 20) then
      r = 0
    else
      r = tbl(i, j)
    end if
  end function

  pure function get_product(tbl, i, j, dir) result (r)
    integer, dimension(20, 20), intent(in) :: tbl
    integer, intent(in) :: i, j, dir
    integer :: r

    integer :: di, dj, t, ti, tj, n
    integer, dimension(2, 8) :: o

    o = reshape((/ 0, -1, &
                  -1, -1, &
                  -1,  0, &
                  -1,  1, &
                   0,  1, &
                   1,  1, &
                   1,  0, &
                   1, -1 /), (/ 2, 8 /))

    di = o(1, dir)
    dj = o(2, dir)

    ti = i
    tj = j
    r = 1
    do n = 1, 4
      if (ti < 1 .or. tj < 1 .or. ti > 20 .or. tj > 20) then
        t = 0
      else
        t = tbl(ti, tj)
      end if
      r = r * t
      ti = ti + di
      tj = tj + dj
    end do
  end function

  subroutine solution()
    use euler_mod

    implicit none

    integer, dimension(20, 20) :: tbl
    integer :: i, j, dir, r

    call read_table(tbl)

    r = 0
    do i = 1, 20
      do j = 1, 20
        do dir = 1, 8
          r = max(r, get_product(tbl, i, j, dir))
        end do
      end do
    end do

    print *, r

  end subroutine
end program
