program colortest
  implicit none
  character        :: ESC, NL
  character(len=7) :: COLORSTART
  integer(kind=2)  :: i, ii
  character(len=4) :: COLOREND

  ESC = achar(27)
  NL = achar(10)
  COLORSTART = ESC // "[48;5;" ! Start of every escape sequence used
  COLOREND = ESC // "[0m"

  ! Print the first 16 colors - these vary by terminal configuration
  write (*, "(A)", advance="no") NL
  do i = 0, 15
    write (*, "(A, I0, A)", advance="no") COLORSTART, i, "m  "
  end do
  write (*, "(A, A, A)", advance="no") COLOREND, NL, NL

  ! Print the 6 sides of the color cube - these are more standardized
  ! but the order is a bit odd, thus the need for this trickery

  do i = 16, 46, 6
    do ii = 0, 5
      write (*, "(A, I0, A)", advance="no") COLORSTART, (i+ii), "m  "
    end do
    write (*, "(A, A)", advance="no") COLOREND, "  "
    do ii = 36, 41
      write (*, "(A, I0, A)", advance="no") COLORSTART, (i+ii), "m  "
    end do
    write (*, "(A, A)", advance="no") COLOREND, "  "
    do ii = 72, 77
      write (*, "(A, I0, A)", advance="no") COLORSTART, (i+ii), "m  "
    end do
    write (*, "(A, A)", advance="no") COLOREND, NL
  end do
  write(*, "(A)", advance="no") NL
  do i = 124, 154, 6
    do ii = 0, 5
      write (*, "(A, I0, A)", advance="no") COLORSTART, (i+ii), "m  "
    end do
    write (*, "(A, A)", advance="no") COLOREND, "  "
    do ii = 36, 41
      write (*, "(A, I0, A)", advance="no") COLORSTART, (i+ii), "m  "
    end do
    write (*, "(A, A)", advance="no") COLOREND, "  "
    do ii = 72, 77
      write (*, "(A, I0, A)", advance="no") COLORSTART, (i+ii), "m  "
    end do
    write (*, "(A, A)", advance="no") COLOREND, NL
  end do
  write(*, "(A)", advance="no") NL

  ! Finally, the 24 grays
  do i = 232, 255
    write (*, "(A, I0, A)", advance="no") COLORSTART, i, "m  "
  end do
  write(*,"(A, A, A)", advance="no") COLOREND, NL, NL

  ! vi: ft=fortran sw=2 sts=2 ts=2
end program colortest
