! SPDX-FileCopyrightText: 2023 - 2025 Eli Array Minkoff
! SPDX-License-Identifier: GPL-3.0-only

program colortest
  implicit none
  character        :: ESC, NL
  integer          :: I
  character(len=4) :: COLOREND
  ESC = achar(27)
  NL = achar(10)
  COLOREND = ESC // "[0m"

  ! Print the first 16 colors - these vary by terminal configuration
  write (*, "(A)", advance="no") NL
  do i = 0, 15
    call color_cell(i)
  end do
  write (*, "(A, A, A)", advance="no") COLOREND, NL, NL

  ! Print the 6 sides of the color cube - these are more standardized
  ! but the order is a bit odd, thus the need for the below trickery
  do i = 16, 46, 6
    call cube_row(i)
  end do
  write (*, "(A)", advance="no") NL
  do i = 124, 154, 6
    call cube_row(i)
  end do
  write (*, "(A)", advance="no") NL

  ! Finally, the 24 grays
  do i = 232, 255
    call color_cell(i)
  end do
  write (*,"(A, A, A)", advance="no") COLOREND, NL, NL

contains

  subroutine color_cell(N)
    integer, intent(in) :: N
    write (*, "(A, I0, A)", advance="no") ESC // "[48;5;", N, "m  "
  end subroutine color_cell

  subroutine cube_row_part(N)
    integer, intent(in) :: N
    integer :: II ! internal iterator
    do II = N, N+5
      call color_cell(II)
    end do
    write (*, "(A)", advance="no") ESC // "[0m"
  end subroutine cube_row_part

  subroutine cube_row(N)
    integer, intent(in) :: N
    call cube_row_part(N)
    write (*, "(A)", advance="no") "  "
    call cube_row_part(N+36)
    write (*, "(A)", advance="no") "  "
    call cube_row_part(N+72)
    write (*, "(A)", advance="no") NL
  end subroutine cube_row
end program colortest

! vi: ft=fortran sw=2 sts=2 ts=2
