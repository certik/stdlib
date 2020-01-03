program test_open
use stdlib_experimental_io, only: open, parse_mode
use stdlib_experimental_error, only: assert
implicit none

character(:), allocatable :: filename
integer :: u, a(3)

filename = get_outpath() // "/io_open.dat"

call test_parse_mode()

! Test mode "w"
u = open(filename, "w")
write(u, *) 1, 2, 3
close(u)

! Test mode "r"
u = open(filename, "r")
read(u, *) a
call assert(all(a == [1, 2, 3]))
close(u)

! Test mode "a"
u = open(filename, "a")
write(u, *) 4, 5, 6
close(u)
u = open(filename, "r")
read(u, *) a
call assert(all(a == [1, 2, 3]))
read(u, *) a
call assert(all(a == [4, 5, 6]))
close(u)


contains

    function get_outpath() result(outpath)
    integer :: ierr
    character(256) :: argv
    character(:), allocatable :: outpath

    call get_command_argument(1, argv, status=ierr)
    if (ierr==0) then
        outpath = trim(argv)
    else
        outpath = '.'
    endif
    end function get_outpath

    subroutine test_parse_mode()
    logical :: r
    r = .false.
    call assert(parse_mode("", r))
    call assert(r)
    end subroutine

end program
