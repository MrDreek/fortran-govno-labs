module IO
    use Environment

    implicit none

    integer, parameter :: LINE_LEN = 80

    type line
        character(LINE_LEN) :: value = ""
        type(line), pointer :: next => Null()
    end type line

contains
    function ReadList(InputFile, OutputFile, Amount) result(lines)
        type(line), pointer :: lines
        integer(I_), intent(inout) :: Amount
        character(*), intent(in) :: InputFile, OutputFile
        integer  In, Out

        open (file = InputFile, encoding = E_, newunit = In)
            open (file = OutputFile, encoding = E_, newunit = Out)
                lines => readNext(In, Out, Amount)
            close (Out)
        close (In)
    endfunction ReadList

    recursive function readNext(In, Out, Amount) result(lines)
        type(line), pointer :: lines
        integer(I_), intent(inout) :: Amount
        integer, intent(in) :: In, Out
        integer  IO
        character(:), allocatable :: format

        allocate (lines)
        format = '(a)'
        read (In, format, iostat = IO) lines%value
        if (IO == 0) then
            write (Out, format, iostat = IO) lines%value
            Amount = Amount + 1
            lines%next => readNext(In, Out, Amount)
        else
            deallocate (lines)
            nullify (lines)
        end if
    end function readNext

    subroutine OutputList(filename, lines)
        type(line) :: lines
        character(*) :: filename
        intent(in) :: filename, lines

        integer :: Out
        open (file = filename, encoding = E_, position = 'append', newunit = Out)
        write (Out, '(a)') "---------"
        write (Out, '(a)') "С заменой"
        write (Out, '(a)') "---------"
        call WriteToFile(Out, lines)
        close (Out)
    end subroutine OutputList

    recursive subroutine WriteToFile(Out, lines)
        type(line) :: lines
        integer :: Out
        intent(in) :: Out, lines

        character(:), allocatable :: format
        integer :: IO

        format = '(a)'
        write (Out, format, iostat = IO) lines%value
        if (Associated(lines%next)) &
                call WriteToFile(Out, lines%next)
    end subroutine WriteToFile
end module IO