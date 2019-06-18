module MathIO
    use Environment

    implicit none
contains

    subroutine ReadData(inputFile, outputFile, array, bool, OPERANDS)
        character(*), allocatable, intent(inout) :: array(:)
        character, intent(in)                    :: OPERANDS(:)
        logical(I_), intent(inout)               :: bool(:)
        character(*), intent(in)                 :: inputFile, outputFile

        character(:), allocatable :: format
        character                 :: element, tmpBool(3)
        integer(I_) out, in, io
        format = '(a1)'

        open (file=inputFile, encoding=E_, newunit=in)
        open (file=outputFile, encoding=E_, newunit=out)
        read (in, format, iostat=io, ADVANCE = "NO") element
        do while(io == 0)
            write (out, format, iostat=io, ADVANCE = "NO") element
            call AddToArray(array, element)
            read (in, format, iostat=io, ADVANCE = "NO") element
        end do
        format = '(3a1)'
        write (out, *)
        read (in, format, iostat=io) tmpBool
        do while(io == 0)
            write (out, format, iostat=io) tmpBool
            bool(findloc(OPERANDS, tmpBool(1))) = tmpBool(3) == 't'
            read (in, format, iostat=io) tmpBool
        end do
        close (out)
        close (in)
    endsubroutine ReadData

    pure subroutine AddToArray(array, element)
        character(*), intent(in)                 :: element
        character(*), allocatable, intent(inout) :: array(:)

        character, allocatable :: carray(:)

        if (allocated(array)) then
            allocate(carray(size(array) + 1), source=array)
            carray(size(carray)) = element
            deallocate(array)
            allocate(array, source=carray)
            deallocate(carray)
        else
            allocate(array(1))
            array(1) = element
        end if

    end subroutine AddToArray

    subroutine WriteData(filename, data)
        logical(I_), intent(in) :: data
        character(*), intent(in)         :: filename

        integer(I_) out

        open (file=filename, encoding=E_, position="append", newunit=out)
        write (out, '(a, l)') "Результат: ", data
        close (out)
    end subroutine WriteData

end module MathIO