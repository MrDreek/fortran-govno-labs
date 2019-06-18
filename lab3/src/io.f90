module IO
    use Environment

    implicit none

    type record
        integer(I_) :: Number = 0
        character(19, kind = CH_) :: Name = ""
    end type record

contains
    subroutine ReadList(InputFile, OutputFile, Records)
        type(record), pointer    :: Records(:)
        character(*), intent(in) :: InputFile, OutputFile

        character(:), allocatable  :: format
        type(record) :: tmpRecord
        integer      :: Out, In, IO
        format = '((i2, 1x), a19)'

        open (file=InputFile, encoding=E_, newunit=In)
        open (file=OutputFile, encoding=E_, newunit=Out)
        read (In, format, iostat=IO) tmpRecord
        do while(IO == 0)
            call AddToList(Records, tmpRecord)
            write (Out, format, iostat=IO) tmpRecord
            read (In, format, iostat=IO) tmpRecord
        end do
        close (Out)
        close (In)
    endsubroutine ReadList

    subroutine AddToList(list, element)
        type(record), intent(in) :: element
        type(record), pointer, intent(inout) :: list(:)
        type(record), pointer :: clist(:)

        if(associated(list)) then
            allocate(clist(size(list) + 1), source=list)
            clist(size(clist)) = element
            allocate(list, source=clist)
            deallocate(clist)
        else
            allocate(list(1))
            list(1) = element
        end if

    end subroutine AddToList

    subroutine OutputList(filename, Records)
        type(record), pointer    :: Records(:)
        character(*), intent(in) :: filename

        character(:), allocatable  :: format
        integer Out, IO, i
        format = '((i2, 1x), a19)'

        open (file=filename, encoding=E_, newunit=Out)
        do i = 1, size(Records)
            write (Out, format, iostat=IO) Records(i)
        end do
        close (Out)
    end subroutine OutputList
end module IO
