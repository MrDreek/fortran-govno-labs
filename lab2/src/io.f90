module IO
    use Environment

    implicit none

    type word
        character(80, kind = CH_) :: value = ""
        type(word), pointer :: next => Null()
    end type word

contains
    function Read_words(Input_File) result(words)
        type(word), pointer :: words
        character(*), intent(in) :: Input_File
        integer  In

        open (file = Input_File, encoding = E_, newunit = In)
        words => Read_word(In)
        close (In)
    end function Read_words

    recursive function Read_word(In) result(wds)
        type(word), pointer :: wds
        integer, intent(in) :: In
        integer  IO
        character(:), allocatable :: format

        allocate (wds)
        format = '(a, /)'
        read (In, format, iostat = IO) wds%value
        if (IO == 0) then
            wds%next => Read_word(In)
        else
            deallocate (wds)
            nullify (wds)
        end if
    end function Read_word

    subroutine OutputList(Output_File, Words, List_Name, Position)
        character(*), intent(in) :: Output_File, Position, List_Name
        type(word), intent(in) :: Words
        integer :: Out

        open (file = Output_File, encoding = E_, position = Position, newunit = Out)
        write (out, '(/a)') List_Name
        call OutputWord(Out, Words)
        close (Out)
    end subroutine OutputList

    recursive subroutine OutputWord(Out, Words)
        integer, intent(in) :: Out
        type(word), intent(in) :: Words

        integer :: IO
        character(:), allocatable :: format

        format = '(a, 1x)'
        write (Out, format, iostat = IO)  Words%value
        call Handle_IO_status(IO, "writing word")
        if (Associated(Words%next)) &
                call OutputWord(Out, Words%next)
    end subroutine OutputWord
end module IO
