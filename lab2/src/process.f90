module Process
    use Environment
    use IO

    implicit none

    integer, parameter :: WORD_LEN = 20
contains

    pure recursive subroutine process_lines(source, destination, length, pattern, replacement)
        type(line), pointer :: source, destination
        integer(I_), intent(in) :: length
        character(*), intent(in) :: pattern, replacement
        character(WORD_LEN), allocatable :: words(:)
        integer i

        allocate (destination, source = source)
        call split_line(destination%value, words)
        destination%value = ''
        do i = 1, size(words)
            if (words(i) == pattern) words(i) = replacement
            if (i == 1) then
                destination%value = trim(words(i))
            else
                destination%value = trim(destination%value) // ' ' // trim(words(i))
            end if
        end do
        if (length > 1) then
            call process_lines(source%next, destination%next, length - 1, pattern, replacement)
        else
            destination%next => Null()
        end if
    end subroutine process_lines

    pure subroutine split_line(line, array)
        ! line  = input line
        ! array =  array of words
        character(*), intent(in) :: line
        character(*), allocatable, intent(out) :: array(:)
        ! temp vars
        character(WORD_LEN) :: word
        integer :: i, beg_idx, end_idx

        beg_idx = 0 ! begin index of the word
        end_idx = 0 ! end index of the word
        i = 1       ! character position in the line
        do while (i <= len(line))
            if ((beg_idx == 0) .and. (line(i:i) /= ' ')) then
                beg_idx = i
            end if
            if (beg_idx > 0) then
                if (line(i:i) == ' ') then
                    end_idx = i - 1
                end if
                if (i == len(line)) then
                    end_idx = i
                end if
                if (end_idx > 0) then
                    word = line(beg_idx:end_idx)
                    call AddToArray(array, word)
                    ! initialize indexces
                    beg_idx = 0
                    end_idx = 0
                end if
            end if
            i = i + 1
        end do
    end subroutine split_line

    pure subroutine AddToArray(array, element)
        character(*), intent(in) :: element
        character(*), allocatable, intent(inout) :: array(:)

        character(WORD_LEN), allocatable :: carray(:)

        if (allocated(array)) then
            allocate(carray(size(array) + 1), source = array)
            carray(size(carray)) = element
            deallocate(array)
            allocate(array, source = carray)
            deallocate(carray)
        else
            allocate(array(1))
            array(1) = element
        end if
    end subroutine AddToArray

end module Process