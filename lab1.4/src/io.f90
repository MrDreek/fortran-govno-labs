module IO
    use Environment

    implicit none
    integer, parameter :: PERSON_AMOUNT = 6
    integer, parameter :: NAME_LEN = 15
    integer, parameter :: BIRTH_LEN = 4
    integer, parameter :: IS_IN_ARMY_LEN = 3
    integer, parameter :: IS_VISITOR_LEN = 1
    integer, parameter :: GENDER_LEN = 1

    ! Структура данных для хранения данных о person.
    type person
        character(NAME_LEN, kind = CH_) :: Name = ""
        character(BIRTH_LEN, kind = CH_) :: Birth = ""
        character(IS_IN_ARMY_LEN, kind = CH_) :: isArmies = ""
        character(kind = CH_) :: isVisitors = ""
        character(kind = CH_) :: Gender = ""
    end type person

contains
    ! Создание неформатированного файла данных.
    subroutine Create_data_file(Input_File, Data_File)
        character(*), intent(in) :: Input_File, data_file

        type(person) :: pers
        integer :: In, Out, IO, i, recl
        character(:), allocatable :: format

        open (file = Input_File, encoding = E_, newunit = In)
            recl = (NAME_LEN + BIRTH_LEN + IS_IN_ARMY_LEN + IS_VISITOR_LEN + GENDER_LEN) * CH_
            open (file = Data_File, form = 'unformatted', newunit = Out, access = 'direct', recl = recl)
                format = '(a, 1x, a, 1x, a, 1x, 1a, 1x, 1a)'
                do i = 1, PERSON_AMOUNT
                    read (In, format, iostat = IO) pers
                    call Handle_IO_status(IO, "reading formatted class list, line " // i)
                    write (Out, iostat = IO, rec = i) pers
                    call Handle_IO_status(IO, "creating unformatted file with class list, record " // i)
                end do
            close (Out)
        close (In)
    end subroutine Create_data_file

    ! Чтение списка класса: фамилии, инициалы, полы и оценки.
    function Read_class_list(Data_File) result(Persons)
        type(person)                Persons(PERSON_AMOUNT)
        character(*), intent(in) :: Data_File

        integer In, IO, recl

        recl = ((NAME_LEN + BIRTH_LEN + IS_IN_ARMY_LEN + IS_VISITOR_LEN + GENDER_LEN) * CH_) * PERSON_AMOUNT
        open (file = Data_File, form = 'unformatted', newunit = In, access = 'direct', recl = recl)
            read (In, iostat = IO, rec = 1) Persons
        close (In)
    end function Read_class_list

    ! Вывод списка класса.
    subroutine Output_class_list(Output_File, Persons, List_name, Position)
        character(*), intent(in) :: Output_File, Position, List_name
        type(person), intent(in) :: Persons(:)

        integer :: Out, IO
        character(:), allocatable :: format

        open (file = Output_File, encoding = E_, position = Position, newunit = Out)
            write (out, '(/a)') List_name
            format = '(a, 1x, a, 1x, a, 1x, 1a, 1x, 1a)'
            write (Out, format, iostat = IO) Persons
            call Handle_IO_status(IO, "writing " // List_name)
        close (Out)
    end subroutine Output_class_list
end module IO
