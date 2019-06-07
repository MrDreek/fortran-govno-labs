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
        type(person), pointer :: next => Null()
    end type person

contains
    ! Чтение списка класса: фамилии, инициалы, полы и оценки.
    function Read_class_list(Input_File) result(Persons)
        type(person), pointer :: Persons
        character(*), intent(in)   :: Input_File
        integer  In

        open (file=Input_File, encoding=E_, newunit=In)
            Persons => Read_person(In)
        close (In)
    end function Read_class_list

    ! Чтение следующего студента.
    recursive function Read_person(In) result(Pers)
        type(person), pointer  :: Pers
        integer, intent(in)     :: In
        integer  IO
        character(:), allocatable  :: format

        allocate (Pers)
        format = '(a, 1x, a, 1x, a, 1x, 1a, 1x, 1a)'
        read (In, format, iostat=IO) Pers%Name, Pers%Birth, Pers%isArmies, Pers%isVisitors, Pers%Gender
        call Handle_IO_status(IO, "reading line from file")
        if (IO == 0) then
            Pers%next => Read_person(In)
        else
            deallocate (Pers)
            nullify (Pers)
        end if
    end function Read_person

    subroutine Output_class_list(Output_File, Persons, List_Name, Position)
        character(*), intent(in)  :: Output_File, Position, List_Name
        type(person), intent(in)  :: Persons
        integer  :: Out

        open (file=Output_File, encoding=E_, position=Position, newunit=Out)
            write (out, '(/a)') List_Name
            call Output_person(Out, Persons)
        close (Out)
    end subroutine Output_class_list

    recursive subroutine Output_person(Out, Pers)
        integer, intent(in)        :: Out
        type(person), intent(in)  :: Pers

        integer  :: IO
        character(:), allocatable  :: format

        format = '(a, 1x, a, 1x, a, 1x, 1a, 1x, 1a)'
        write (Out, format, iostat=IO)  Pers%Name, Pers%Birth, Pers%isArmies, Pers%isVisitors, Pers%Gender
        call Handle_IO_status(IO, "writing person")
        if (Associated(Pers%next)) &
                call Output_person(Out, Pers%next)
    end subroutine Output_person
end module IO
