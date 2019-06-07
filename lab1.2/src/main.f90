program upr8_2
    use Environment

    implicit none

    character(:), allocatable :: input_file, output_file
    integer, parameter :: PERSON_AMOUNT = 6, NAME_LEN = 15, BIRTH_LEN = 4, IS_IN_ARMY_LEN = 3, IS_VISITOR_LEN = 1, &
            GENDER_LEN = 1
    character(kind = CH_), parameter :: VISITOR = Char(1057, CH_), NOT_VISITOR = Char(1055, CH_)
    character(LEN = 3, kind = CH_) :: YES = Char(1076, CH_) // Char(1072, CH_) // Char(32, CH_)

    character(kind = CH_) :: Names(PERSON_AMOUNT, NAME_LEN) = "", &
            Births(PERSON_AMOUNT, BIRTH_LEN) = "", &
            Genders(PERSON_AMOUNT, GENDER_LEN) = "", &
            isVisitors(PERSON_AMOUNT) = "", &
            isArmies(PERSON_AMOUNT, IS_IN_ARMY_LEN) = ""

    character(kind = CH_), allocatable :: vNames(:, :), pNames(:, :)
    character(kind = CH_), allocatable :: vBirths(:, :), pBirths(:, :)
    character(kind = CH_), allocatable :: vGenders(:, :), pGenders(:, :)

    input_file = "../data/input.txt"
    output_file = "../data/output.txt"

    call Read_list(input_file, Names, Births, Genders, isArmies, isVisitors)

    call Output_list_full(output_file, Names, Births, Genders, isArmies, isVisitors, "Исходный список:", "rewind")

    call Get_list(Names, Births, Genders, isArmies, isVisitors, vNames, vGenders, vBirths, VISITOR, YES)
    call Get_list(Names, Births, Genders, isArmies, isVisitors, pNames, pGenders, pBirths, NOT_VISITOR, YES)

    call Sort(vNames, vGenders, vBirths)
    call Sort(pNames, pGenders, pBirths)

    call Output_list(output_file, pNames, pBirths, pGenders, "Служившие петербуржцы:", "append")
    call Output_list(output_file, vNames, vBirths, vGenders, "Служившие гости:", "append")


contains
    subroutine Read_list(Input_File, Names, Births, Genders, isArmies, isVisitors)
        character(*)         Input_File
        character(kind = CH_)  Names(:, :), Births(:, :), Genders(:, :), isArmies(:, :), isVisitors(:)
        intent (in)          Input_File
        intent (out)         Names, Births, Genders, isArmies, isVisitors

        integer In, IO, i
        character(:), allocatable :: format

        open (file = Input_File, encoding = E_, newunit = In)
        format = '(15a, 1x, 4a, 1x, 3a, 1x, a1, 1x, a1)'
        read (In, format, iostat = IO) (Names(i, :), Births(i, :), isArmies(i, :), isVisitors(i), Genders(i, :), &
                i = 1, PERSON_AMOUNT)
        close (In)
    end subroutine Read_list

    ! Вывод списка класса.
    subroutine Output_list_full(Output_File, Names, Births, Genders, isArmies, isVisitors, List_name, Position)
        character(*)         Output_File, Position, List_name
        character(kind = CH_)  Names(:, :), Births(:, :), Genders(:, :), isArmies(:, :), isVisitors(:)
        intent (in)         Output_File, Names, Births, Genders, isArmies, isVisitors, List_name, Position

        integer :: Out, i, IO
        character(:), allocatable :: format

        open (file = output_file, encoding = E_, position = position, newunit = Out)
        write (out, '(/a)') List_name
        format = '(15a1, 1x, 4a1, 1x, 3a1, 1x, a1, 1x, a1)'
        write (Out, format, iostat = IO)  (Names(i, :), Births(i, :), isArmies(i, :), isVisitors(i), Genders(i, :), &
                i = 1, Size(isVisitors))
        close (Out)
    end subroutine Output_list_full


    ! Вывод списка класса.
    subroutine Output_list(Output_File, Names, Births, Genders, List_name, Position)
        character(*)         Output_File, Position, List_name
        character(kind = CH_)  Names(:, :), Births(:, :), Genders(:, :)
        intent (in)         Output_File, Names, Births, Genders, List_name, Position

        integer :: Out, i, IO
        character(:), allocatable :: format

        open (file = output_file, encoding = E_, position = position, newunit = Out)
        write (out, '(/a)') List_name
        format = '(15a1, 1x, 4a1, 1x, 1a1)'
        write (Out, format, iostat = IO)  (Names(i, :), Births(i, :), Genders(i, :), i = 1, Size(Names(:, 1)))
        close (Out)
    end subroutine Output_list


    ! Получение списков.
    pure subroutine Get_list(Names, Births, Genders, isArmies, isVisitors, vNames, vGenders, vBirths, VISITOR, YES)
        character(kind = CH_)  Names(:, :), Births(:, :), Genders(:, :), isArmies(:, :), isVisitors(:)
        character(kind = CH_)  vNames(:, :), vGenders(:, :), vBirths(:, :)

        character(kind = CH_)  VISITOR, YES
        intent(in)           Names, Births, Genders, isArmies, isVisitors, VISITOR, YES
        intent(out)          vNames, vGenders, vBirths
        allocatable          vNames, vGenders, vBirths

        logical, allocatable :: IsNeededObject(:)
        integer, allocatable :: vPos(:)
        integer :: Amount, i
        integer, parameter :: INDEXES(*) = [(i, i = 1, PERSON_AMOUNT)]

        ! Составление логической маски, соответствующей полу.
        IsNeededObject = isVisitors == VISITOR .and. YES == isArmies(:, 1)
        Amount = Count(IsNeededObject)

        vPos = Pack(INDEXES, IsNeededObject)
        allocate (vNames(Amount, NAME_LEN), vBirths(Amount, BIRTH_LEN), vGenders(Amount, GENDER_LEN))
        do concurrent (i = 1:Amount)
            vNames(i, :) = Names(vPos(i), :)
            vGenders(i, :) = Genders(vPos(i), :)
            vBirths(i, :) = Births(vPos(i), :)
        end do
    end subroutine Get_list

    pure subroutine Sort(Names, Genders, Births)
        character(kind = CH_)  Names(:, :), Genders(:, :), Births(:, :)
        intent (inout)       Names, Genders, Births

        integer              i, j

        do i = Size(Names(:,1)), 2, -1
            do j = 1, i - 1
                if (Swap(Names, j)) then
                    call Swap_from_current(Names, Genders, Births, j)
                end if
            end do
        end do
    end subroutine Sort

    ! Проверка того, стоит ли менять местами текущего учащегося со следующим.
    pure logical function Swap(Names, j)
        character(kind = CH_)  Names(:, :)
        integer              j
        intent (in) :: Names, j

        Swap = .false.
        if (GT(Names(j, :), Names(j + 1, :))) then
            Swap = .true.
        end if
    end function Swap

    ! Функция операции > для массивов символов.
    pure logical function GT(arr1, arr2)
        character(kind = CH_), intent(in) :: arr1(:), arr2(:)

        integer :: i

        ! Поиск первого отличного символа или остановка на последнем символе.
        do i = 1, Min(Size(arr1), Size(arr2)) - 1
            if (arr1(i) /= arr2(i)) &
                    exit
        end do
        GT = arr1(i) > arr2(i)
    end function GT

    pure subroutine Swap_from_current(Names, Genders, Births, j)
        character(kind = CH_)  Names(:, :), Genders(:, :), Births(:, :)
        integer, intent(in) :: j
        intent (inout) :: Names, Genders, Births

        character(kind = CH_)  tmpName(NAME_LEN), tmpGender(GENDER_LEN), tmpBirth(BIRTH_LEN)

        tmpName = Names(j, :)
        Names(j, :) = Names(j + 1, :)
        Names(j + 1, :) = tmpName
        tmpGender = Genders(j, :)
        Genders(j, :) = Genders(j + 1, :)
        Genders(j + 1, :) = tmpGender
        tmpBirth = Births(j, :)
        Births(j, :) = Births(j + 1, :)
        Births(j + 1, :) = tmpBirth
    end subroutine Swap_from_current

end program upr8_2
