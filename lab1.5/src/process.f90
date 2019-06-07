! Copyright 2015 Fyodorov S. A.

module Process
    ! Модуль с ЧИСТЫМИ процедурами обработки данных.
    use Environment
    use IO

    implicit none

contains
    ! Получение списков по полу.
    pure recursive subroutine Get_list_by_filter(Pers, List, Amount, VISITOR, YES)
        type(person), intent(in) :: Pers
        type(person), pointer :: List
        integer(I_), intent(inout) :: Amount
        character(kind = CH_), intent(in) :: VISITOR
        character(LEN = 3, kind = CH_), intent(in) :: YES

        ! Если найден студент нужного пола, то размещаем в новом списке элемент и копируем его данные.
        if (Pers%isVisitors == VISITOR .and. Pers%isArmies == YES) then
            allocate (List, source = Pers)
            Amount = Amount + 1
            List%next => Null()
            ! Если ещё остались студенты, сканируем дальше, а в создаваемом списке передаём место СОСЕДА.
            if (Associated(Pers%next)) &
                    call Get_list_by_filter(Pers%next, List%next, Amount, VISITOR, YES)
            ! Если ещё остались студенты, сканируем дальше, а в создаваемом списке передаём ПРЕЖНЕЕ место.
        else if (Associated(Pers%next)) then
            call Get_list_by_filter(Pers%next, List, Amount, VISITOR, YES)
        end if
    end subroutine Get_list_by_filter

    ! Сортировка списка класса по среднему баллу рекурсивно методом пузырька.
    pure recursive subroutine Sort_class_list(PersonsList, N)
        type(person), pointer, intent(inout) :: PersonsList
        integer, intent(in) :: N

        ! Работаем только с первыми N элементами: помещаем в ИХ конец менее успешного.
        call Drop_down(PersonsList, 1, N - 1)

        ! Если необходимо, делаем то же с последними N-1 элементами.
        if (N >= 3) &
                call Sort_class_list(PersonsList, N - 1)
    end subroutine Sort_class_list

    ! Помещаем c j-ой на N-ую позицию менее успешного, поочерёдно сравнивая.
    pure recursive subroutine Drop_down(PersonsList, j, N)
        type(person), pointer :: PersonsList
        integer, intent(in) :: j, N

        if (Swap(PersonsList)) &
                call Swap_from_current(PersonsList)
        if (j < N) &
                call Drop_down(PersonsList%next, j+1, N)
    end subroutine Drop_down

    ! Перестановка местами двух эелементов списка, начиная с текущего.
    pure subroutine Swap_from_current(Current)
        type(person), pointer  :: Current

        type(person), pointer  :: tmp_pers

        tmp_pers       => Current%next
        Current%next   => Current%next%next
        tmp_pers%next  => Current
        Current        => tmp_pers
    end subroutine Swap_from_current

    ! Проверка того, стоит ли менять местами текущего учащегося со следующим.
    pure logical function Swap(Current)
        type(person), intent(in)  :: Current

        Swap = .false.
        if (Current%Name > Current%next%Name) then
            Swap = .true.
        end if
    end function Swap
end module Process
