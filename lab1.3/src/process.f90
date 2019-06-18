! Copyright 2015 Fyodorov S. A.

module Process
    ! Модуль с ЧИСТЫМИ процедурами обработки данных.
    use Environment
    use IO

    implicit none

contains
    ! Сортировка списка класса по среднему баллу.
    pure subroutine Sort_class_list(Persons)
        type(person), intent(inout) :: Persons(:)

        integer :: i, j
        type(person) :: tmp_pers

        ! Сортировка списка класса по среднему баллу методом пузырька.
        do i = Size(Persons), 2, -1
            ! Просматриваем список с начала, ставя в конец менее успешного.
            do j = 1, i - 1
                ! Проверка на то, стоит ли менять учащихся местами.
                if (Swap(Persons, j)) then
                    ! Перестановка местами двух эелементов списка, начиная с текущего.
                    tmp_pers = Persons(j + 1)
                    Persons(j + 1) = Persons(j)
                    Persons(j) = tmp_pers
                end if
            end do
        end do
    end subroutine Sort_class_list

    ! Проверка того, стоит ли менять местами текущего учащегося со следующим.
    pure logical function Swap(Persons, j)
        type(person), intent(in) :: Persons(:)
        integer, intent(in) :: j

        Swap = .false.
        if (Persons(j)%Name > Persons(j + 1)%Name) then
            Swap = .true.
        end if
    end function Swap
end module Process
