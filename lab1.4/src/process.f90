! Copyright 2015 Fyodorov S. A.

module Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use IO

   implicit none
   
contains

   ! Сортировка списка класса по среднему баллу рекурсивно методом пузырька.
   pure recursive subroutine Sort_class_list(Persons, N)
      type(person), intent(inout)  :: Persons(:)
      integer, intent(in)          :: N

      ! Работаем только с первыми N элементами: помещаем в ИХ конец менее успешного.
      call Drop_down(Persons, 1, N-1)

      ! Если необходимо, делаем то же с последними N-1 элементами.
      if (N >= 3) &
              call Sort_class_list(Persons, N-1)
   end subroutine Sort_class_list

   ! Помещаем c j-ой на N-ую позицию менее успешного, поочерёдно сравнивая.
   pure recursive subroutine Drop_down(Persons, j, N)
      type(person), intent(inout)  :: Persons(:)
      integer, intent(in)           :: j, N

      type(person)  :: tmp_pers

      ! Если требуется, то меняем местами текущего студента со следующим.
      if (Swap(Persons, j)) then
         ! Перестановка местами двух эелементов списка, начиная с текущего.
         tmp_pers = Persons(j+1)
         Persons(j+1) = Persons(j)
         Persons(j) = tmp_pers
      end if
      if (j < N) &
              call Drop_down(Persons, j+1, N)
   end subroutine Drop_down

   ! Проверка того, стоит ли менять местами текущего учащегося со следующим.
   pure logical function Swap(Persons, j)
      type(person), intent(in)  :: Persons(:)
      integer, intent(in)        :: j

      Swap = .false.
      if (Persons(j)%Name > Persons(j+1)%Name) then
         Swap = .true.
      end if
   end function Swap
end module Process
