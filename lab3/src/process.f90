module Process
    use Environment
    use IO

    implicit none

contains
    pure subroutine removeItems(records)
        type(record), pointer, intent(inout) :: records(:)
        type(record) :: lastElement
        integer i

        lastElement = records(size(records) - 1)

        do i = 1, size(records)
            if (records(i)%Name == lastElement%Name) then
!                Как удалить объект из массива?
            end if
        end do
    end subroutine removeItems
end module Process
