module Process
    use Environment
    use IO

    implicit none

contains
    pure subroutine removeItems(records)
        type(record), pointer, intent(inout) :: records(:)
        type(record), allocatable :: tmpRecords(:)
        type(record) :: lastElement
        integer i, count
        count = 0
        allocate(tmpRecords(size(records)))

        lastElement = records(size(records))

        do i = 1, size(records)
            if (records(i)%Name /= lastElement%Name) then
                count = count + 1
                tmpRecords(count) = records(i)
            end if
        end do

        deallocate(records)
        allocate(records(count), source=tmpRecords)

    end subroutine removeItems
end module Process
