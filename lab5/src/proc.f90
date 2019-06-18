module MathProcess
    use Environment
    use MathIO

    implicit none

    integer(I_), parameter :: OK = 0, ERR = 1, WIP = 2
contains

    pure subroutine Calculate(infix, OPERANDS, bool, result)
        character, allocatable, intent(in) :: infix(:)
        character, intent(in)              :: OPERANDS(:)
        logical(I_), intent(in)            :: bool(:)
        logical(I_), intent(out)           :: result

        character, allocatable :: postfix(:), opStack(:)
        character   :: op1, op2
        logical(I_) :: res
        integer(I_) i

        !------------!
        !convertation!
        !------------!
        do i = 1, size(infix)
            if (any(OPERANDS == infix(i))) then
                call AddToArray(postfix, infix(i))
            elseif (infix(i) == "(") then
                call AddToArray(opStack, infix(i))
            elseif (infix(i) == ")") then
                call ArrayPop(opStack, op1)
                do while(op1 /= "(")
                    call AddToArray(postfix, op1)
                    call ArrayPop(opStack, op1)
                end do
            else
                do while(size(opStack) /= 0 .and. &
                        (opStack(size(opStack)) == "a" .and. infix(i) == "o" &
                                .or. opStack(size(opStack)) /= "(" .and. infix(i) == "("))
                    call ArrayPop(opStack, op1)
                    call AddToArray(postfix, op1)
                end do
                call AddToArray(opStack, infix(i))
            end if
        end do
        do while(size(opStack) /= 0)
            call ArrayPop(opStack, op1)
            call AddToArray(postfix, op1)
        end do

        !-----------------!
        !   calculation   !
        !-----------------!
        do i = 1, size(postfix)
            if (any(OPERANDS == postfix(i))) then
                call AddToArray(opStack, postfix(i))
            else
                call ArrayPop(opStack, op1)
                call ArrayPop(opStack, op2)
                if (postfix(i) == 'a') then
                    res = any(bool(findloc(OPERANDS, op1)) .and. bool(findloc(OPERANDS, op2)))
                else
                    res = any(bool(findloc(OPERANDS, op1)) .or. bool(findloc(OPERANDS, op2)))
                end if
                if (res) then
                    call AddToArray(opStack, 't')
                else
                    call AddToArray(opStack, 'f')
                end if
            end if

        end do
        call ArrayPop(opStack, op1)
        res = any(bool(findloc(OPERANDS, op1)))
        result = res
    end subroutine Calculate

    pure subroutine ArrayPop(array, result)
        character, allocatable, intent(inout) :: array(:)
        character, intent(inout)              :: result
        character, allocatable                :: carray(:)

        if (allocated(array)) then
            allocate(carray(size(array) - 1), source=array)
            result = array(size(array))
            deallocate(array)
            allocate(array, source=carray)
            deallocate(carray)
        end if
    end subroutine ArrayPop

end module MathProcess
