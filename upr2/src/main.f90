program upr2
    use Environment

    implicit none
    character(*), parameter    :: input_file = "../data/input.txt"
    integer                    :: In = 0
    integer                    :: a, b, c

    open (file=input_file, newunit=In)
    read (In, *) a, b, c
    close (In)

    if(a == b) then
        write(*, *) "Искомое число = ", c
    else if(a == c) then
        write(*, *) "Искомое число = ", b
    else if(b == c) then
        write(*, *) "Искомое число = ", a
    else
        write(*, *) "Нет одинаковых чисел"
    end if

end program upr2
