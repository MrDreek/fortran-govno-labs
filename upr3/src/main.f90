function fact(n)
    integer fact, n, p, i
    p = 1
    do i = 1, n
        p = p * i
    end do
    fact = p
end

program upr3
    use Environment

    implicit none
    integer :: n, fact
    real :: y = 1

    character(*), parameter :: input_file = "../data/input.txt"
    integer :: In = 0
    integer :: i

    open (file = input_file, newunit = In)
    read (In, *) i
    close (In)

    do n = 1, i, 1
        y = y * ((n + 1) / fact(n) + n)
        write(*, *) "Произведение равно ", y
    end do

!    write(*, *) "Произведение равно ", y

end program upr3
