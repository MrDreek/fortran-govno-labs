program upr5
    use Environment

    implicit none
    integer :: N = 10, i = 1
    real(R_), allocatable   :: X(:)
    real :: sum = 0
    character(*), parameter :: input_file = "../data/input.txt"
    integer :: In = 0

    allocate(X(N))

    open (file = input_file, newunit = In)
    read (In, *) X
    close (In)

    do while (X(i) > 1 .OR. X(i) < 0)
        sum = sum + X(i)
        i = i + 1
    end do

    write(* , *) "Сумма равна ", sum

    deallocate(X)
end program upr5
