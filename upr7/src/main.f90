program upr7
    use Environment

    implicit none
    character(*), parameter :: input_file = "../data/input.txt", output_file = "../data/output.txt"
    integer                 :: In = 0, Out = 0, M = 0
    real(R_), allocatable   :: A(:)

    open (file=input_file, newunit=In)
    read (In, *) M
    allocate (A(M))
    read (In, *) A
    close (In)

    open (file=output_file, encoding=E_, newunit=Out)
    write (Out, "(f6.2)") A
    close (Out)

    call quicksort(A, 1, 100)

    open (file=output_file, encoding=E_, newunit=Out, position='append')
    write (Out, "(f12.2)") A
    close (Out)

contains
    pure recursive subroutine quicksort(A, first, last)
        implicit none
        real(R_), intent(inout)  ::  A(*)
        real(R_)  ::  x, t
        integer, intent(in) :: first, last
        integer :: i, j

        x = A( (first+last) / 2 )
        i = first
        j = last
        do
            do while (A(i) < x)
                i=i+1
            end do
            do while (x < A(j))
                j=j-1
            end do
            if (i >= j) exit
            t = A(i);  A(i) = A(j);  A(j) = t
            i=i+1
            j=j-1
        end do
        if (first < i-1) call quicksort(A, first, i-1)
        if (j+1 < last)  call quicksort(A, j+1, last)
    end subroutine quicksort
end program upr7
