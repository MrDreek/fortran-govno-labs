program upr7_2
    use Environment

    implicit none
    character(*), parameter :: output_file = "../data/output.txt"
    integer :: Out = 0, N = 10, i = 0
    real(R_), allocatable :: A(:, :)
    real(R_) :: norm

    norm = 0

    allocate (A(N, N))

    call RANDOM_NUMBER(A)

    open (file = output_file, encoding = E_, newunit = Out)
    write (Out, '(10f6.2)') (A(i, :), i = 1, N)
    close (Out)

    call normFinder(A, norm)

    open (file = output_file, encoding = E_, newunit = Out, position = 'append')
    write (Out, '(A, f6.2)') 'Ответ:', norm
    close (Out)

contains
    pure subroutine normFinder(A, norm)
        implicit none
        real(R_), intent(in) :: A(:, :)
        real(R_), intent(out) :: norm

        norm = SQRT(sum(A ** 2))

    end subroutine normFinder
end program upr7_2
