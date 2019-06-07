program upr7_5
    use Environment

    implicit none
    character(*), parameter :: output_file = "../data/output.txt"
    integer :: Out = 0, N = 10, M = 15, i = 0
    real(R_), allocatable :: A(:, :), C(:)

    allocate (A(N, M))

    call random_number(A)
    A = -50 + floor(A * 100._R_)

    open (file = output_file, encoding = E_, newunit = Out)
    write (Out, '(A)') "Начальный массив: "
    write (Out, '(15f6.1)') (A(i, :), i = 1, N)
    close (Out)

    C = [([pack(A(i, :), A(i, :) > 0)], i = 1, N)]

    open (file = output_file, encoding = E_, newunit = Out, position = 'append')
    write (Out, '(A)') "Новый массив: "
    write (Out, '(f6.1)')  C

    close (Out)
end program upr7_5
