program upr7_4
    use Environment

    implicit none
    character(*), parameter :: output_file = "../data/output.txt"
    integer :: Out = 0, N = 15, i = 0
    real(R_), allocatable :: A(:, :)

    allocate (A(N, N))

    call random_number(A)
    A = floor(A * 100._R_)

    open (file = output_file, encoding = E_, newunit = Out)
    write (Out, '(A)') "Начальный массив: "
    write (Out, '(15f6.1)') (A(i, :), i = 1, N)
    close (Out)

    call flip(A)

    open (file = output_file, encoding = E_, newunit = Out, position = 'append')
    write (Out, '(A)') "Новый массив: "
    write (Out, '(15f6.1)') (A(i, :), i = 1, N)

    close (Out)

contains
    pure subroutine flip(A)
        real(R_), intent(inout) :: A(:, :)
        real(R_) :: temp

        integer :: column_main_index, column_sub_index, i

        column_main_index = 1
        column_sub_index = N

        do concurrent(i = 1:N)
            temp = A(i, column_main_index)
            A(i, column_main_index) = A(i, column_sub_index)
            A(i, column_sub_index) = temp
            column_main_index = column_main_index + 1
            column_sub_index = column_sub_index - 1
        end do

    end subroutine flip
end program upr7_4
