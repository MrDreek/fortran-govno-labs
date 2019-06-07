program upr7_3
    use Environment

    implicit none
    character(*), parameter :: output_file = "../data/output.txt"
    integer :: Out = 0, N = 100, M = 50, i = 0
    real(R_), allocatable :: A(:, :)
    real(R_) :: max = 0
    integer, allocatable :: Indexes(:, :), Ind_max(:, :)
    logical, allocatable :: Mask(:)

    allocate (A(N, M))

    call random_number(A)
    A = floor(A * 100._R_)

    open (file = output_file, encoding = E_, newunit = Out)
    write (Out, '(50f6.1)') (A(i, :), i = 1, N)
    close (Out)

    allocate (Indexes(N * M, 2))
    allocate (Mask(N * M), source = .false.)

    call findMax(A, max, Mask, Indexes, Ind_max)

    open (file = output_file, encoding = E_, newunit = Out, position = 'append')
    write (Out, '(a, f6.1)') "Наибольшее число:", max
    write (Out, '(2i3)') (Ind_max(i, :), i = 1, UBound(Ind_max, 1))
    close (Out)

contains
    pure subroutine findMax(A, max, Mask, Indexes, Ind_max)
        real(R_), intent(in) :: A(:, :)
        real(R_), intent(out) :: max
        integer, intent(out) :: Indexes(:, :)
        integer, allocatable, intent(out) :: Ind_max(:, :)
        logical, intent(out) :: Mask(:)

        integer N_max, i, j

        ! Формируем двумерный массив индексов:
        ! | 1 | 1 |
        ! | 2 | 1 |
        ! ...
        ! | N | 1 |
        ! ...
        ! | 1 | 2 |
        ! | 2 | 2 |
        ! ...
        ! | N | 2 |
        ! ...
        ! | 1 | M |
        ! | 2 | M |
        ! ...
        ! | N | M |
        Indexes(:, 1) = [((i, i = 1, N), j = 1, M)]
        Indexes(:, 2) = [((j, i = 1, N), j = 1, M)]

        ! Поиск наибольшего.
        max = MaxVal(A)
        ! Получаем маску для элементов, равных наибольшему.
        Mask = [A == max]
        N_max = Count(Mask)

        ! Формируем массив индексов, удовлетворяющих заданной маске:

        ! Размещение массивов индексов.
        allocate(Ind_max(N_max, 2))
        ! Упаковка массива индексов по каждой из координат.
        Ind_max(:, 1) = Pack(Indexes(:, 1), Mask)
        Ind_max(:, 2) = Pack(Indexes(:, 2), Mask)

    end subroutine findMax
end program upr7_3
