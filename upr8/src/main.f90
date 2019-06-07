program upr8
    use Environment

    implicit none
    character(*), parameter :: input_file = "../data/input.txt", output_file = "../data/output.txt"
    integer :: In = 0, Out = 0, M = 15, N = 20, i
    character, allocatable :: C(:, :)

    allocate (C(N, M))

    open (file = input_file, newunit = In)
    do i = 1, N
        read (In, '(15A)') C(i, :)
    end do
    close (In)

    do concurrent (i = 1:N)
        call sort(C(i, :), M)
    end do

    open (file = output_file, encoding = E_, newunit = Out)
    write (Out, '(15A)') (C(i, :), i = 1, N)
    close (Out)

contains
    pure subroutine sort(A, N)
        character, intent(inout) :: A(*)
        integer, intent(in) :: N
        integer :: i, j
        character :: t

        do i = N - 1, 1, -1
            do j = 1, i
                if (A(j) > A(j + 1)) then
                    t = A(j)
                    A(j) = A(j + 1)
                    A(j + 1) = t
                end if
            end do
        end do
    end subroutine sort
end program upr8