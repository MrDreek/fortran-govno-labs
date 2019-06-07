program upr6
    use Environment

    implicit none
    real(R_) :: ABSERR = 0, x = 0, arcsin_x = 0
    character(*), parameter :: input_file = "../data/input.txt"
    integer :: In = 0

    open (file = input_file, newunit = In)
    read (In, *) x, ABSERR
    close (In)

    call ArcSin(arcsin_x, x, ABSERR)

    write(*, *) arcsin_x

contains
    pure subroutine ArcSin(ArcSinX, x, ABSERR)
        real(R_), intent(in) :: x, ABSERR
        real(R_), intent(out) :: ArcSinX
        real(R_) :: currentStepValue
        integer :: step

        currentStepValue = x
        ArcSinX = 0
        step = 0

        do
            ArcSinX = ArcSinX + currentStepValue
            currentStepValue = (currentStepValue * (x ** 2) * ((2 * step + 1) ** 2)) / (2 * (step + 1) * (2 * step + 3))
            if (abs(currentStepValue / ArcSinX) < ABSERR) exit
            step = step + 1
        end do
    end subroutine ArcSin
end program upr6
