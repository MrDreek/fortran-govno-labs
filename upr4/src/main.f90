program upr4
    use Environment

    implicit none
    integer(I_) :: i
    real(R_) :: X(0:4) = 0

    forall(i = 0:4) X(i) = (263-i*20)/80.

    print *, X

    write(*, *) sin(3 * X) - 3 * sin(X)

end program upr4
