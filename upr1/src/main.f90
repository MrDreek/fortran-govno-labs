function fact(n)
    integer fact, n, p, i
    p = 1
    do i = 1, n
        p = p * i
    end do
    fact = p
end

program upr1
    use Environment

    implicit none

    real    :: value = 1, x = 0.9, temp, sqr
    integer :: fact, i, factValue

    do i = 2, 6, 2
        factValue = fact(i)
        sqr =  x**i
        temp = sqr / factValue
        value = value + ((-1) ** (i / 2) * temp)
        write(*, "(I1,A,I3)") i, '! = ' , factValue
        write(*, "(A,I1,A,F10.8)") 'x^', i, ' = ', sqr
        write(*, "(A,I1,A,I1,A,F10.8)") 'x^', i, ' / ', i, '! = ' , temp
        write(*, "(A,F10.8)") 'cos(x) = ',  value
    end do

end program upr1
