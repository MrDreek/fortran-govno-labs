module Process
    use Environment
    use IO

    implicit none

contains
    pure subroutine Replace_Text (s, text, rep)
        CHARACTER(*), intent(in) :: s, text, rep
        CHARACTER(LEN(s) + 100, kind = CH_) :: outs     ! provide outs with extra 100 char len
        INTEGER :: i, nt, nr

        outs = s ; nt = LEN_TRIM(text) ; nr = LEN_TRIM(rep)
        DO
            i = INDEX(outs, text(:nt)) ; IF (i == 0) EXIT
            outs = outs(:i - 1) // rep(:nr) // outs(i + nt:)
        END DO
    END subroutine Replace_Text
end module Process
