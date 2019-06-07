program upr8_1
    use Environment

    implicit none

    character(:), allocatable :: input_file, output_file, format
    integer, parameter :: PERSON_AMOUNT = 6, NAME_LEN = 15, BIRTH_LEN = 4, IS_IN_ARMY_LEN = 3, IS_VISITOR_LEN = 1, GENDER_LEN = 1
    character(kind = CH_), parameter :: VISITOR = Char(1057, CH_)
    character(LEN = 3, kind = CH_) :: YES = Char(1076, CH_) // Char(1072, CH_) // Char(32, CH_)

    character(NAME_LEN, kind = CH_) :: Names(PERSON_AMOUNT) = "", tmpName = ""
    character(NAME_LEN, kind = CH_), allocatable :: vNames(:), pNames(:)

    character(BIRTH_LEN, kind = CH_) :: Births(PERSON_AMOUNT) = "", tmpBirth = ""
    character(BIRTH_LEN, kind = CH_), allocatable :: vBirths(:), pBirths(:)

    character(kind = CH_) :: Gender(PERSON_AMOUNT) = "", tmpGender = ""
    character(GENDER_LEN, kind = CH_), allocatable :: vGenders(:), pGenders(:)

    character(kind = CH_) :: isVisitors(PERSON_AMOUNT) = ""
    character(IS_IN_ARMY_LEN, kind = CH_) :: isArmies(PERSON_AMOUNT) = ""

    logical, allocatable :: isVisitor(:), isLocal(:)
    integer :: visitorAmount = 0, localAmount = 0

    integer :: In, Out, IO, i, j
    integer, parameter :: INDEXES(*) = [(i, i = 1, PERSON_AMOUNT)]
    integer, allocatable :: pPos(:), vPos(:)

    input_file = "../data/input.txt"
    output_file = "../data/output.txt"
    open (file = input_file, encoding = E_, newunit = In)
    format = '(a15, 1x, a4, 1x, a3, 1x, a1, 1x, a1)'
    read (In, format, iostat = IO) (Names(i), Births(i), isArmies(i), isVisitors(i), Gender(i), i = 1, PERSON_AMOUNT)
    close (In)

    ! Вывод списка.
    open (file = output_file, encoding = E_, newunit = Out)
    write (out, '(a)') "Исходный список:"
    write (Out, format, iostat = IO)  (Names(i), Births(i), isArmies(i), isVisitors(i), Gender(i), i = 1, PERSON_AMOUNT)
    close (Out)

    isVisitor = isVisitors == VISITOR .and. YES == isArmies
    visitorAmount = Count(isVisitor)

    vPos = Pack(INDEXES, isVisitor)
    allocate (vNames(visitorAmount), vBirths(visitorAmount), vGenders(visitorAmount))
    do concurrent (i = 1:visitorAmount)
        vNames(i) = Names(vPos(i))
        vGenders(i) = Gender(vPos(i))
        vBirths(i) = Births(vPos(i))
    end do

    do i = visitorAmount - 1, 1, -1
        do j = 1, i
            if (vNames(j) > vNames(j + 1)) then
                tmpName = vNames(j)
                vNames(j) = vNames(j + 1)
                vNames(j + 1) = tmpName
                tmpGender = vGenders(j)
                vGenders(j) = vGenders(j + 1)
                vGenders(j + 1) = tmpGender
                tmpBirth = vBirths(j)
                vBirths(j) = vBirths(j + 1)
                vBirths(j + 1) = tmpBirth
            end if
        end do
    end do

    isLocal = isVisitors /= VISITOR .and. YES == isArmies
    localAmount = Count(isLocal)

    pPos = Pack(INDEXES, isLocal)
    allocate (pNames(localAmount), pBirths(localAmount), pGenders(localAmount))
    do concurrent (i = 1:localAmount)
        pNames(i) = Names(pPos(i))
        pGenders(i) = Gender(pPos(i))
        pBirths(i) = Births(pPos(i))
    end do

    do i = localAmount - 1, 1, -1
        do j = 1, i
            if (pNames(j) > pNames(j + 1)) then
                tmpName = pNames(j)
                pNames(j) = pNames(j + 1)
                pNames(j + 1) = tmpName
                tmpGender = pGenders(j)
                pGenders(j) = pGenders(j + 1)
                pGenders(j + 1) = tmpGender
                tmpBirth = pBirths(j)
                pBirths(j) = pBirths(j + 1)
                pBirths(j + 1) = tmpBirth
            end if
        end do
    end do

    format = '(a15, 1x, a4, 1x, a1)'
    open (file = output_file, encoding = E_, position = 'append', newunit = Out)
    write (out, '(/a)') "Служившие петербуржцы:"
    write (Out, format, iostat = IO) (pNames(i), pBirths(i), pGenders(i), i = 1, localAmount)
    close (Out)

    open (file = output_file, encoding = E_, position = 'append', newunit = Out)
    write (out, '(/a)') "Служившие гости города:"
    write (Out, format, iostat = IO) (vNames(i), vBirths(i), vGenders(i), i = 1, visitorAmount)
    close (Out)

end program upr8_1
