program upr7
    use Environment
    use Process
    use IO

    implicit none
    character(:), allocatable :: input_file, output_file, data_file
    character(kind = CH_), parameter :: VISITOR = Char(1057, CH_), NOT_VISITOR = Char(1055, CH_)
    character(LEN = 3, kind = CH_) :: YES = Char(1076, CH_) // Char(1072, CH_) // Char(32, CH_)

    integer :: vSize, pSize

    type(person)              :: Persons(PERSON_AMOUNT)
    type(person), allocatable :: vPersons(:), pPersons(:)

    input_file  = "../data/input.txt"
    output_file = "../data/output.txt"
    data_file   = "persons.dat"

    call Create_data_file(input_file, data_file)

    Persons = Read_class_list(data_file)

     call Output_class_list(output_file, Persons, "Исходный список:", "rewind")

     vPersons = Pack(Persons, Persons%isVisitors == VISITOR .and. Persons%isArmies == YES)
     pPersons = Pack(Persons, Persons%isVisitors /= VISITOR .and. Persons%isArmies == YES)

    vSize = Size(vPersons)
    pSize = Size(pPersons)

     call Sort_class_list(vPersons, vSize)
     call Sort_class_list(pPersons, pSize)

     call Output_class_list(output_file, pPersons, "Служившие петербуржцы:", "append")
     call Output_class_list(output_file, vPersons, "Служившие гости:", "append")
end program upr7
