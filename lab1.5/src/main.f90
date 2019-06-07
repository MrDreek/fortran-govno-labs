program upr7
    use Environment
    use Process
    use IO

    implicit none
    character(:), allocatable :: input_file, output_file
    character(kind = CH_), parameter :: VISITOR = Char(1057, CH_), NOT_VISITOR = Char(1055, CH_)
    character(LEN = 3, kind = CH_) :: YES = Char(1076, CH_) // Char(1072, CH_) // Char(32, CH_)

    type(person), pointer :: Persons => Null(), vPersons => Null(), pPersons => Null()
    integer(I_) :: vPersonsAmount = 0, pPersonsAmount = 0

    input_file = "../data/input.txt"
    output_file = "../data/output.txt"

    Persons => Read_class_list(input_file)

    if (Associated(Persons)) then
        call Output_class_list(output_file, Persons, "Исходный список:", "rewind")

        call Get_list_by_filter(Persons, vPersons, vPersonsAmount, VISITOR, YES)
        call Get_list_by_filter(Persons, pPersons, pPersonsAmount, NOT_VISITOR, YES)

        call Sort_class_list(vPersons, vPersonsAmount)
        call Sort_class_list(pPersons, pPersonsAmount)

        if (Associated(pPersons)) &
                call Output_class_list(output_file, pPersons, "Служившие петербуржцы:", "append")
        if (Associated(vPersons)) &
                call Output_class_list(output_file, vPersons, "Служившие гости:", "append")

    end if
end program upr7
