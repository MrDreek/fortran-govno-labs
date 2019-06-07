program upr7
    use Environment
    use Process
    use IO

    implicit none
    character(LEN = *), parameter :: input_file = "../data/input.txt", output_file = "../data/output.txt"
    type(record), pointer :: records(:)

    call ReadList(input_file, output_file, records)
    call removeItems(records)
    call OutputList(output_file, records)
end program upr7
