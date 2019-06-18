program upr7
    use Environment
    use Process
    use IO

    implicit none
    character(LEN = *), parameter :: input_file = "../data/input.txt", output_file = "../data/output.txt"
    type(record), pointer :: records(:) => Null()

    call ReadList(input_file, output_file, records)
    write(*, *)
    call removeItems(records)
    call OutputList(output_file, records)
    deallocate(records)
end program upr7
