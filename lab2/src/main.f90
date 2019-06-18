program lab2
    use Environment
    use IO
    use Process

    implicit none

    character(LEN = *), parameter :: input_file = "../data/input.txt", output_file = "../data/output.txt"
    type(line), pointer :: lines => Null(), result => Null()
    integer(I_) :: amount = 0

    lines => ReadList(input_file, output_file, amount)
    if (Associated(lines)) then
        call process_lines(lines, result, amount, 'asdn', 'aaaaa')
        call OutputList(output_file, result)
    end if

end program lab2
