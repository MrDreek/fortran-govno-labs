program project_1
    use Environment
    use MathIO
    use MathProcess

    implicit none

    character(LEN=*), parameter      :: input_file = "../data/input.txt", output_file = "output.txt"
    character, allocatable :: infix(:)
    character :: OPERANDS(1:28) = ["A","B","C","D","E","F","G","H","I","J","K","L","M",&
            "N","O","P","Q","R","S","T","U","V","W","X","Y","Z", "t", "f"]
    logical(I_) :: result, bool(28) = .false.
    bool(27) = .true.

    call ReadData(input_file, output_file, infix, bool, OPERANDS)
    call Calculate(infix, OPERANDS, bool, result)
    call WriteData(output_file, result)

end program project_1
