program upr7
    use Environment
    use Process
    use IO

    implicit none
    character(LEN = *), parameter :: input_file = "../data/input.txt", output_file = "../data/output.txt", &
            searchWord = "символ", replaceWord = "666666"
    type(word), pointer :: words => Null()

    words => Read_words(input_file)

    if (Associated(Persons)) then
        call Output_class_list(output_file, words, "Исходный text:", "rewind")
        call Replace_Text(searchWord, Words, replaceWord)

    end if

    call OutputList(output_file, words, "Изменёный список::", "append")
end program upr7
