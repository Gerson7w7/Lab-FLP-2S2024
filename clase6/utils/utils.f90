module Utils 
contains
    ! read file
    subroutine readFile(file, lines)
    implicit none
        character(len=100), intent(in) :: file
        character(len=100), intent(out) :: lines(:)
        integer :: ios
        integer :: i
        character(len=100) :: line
        open(unit=10, file=file, status='old')
        do
            read(10, '(A)', iostat=ios) line
            if (ios /= 0) exit
            lines = [lines, line]
        end do
        close(10)
    end subroutine readFile

    ! añade un caracter al buffer
    subroutine addtoBuffer(current_char, buffer_, column)
    implicit none
        character(len=1), intent(in) :: current_char
        character(len=100), intent(inout) :: buffer_
        integer, intent(inout) :: column
        buffer_ = trim(buffer_) // current_char
        column = column + 1
    end subroutine addtoBuffer

    ! validar palabra reservada
    function isReservedWord(buffer) result(reserved)
    implicit none
        character(len=*), intent(in) :: buffer
        character(len=10) :: reserved

        if (buffer == 'CrearBD' .or. buffer == 'nueva' .or. buffer == 'CrearColeccion' .or. &
            buffer == 'InsertarUnico' .or. buffer == 'EliminarBD') then
            reserved = 'RESERVADA'
        else
            reserved = 'IDENTIFICADOR'
        end if
    end function isReservedWord

    ! salto de linea
    subroutine newLine(linea, columna)
    implicit none
        integer, intent(inout) :: linea
        integer, intent(inout) :: columna
        linea = linea + 1
        columna = 1
    end subroutine newLine

    ! limpiar buffer
    subroutine clearBuffer(buffer)
    implicit none
        character(len=100), intent(inout) :: buffer
        buffer = ''
    end subroutine clearBuffer

    ! ir a un estado en específico
    subroutine iraState(estado, next_state)
    implicit none
        integer, intent(inout) :: estado
        integer, intent(in) :: next_state
        estado = next_state       
    end subroutine iraState

    ! retroceder una posición
    subroutine goBack(i, columna, buffer_)
        implicit none
        integer, intent(inout) :: i
        integer, intent(inout) :: columna
        character(len=100), intent(inout) :: buffer_
        integer :: actual_length

        i = i - 1
        columna = columna - 1

        ! Obtén la longitud real del contenido de buffer_ sin los espacios en blanco finales
        actual_length = len_trim(buffer_)
        if (actual_length > 0) then
            buffer_ = buffer_(:actual_length-1)
        endif
    end subroutine goBack

    ! saber si un caracter es especial
    function isSpecialChar(current_char) result(special)
        implicit none
        character(len=1), intent(in) :: current_char
        logical :: special

        if (current_char == ' ' .or. current_char == '\t' .or. &
            current_char == '\r' .or. current_char == '\f' .or. &
            current_char == '\0') then
            special = .true.
        else
            special = .false.
        end if
    end function isSpecialChar

end module Utils
