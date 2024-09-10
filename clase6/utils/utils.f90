module Utils 
contains 

    ! Añade un caracter al buffer_
    subroutine addtoBuffer(current_char, buffer_, column)
        implicit none
        character(len=1), intent(in) :: current_char
        character(len=100), intent(inout) :: buffer_
        integer, intent(inout) :: column

        ! añadir el caracter al buffer_
        buffer_ = trim(buffer_) // current_char
        column = column + 1
    end subroutine addtoBuffer

    ! Verifica si un caracter es especial
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

    ! Limpia el buffer_
    subroutine clearBuffer(buffer_)
        implicit none
        character(len=100), intent(inout) :: buffer_

        ! limpiar el buffer_
        buffer_ = ""
    end subroutine clearBuffer

    ! Cambia el estado
    subroutine iraState(estado, next_state)
        implicit none
        integer, intent(inout) :: estado
        integer, intent(in) :: next_state

        ! cambiar el estado
        estado = next_state
    end subroutine iraState

    ! Salto de linea 
    subroutine newLine(linea, columna)
        implicit none
        integer, intent(inout) :: linea, columna

        ! cambiar la linea y la columna
        linea = linea + 1
        columna = 1
    end subroutine newLine

    ! retroceder un caracter 
    subroutine goBack(i, columna, buffer_)
        implicit none
        integer, intent(inout) :: i, columna
        character(len=100), intent(inout) :: buffer_
        integer :: actual_length

        i = i - 1
        columna = columna - 1

        ! obtener la longitud actual del buffer_
        actual_length = len_trim(buffer_)
        if (actual_length > 0) then 
            buffer_ = buffer_(:actual_length-1)
        end if
    end subroutine goBack

    ! Verifica si una palabra es reservada
    function isReservedWord(buffer_) result(reserved)
        implicit none
        character(len=*), intent(in) :: buffer_
        character(len=10) :: reserved

        ! verificar si es una palabra reservada
        if (buffer_ == "CrearDB" .or. buffer_ == "nueva" .or. &
            buffer_ == "CrearColeccion" .or. buffer_ == "InsertarUnico" .or. &
            buffer_ == "EliminarDB") then 
            reserved = "RESERVADA"
        else
            reserved = "IDENTIFICADOR"
        end if
    end function isReservedWord

end module Utils