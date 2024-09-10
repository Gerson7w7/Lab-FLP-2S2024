module states
    use TokenModule
    use ErrorModule
    use Utils
    implicit none

contains

    ! estado 0
    subroutine state0(current_char, buffer_, tokens, errors, linea, columna, estado, salto_linea)
        implicit none 
        character(len=1), intent(in) :: current_char
        character(len=100), intent(inout) :: buffer_
        type(Token), allocatable, intent(inout) :: tokens(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        integer, intent(inout) :: linea, columna
        integer, intent(inout) :: estado
        logical, intent(in) :: salto_linea

        ! añadir el caracter al buffer_ 
        call addtoBuffer(current_char, buffer_, columna)

        ! verificar si es un caracter del estado 0 
        if (current_char == '=') then 
            call addToken(tokens, buffer_, "IGUAL", linea, columna)
        else if (current_char == '(') then 
            call addToken(tokens, buffer_, "PARENTESIS_IZQUIERDO", linea, columna)
        else if (current_char == ')') then 
            call addToken(tokens, buffer_, "PARENTESIS_DERECHO", linea, columna)
        else if (current_char == '{') then 
            call addToken(tokens, buffer_, "LLAVE_IZQUIERDO", linea, columna)
        else if (current_char == '}') then 
            call addToken(tokens, buffer_, "LLAVE_DERECHO", linea, columna)
        else if (current_char == ';') then 
            call addToken(tokens, buffer_, "PUNTO_Y_COMA", linea, columna)
        else if (current_char == ',') then 
            call addToken(tokens, buffer_, "COMA", linea, columna)
        else if (current_char == ':') then 
            call addToken(tokens, buffer_, "DOS_PUNTOS", linea, columna)
        else if (isSpecialChar(current_char)) then 
            call clearBuffer(buffer_)
        else if (current_char >= '0' .and. current_char <= '9') then 
            ! si viene un numero
            call iraState(estado, 8)
        else if ((current_char >= 'a' .and. current_char <= 'z') .or. &
                 (current_char >= 'A' .and. current_char <= 'Z')) then 
            ! si viene una letra
            call iraState(estado, 1)
        else if (current_char == '-') then 
            call iraState(estado, 3)
        else if (current_char == '"') then
            call iraState(estado, 2)
        else if (current_char == '/') then
            call iraState(estado, 5)
        else 
            print *, "Error: caracter no reconocido: ", current_char
            call addError(errors, "Caracter no reconocido: " // current_char, buffer_, "LEXICO", linea, columna)
        end if

        ! si es un salto de linea
        if (salto_linea) then 
            call newLine(linea, columna)
            call clearBuffer(buffer_)
        end if
    end subroutine state0 

    ! estado 1
    subroutine state1(current_char, buffer_, tokens, errors, linea, columna, estado, i)
        implicit none 
        character(len=1), intent(in) :: current_char
        character(len=100), intent(inout) :: buffer_
        type(Token), allocatable, intent(inout) :: tokens(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        integer, intent(inout) :: linea, columna
        integer, intent(inout) :: estado
        integer, intent(inout) :: i
        character(len=100) :: tokenType

        ! inicializar el tokenType 
        tokenType = ""

        ! añadir el caracter al buffer_
        call addtoBuffer(current_char, buffer_, columna)

        ! verficar si es un caracter alfanumerico
        if ((current_char >= 'a' .and. current_char <= 'z') .or. &
            (current_char >= 'A' .and. current_char <= 'Z')) then 
            call iraState(estado, 1)
        else 
            ! retrocedemos un caracter 
            if(isSpecialChar(current_char) .eqv. .false.) then 
                call goBack(i, columna, buffer_)
            end if

            ! verificamos si es una palabra reservada
            tokenType = isReservedWord(buffer_)
            call addToken(tokens, buffer_, tokenType, linea, columna)
            call iraState(estado, 0)
        end if
    end subroutine state1

end module states