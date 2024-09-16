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
        integer, intent(inout) :: linea
        integer, intent(inout) :: columna
        integer, intent(inout) :: estado
        logical, intent(in) :: salto_linea

        ! añadimos el caracter al buffer_
        call addtoBuffer(current_char, buffer_, columna)

        ! luego verificamos si es un caracter especial
        if (current_char == '=') then 
            call addToken(tokens, buffer_, 'IGUAL', linea, columna)
        else if (current_char == '(') then
            call addToken(tokens, buffer_, 'PARENTESIS_IZQUIERDO', linea, columna)
        else if (current_char == ')') then
            call addToken(tokens, buffer_, 'PARENTESIS_DERECHO', linea, columna)
        else if (current_char == '{') then
            call addToken(tokens, buffer_, 'LLAVE_IZQUIERDA', linea, columna)
        else if (current_char == '}') then
            call addToken(tokens, buffer_, 'LLAVE_DERECHA', linea, columna)
        else if (current_char == ';') then
            call addToken(tokens, buffer_, 'PUNTO_Y_COMA', linea, columna)
        else if (current_char == ',') then
            call addToken(tokens, buffer_, 'COMA', linea, columna)
        else if (current_char == ':') then
            call addToken(tokens, buffer_, 'DOS_PUNTOS', linea, columna)
        else if (isSpecialChar(current_char)) then
            call clearBuffer(buffer_)
        ! digito
        else if (current_char >= '0' .and. current_char <= '9') then
            call iraState(estado, 8)
        ! alfabetico
        else if ((current_char >= 'a' .and. current_char <= 'z') .or. (current_char >= 'A' .and. current_char <= 'Z')) then
            call iraState(estado, 1)
        else if (current_char == '-') then
            call iraState(estado, 3)
        else if (current_char == '"') then
            call iraState(estado, 2)
        else if (current_char == '/') then
            call iraState(estado, 5)
        else 
            print *, "Caracter no reconocido: ", current_char
            call addError(errors, "Caracter no reconocido: " // current_char, buffer_, "LEXICO", linea, columna)
        end if

        ! verificamos si es un salto de línea
        if (salto_linea) then
            call newLine(linea, columna)
            call clearBuffer(buffer_)
        end if
    end subroutine state0

    ! estado 1
    subroutine state1(current_char, buffer_, tokens, linea, columna, estado, i)
        implicit none
        character(len=1), intent(in) :: current_char
        character(len=100), intent(inout) :: buffer_
        type(Token), allocatable, intent(inout) :: tokens(:)
        integer, intent(inout) :: linea
        integer, intent(inout) :: columna
        integer, intent(inout) :: estado
        integer, intent(inout) :: i
        character(len=100) :: tokenType

        ! inicializamos el tokenType
        tokenType = ''

        ! añadimos el caracter al buffer_
        call addtoBuffer(current_char, buffer_, columna)

        ! verificamos si es un caracter alfabético
        if ((current_char >= 'a' .and. current_char <= 'z') .or. (current_char >= 'A' .and. current_char <= 'Z')) then
            call iraState(estado, 1)
        else
            ! solo retrocedemos si es un caracter imprimeble
            if (isSpecialChar(current_char) .eqv. .false.) then
                call goBack(i, columna, buffer_)
            end if
            ! verificamos si es una palabra reservada
            tokenType = isReservedWord(buffer_)
            call addToken(tokens, buffer_, tokenType, linea, columna)
            call iraState(estado, 0)
        end if
    end subroutine state1

    ! estado 2
    subroutine state2(current_char, buffer_, tokens, linea, columna, estado)
        implicit none
        character(len=1), intent(in) :: current_char
        character(len=100), intent(inout) :: buffer_
        type(Token), allocatable, intent(inout) :: tokens(:)
        integer, intent(inout) :: linea
        integer, intent(inout) :: columna
        integer, intent(inout) :: estado

        ! añadimos el caracter al buffer_
        call addtoBuffer(current_char, buffer_, columna)

        ! verificamos si es una comilla
        if (current_char == '"') then
            call addToken(tokens, buffer_, 'CADENA', linea, columna)
            call iraState(estado, 0)
        else
            call iraState(estado, 2)
        end if
    end subroutine state2

    ! estado 3
    subroutine state3(current_char, buffer_, tokens, errors, linea, columna, estado)
        implicit none
        character(len=1), intent(in) :: current_char
        character(len=100), intent(inout) :: buffer_
        type(Token), allocatable, intent(inout) :: tokens(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        integer, intent(inout) :: linea
        integer, intent(inout) :: columna
        integer, intent(inout) :: estado

        ! añadimos el caracter al buffer_
        call addtoBuffer(current_char, buffer_, columna)

        ! verificamos si es un guión para empezar un comentario
        if (current_char == '-') then
            call iraState(estado, 4)
        else
            print *, "Se esperaba el segundo guion para comentario pero se obtuvo ", current_char
            call addError(errors, "Se esperaba el segundo guión para comentario pero se obtuvo " & 
            // current_char, buffer_, "LEXICO", linea, columna)
            call iraState(estado, 0)
            ! call clearBuffer(buffer_)
        end if
    end subroutine state3

    ! estado 4
    subroutine state4(current_char, buffer_, tokens, linea, columna, estado, salto_linea)
        implicit none
        character(len=1), intent(in) :: current_char
        character(len=100), intent(inout) :: buffer_
        type(Token), allocatable, intent(inout) :: tokens(:)
        integer, intent(inout) :: linea
        integer, intent(inout) :: columna
        integer, intent(inout) :: estado
        logical, intent(in) :: salto_linea

        ! añadimos el caracter al buffer_
        call addtoBuffer(current_char, buffer_, columna)

        ! verificamos si es un salto de línea
        if (salto_linea) then
            ! los comentarios no se añaden a los tokens, solo se ignoran
            call newLine(linea, columna)
            call clearBuffer(buffer_)
            call iraState(estado, 0)
        else
            call iraState(estado, 4)
        end if
    end subroutine state4

    ! estado 5
    subroutine state5(current_char, buffer_, tokens, errors, linea, columna, estado)
        implicit none
        character(len=1), intent(in) :: current_char
        character(len=100), intent(inout) :: buffer_
        type(Token), allocatable, intent(inout) :: tokens(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        integer, intent(inout) :: linea
        integer, intent(inout) :: columna
        integer, intent(inout) :: estado

        ! añadimos el caracter al buffer_
        call addtoBuffer(current_char, buffer_, columna)

        ! verificamos si es un asterisco para empezar un comentario
        if (current_char == '*') then
            call iraState(estado, 6)
        else
            print *, "Se esperaba un asterisco para comentario multilinea pero se obtuvo ", current_char
            call addError(errors, "Se esperaba un asterisco para comentario multilinea pero se obtuvo " &
            // current_char, buffer_, "LEXICO", linea, columna)
            call iraState(estado, 0)
        end if
    end subroutine state5

    ! estado 6
    subroutine state6(current_char, buffer_, tokens, linea, columna, estado, salto_linea)
        implicit none
        character(len=1), intent(in) :: current_char
        character(len=100), intent(inout) :: buffer_
        type(Token), allocatable, intent(inout) :: tokens(:)
        integer, intent(inout) :: linea
        integer, intent(inout) :: columna
        integer, intent(inout) :: estado
        logical, intent(in) :: salto_linea

        ! añadimos el caracter al buffer_
        call addtoBuffer(current_char, buffer_, columna)

        ! verificamos si es un asterisco para terminar el comentario
        if (current_char == '*') then
            call iraState(estado, 7)
        else
            if (salto_linea) call newLine(linea, columna)
            call iraState(estado, 6)
        end if
    end subroutine state6

    ! estado 7
    subroutine state7(current_char, buffer_, tokens, linea, columna, estado, salto_linea)
        implicit none
        character(len=1), intent(in) :: current_char
        character(len=100), intent(inout) :: buffer_
        type(Token), allocatable, intent(inout) :: tokens(:)
        integer, intent(inout) :: linea
        integer, intent(inout) :: columna
        integer, intent(inout) :: estado
        logical, intent(in) :: salto_linea

        ! añadimos el caracter al buffer_
        call addtoBuffer(current_char, buffer_, columna)

        ! verificamos si es un slash para terminar el comentario
        if (current_char == '/') then
            call clearBuffer(buffer_)
            call iraState(estado, 0)
        else
            if (salto_linea) call newLine(linea, columna)
            call iraState(estado, 6)
        end if
    end subroutine state7

    ! estado 8
    subroutine state8(current_char, buffer_, tokens, linea, columna, estado, i)
        implicit none
        character(len=1), intent(in) :: current_char
        character(len=100), intent(inout) :: buffer_
        type(Token), allocatable, intent(inout) :: tokens(:)
        integer, intent(inout) :: linea
        integer, intent(inout) :: columna
        integer, intent(inout) :: estado
        integer, intent(inout) :: i

        ! añadimos el caracter al buffer_
        call addtoBuffer(current_char, buffer_, columna)

        ! verificamos si es un digito
        if (current_char >= '0' .and. current_char <= '9') then
            call iraState(estado, 8)
        else
            ! solo retrocedemos si es un caracter imprimeble
            if (isSpecialChar(current_char) .eqv. .false.) then
                call goBack(i, columna, buffer_)
            end if
            call addToken(tokens, buffer_, 'NUMERO', linea, columna)
            call iraState(estado, 0)
        end if
    end subroutine state8
end module states
