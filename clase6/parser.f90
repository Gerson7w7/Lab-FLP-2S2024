module ParserModule
    use TokenModule
    use ErrorModule
    use ObjetoModule
    use Utils
    use parserUtils
    implicit none
    contains 

    subroutine parser(tokens, errors, instancias)
        implicit none
        type(Token), allocatable, intent(inout) :: tokens(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        type(Objeto), allocatable, intent(inout) :: instancias(:)
        type(Token), allocatable :: pila(:)

        ! Asignar a la pila los tokens
        pila = tokens
        ! Añaadir un token de fin de archivo
        call addToken(pila, 'EOF', 'EOF', 0, 0)

        ! Llamar a la función de análisis sintáctico
        call inicio(pila, errors, instancias)
    end subroutine parser

    ! INICIO : INSTRUCCION INSTRUCCIONES
    subroutine inicio(pila, errors, instancias)
        implicit none
        type(Token), allocatable, intent(inout) :: pila(:)
        type(Error), allocatable, intent(inout) :: errors(:)

        call instruccion(pila, errors)
        call instrucciones(pila, errors)
    end subroutine inicio

    ! INSTRUCCIONES : punto_coma EOF INSTRUCCION INSTRUCCIONES
    !               | ε
    subroutine instrucciones(pila, errors, instancias)
        implicit none
        type(Token), allocatable, intent(inout) :: pila(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        type(Token) :: actualToken

        if (esIgual('PUNTO_COMA', pila, errors, .false., actualToken)) then
            if (esIgual('EOF', pila, errors, .true., actualToken)) then
                print *, 'Analisis sintactico exitoso'
            else 
                call instruccion(pila, errors)
                call instrucciones(pila, errors)
            end if
        else 
            ! sino ε
        end if
    end subroutine instrucciones

    ! INSTRUCCION : PALABRA_RESERVADA VARIABLE igual nueva FUNCION
    subroutine instruccion(pila, errors, instancias)
        implicit none
        type(Token), allocatable, intent(inout) :: pila(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        type(Objeto), allocatable, intent(inout) :: instancias(:)
        type(Token) :: actualToken

        ! añadimos un objeto a la lista de instancias
        call addObjeto(instancias)

        call palabra_reservada(pila, errors, instancias)
        call variable(pila, errors, instancias)
        if (esIgual('IGUAL', pila, errors, .true., actualToken)) then
            if (esIgual('NUEVA', pila, errors, .true., actualToken)) then
                call funcion(pila, errors, instancias)
            end if
        end if
    end subroutine instruccion

    ! PALABRA_RESERVADA : CrearBD 
    !                   | CrearColeccion 
    !                   | InsertarUnico 
    !                   | EliminarBD
    subroutine palabra_reservada(pila, errors, instancias)
        implicit none
        type(Token), allocatable, intent(inout) :: pila(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        type(Objeto), allocatable, intent(inout) :: instancias(:)
        type(Token) :: actualToken

        if (esIgual('CREARBD', pila, errors, .false., actualToken)) then
            ! añadir la palabra reservada a la lista de instancias
            call addPalabraReservada(pila, instancias, 'CrearBD', errors)
        else if (esIgual('CREARCOLECCION', pila, errors, .false., actualToken)) then
            call addPalabraReservada(instancias, 'CrearColeccion', errors)
        else if (esIgual('INSERTARUNICO', pila, errors, .false., actualToken)) then
            call addPalabraReservada(instancias, 'InsertarUnico', errors)
        else if (esIgual('ELIMINARBD', pila, errors, .false., actualToken)) then
            call addPalabraReservada(instancias, 'EliminarBD', errors)
        else
            call addError(errors, 'Se esperaba una palabra reservada', pila(1)%linea, pila(1)%columna)
        end if
    end subroutine palabra_reservada

    ! VARIABLE : identificador
    subroutine variable(pila, errors, instancias)
        implicit none
        type(Token), allocatable, intent(inout) :: pila(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        type(Objeto), allocatable, intent(inout) :: instancias(:)
        type(Token) :: actualToken

        if (esIgual('IDENTIFICADOR', pila, errors, .true., actualToken)) then
            ! añadir la variable a la lista de instancias
            call addVariable(instancias, pila(1)%lexema)
        end if
    end subroutine variable

    ! FUNCION : PALABRA_RESERVADA parentesis_izq DEFINICION_FUNCION 
    subroutine funcion(pila, errors, instancias)
        implicit none
        type(Token), allocatable, intent(inout) :: pila(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        type(Objeto), allocatable, intent(inout) :: instancias(:)
        type(Token) :: actualToken

        call palabra_reservada(pila, errors, instancias)
        if (esIgual('PARENTESIS_IZQ', pila, errors, .true., actualToken)) then
            call definicion_funcion(pila, errors, instancias)
        end if
    end subroutine funcion

    ! DEFINICION_FUNCION : TIPO_PARAMETRO PARAMETROS parentesis_der
    !                    | parentesis_der
    subroutine definicion_funcion(pila, errors, instancias)
        implicit none
        type(Token), allocatable, intent(inout) :: pila(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        type(Objeto), allocatable, intent(inout) :: instancias(:)
        type(Token) :: actualToken

        if (esIgual('PARENTESIS_DER', pila, errors, .true., actualToken)) then
            ! todo bien
        else
            call tipo_parametro(pila, errors, instancias)
            call parametros(pila, errors, instancias)
            if (esIgual('PARENTESIS_DER', pila, errors, .true., actualToken)) then
                ! todo bien
            end if
        end if
    end subroutine definicion_funcion

    ! PARAMETROS : coma TIPO_PARAMETRO PARAMETROS
    !           | ε
    subroutine parametros(pila, errors, instancias)
        implicit none
        type(Token), allocatable, intent(inout) :: pila(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        type(Objeto), allocatable, intent(inout) :: instancias(:)
        type(Token) :: actualToken

        if (esIgual('COMA', pila, errors, .false., actualToken)) then
            call tipo_parametro(pila, errors, instancias)
            call parametros(pila, errors, instancias)
        else
            ! sino ε
        end if
    end subroutine parametros

    ! TIPO_PARAMETRO : TIPO_DATO
    !                | llave_izq cadena dos_puntos TIPO_DATO LLAVE_VALOR llave_der
    subroutine tipo_parametro(pila, errors, instancias)
        implicit none
        type(Token), allocatable, intent(inout) :: pila(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        type(Objeto), allocatable, intent(inout) :: instancias(:)
        type(Token) :: actualToken

        if (esIgual('LLAVE_IZQ', pila, errors, .true., actualToken)) then
            if (esIgual('CADENA', pila, errors, .true., actualToken)) then
                call setClaveJson()
                if (esIgual('DOS_PUNTOS', pila, errors, .true., actualToken)) then
                    call tipo_dato(pila, errors, instancias)
                    call llave_valor(pila, errors, instancias)
                    if (esIgual('LLAVE_DER', pila, errors, .true., actualToken)) then
                        ! todo bien
                    end if
                end if
            end if
        else 
            call tipo_dato(pila, errors, instancias)
        end if
    end subroutine tipo_parametro

    ! LLAVE_VALOR : coma cadena dos_puntos TIPO_DATO LLAVE_VALOR
    !             | ε
    subroutine llave_valor(pila, errors, instancias)
        implicit none
        type(Token), allocatable, intent(inout) :: pila(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        type(Objeto), allocatable, intent(inout) :: instancias(:)
        type(Token) :: actualToken

        if (esIgual('COMA', pila, errors, .false., actualToken)) then
            if (esIgual('CADENA', pila, errors, .true., actualToken)) then
                if (esIgual('DOS_PUNTOS', pila, errors, .true., actualToken)) then
                    call tipo_dato(pila, errors, instancias)
                    call llave_valor(pila, errors, instancias)
                end if
            end if
        else
            ! sino ε
        end if
    end subroutine llave_valor

    ! TIPO_DATO : cadena
    !           | numero
    !           | identificador
    subroutine tipo_dato(pila, errors, instancias)
        implicit none
        type(Token), allocatable, intent(inout) :: pila(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        type(Objeto), allocatable, intent(inout) :: instancias(:)
        type(Token) :: actualToken

        if (esIgual('CADENA', pila, errors, .true., actualToken) &&
            .OR. esIgual('NUMERO', pila, errors, .true., actualToken) &&
            .OR. esIgual('IDENTIFICADOR', pila, errors, .true., actualToken)) then
            ! todo bien
        else
            call addError(errors, 'Se esperaba un tipo de dato', pila(1)%linea, pila(1)%columna)
        end if
    end subroutine tipo_dato

end module ParserModule