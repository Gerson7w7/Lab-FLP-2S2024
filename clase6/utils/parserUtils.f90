module parserUtils 
    implicit none
    ! Importar módulos
    use TokenModule
    use ErrorModule
    contains

    function esIgual(tokenType, pila, errors, imprimirError)
        implicit none
        character(len=*), intent(in) :: tokenType
        type(Token), allocatable, intent(inout) :: pila(:)
        type(Error), allocatable, intent(inout) :: errors(:)
        logical, intent(in) :: imprimirError
        character(len=*) :: mensaje
        logical :: esIgual

        esIgual = .false. ! Por defecto no son iguales
        if (pila(1)%tipo == tokenType) then
            call pop(pila)
            esIgual = .true.
        else if (imprimirError) then
            mensaje = 'Error sintáctico: Se esperaba ' // trim(tokenType) // ' en la posicion ' // trim(pila(1)%linea) // ':' // trim(pila(1)%columna) // ' pero se encontró ' // trim(pila(1)%tipo)
            call addError(errors, mensaje, pila(1)%linea, pila(1)%columna)
        end if
    end function esIgual

    subroutine addPalabraReservada(pila, instancias, palabraReservada, errors)
        implicit none
        type(Token), allocatable, intent(inout) :: pila(:)
        type(Objeto), allocatable, intent(inout) :: instancias(:)
        character(len=*), intent(in) :: palabraReservada
        type(Error), allocatable, intent(inout) :: errors(:)
        integer :: n 
        character(len=*) :: mensaje

        ! size of the array
        n = size(instancias)

        if (instancias(n)%nombre == '')
            instancias(n)%nombre = palabraReservada
        else if (instancias(n)%nombre /= '')
            mensaje = 'Error semántico: Se esperaba un nombre de variable en la posición ' // trim(pila(1)%linea) // ':' // trim(pila(1)%columna) // ' pero se encontró ' // trim(palabraReservada)
            add error(errors, mensaje, mensaje, 'SEMANTICO', pila(1)%linea, pila(1)%columna)
        end if
    end subroutine addPalabraReservada

    subroutine addVariable(instancias, lexema)
        implicit none
        type(Objeto), allocatable, intent(inout) :: instancias(:)
        character(len=*), intent(in) :: lexema
        integer :: n

        ! size of the array
        n = size(instancias)

        if (instancias(n)%nombre == '')
            instancias(n)%nombre = lexema
        else if (instancias(n)%nombre /= '')
            mensaje = 'Error semántico: Se esperaba un nombre de variable en la posición ' // trim(pila(1)%linea) // ':' // trim(pila(1)%columna) // ' pero se encontró ' // trim(lexema)
            add error(errors, mensaje, mensaje, 'SEMANTICO', pila(1)%linea, pila(1)%columna)
        end if
    end subroutine addVariable
        

end module parserUtils