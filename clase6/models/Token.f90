module TokenModule 
    implicit none

    ! Definimos el tipo Token
    type :: Token
        character(len=100) :: lexema
        character(len=100) :: tipo
        integer :: linea
        integer :: columna
    end type Token

    contains 
    
    subroutine initToken(buffer_, tipo, linea, columna, t)
        implicit none
        character(len=*), intent(in) :: buffer_, tipo
        integer, intent(in) :: linea, columna
        type(Token), intent(inout) :: t

        ! limpiar el token
        t%lexema = ""
        t%tipo = ""
        t%linea = 0
        t%columna = 0

        ! asignar valores al token
        t%lexema = trim(buffer_)
        t%tipo = trim(tipo)
        t%linea = linea
        t%columna = columna
    end subroutine initToken

    subroutine addToken(tokens, buffer_, tipo, linea, columna)
        implicit none 
        type(Token), allocatable, intent(inout) :: tokens(:)
        character(len=*), intent(inout) :: buffer_, tipo
        integer, intent(in) :: linea, columna
        type(Token) :: t
        integer :: n

        ! inicializamos el token
        call initToken(buffer_, tipo, linea, columna, t)

        ! añadir el token al array
        if (allocated(tokens)) then
            n = size(tokens)
            call extendArray(tokens)
        else
            allocate(tokens(1))
            n = 0
        end if

        ! añadir el token al array
        tokens(n+1) = t
    end subroutine addToken

    subroutine extendArray(tokens)
        implicit none
        type(Token), allocatable, intent(inout) :: tokens(:)
        type(Token), allocatable :: temp(:)
        integer :: n

        ! extender el array
        n = size(tokens)
        allocate(temp(n+1))
        temp(1:n) = tokens
        deallocate(tokens)
        tokens = temp
    end subroutine extendArray

end module TokenModule