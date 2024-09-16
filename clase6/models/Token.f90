module TokenModule
    use Utils
    implicit none

    ! Definición del tipo Token
    type :: Token
        character(len=100) :: lexema
        character(len=100) :: tipo
        integer :: linea
        integer :: columna
    end type Token

contains

    ! Subrutina para inicializar un Token
     subroutine initToken(buffer_, tipo, linea, columna, t)
        implicit none
        character(len=*), intent(in) :: buffer_
        character(len=*), intent(in) :: tipo
        integer, intent(in) :: linea
        integer, intent(in) :: columna
        type(Token), intent(inout) :: t

        ! limpia el token
        t%lexema = ''
        t%tipo = ''
        t%linea = 0
        t%columna = 0

        ! inicializa el token asegurándose de que no se concatenen caracteres no deseados
        t%lexema = trim(buffer_)
        t%tipo = trim(tipo)
        t%linea = linea
        t%columna = columna
    end subroutine initToken


    ! Subrutina para añadir un Token a un array de Tokens
    subroutine addToken(tokens, buffer_, tipo, linea, columna)
        implicit none
        type(Token), allocatable, intent(inout) :: tokens(:)
        character(len=100), intent(inout) :: buffer_
        character(len=*), intent(in) :: tipo
        integer, intent(in) :: linea
        integer, intent(in) :: columna
        type(Token) :: t
        integer :: n
    
        call initToken(buffer_, tipo, linea, columna, t)
        
        ! Añadimos el token al array, extendiendo el array de tokens
        if (allocated(tokens)) then
            n = size(tokens)
            call extendArray(tokens)
        else
            allocate(tokens(1))
            n = 0
        end if
        tokens(n+1) = t
        
        ! Limpiamos el buffer_
        call clearBuffer(buffer_)
    end subroutine addToken

    subroutine extendArray(tokens)
        implicit none
        type(Token), allocatable, intent(inout) :: tokens(:)
        type(Token), allocatable :: temp(:)
        integer :: n

        n = size(tokens)
        allocate(temp(n + 1))
        temp(1:n) = tokens
        deallocate(tokens)
        tokens = temp
    end subroutine extendArray

    subroutine pop(tokens)
        implicit none
        type(Token), allocatable, intent(inout) :: tokens(:)
        type(Token), allocatable :: temp(:)
        integer :: n

        n = size(tokens)
        allocate(temp(n - 1))
        temp = tokens(2:n)
        deallocate(tokens)
        tokens = temp
    end subroutine pop

end module TokenModule
