module ErrorModule
    use Utils
    implicit none

    ! Definición del tipo Error
    type :: Error
        character(len=100) :: mensaje
        character(len=100) :: tipo
        integer :: linea
        integer :: columna
    end type Error

contains

    ! Subrutina para inicializar un Error
    subroutine initError(mensaje, tipo, linea, columna, e)
        implicit none
        character(len=*), intent(in) :: mensaje
        character(len=*), intent(in) :: tipo
        integer, intent(in) :: linea
        integer, intent(in) :: columna
        type(Error), intent(out) :: e

        ! limpia el error
        e%mensaje = ''
        e%tipo = ''
        e%linea = 0
        e%columna = 0
        
        e%mensaje = trim(mensaje)
        e%tipo = trim(tipo)
        e%linea = linea
        e%columna = columna
    end subroutine initError

    ! Subrutina para añadir un Error a un array de Errors
    subroutine addError(errors, mensaje, buffer_, tipo, linea, columna)
        implicit none
        type(Error), allocatable, intent(inout) :: errors(:)
        ! mensaje de longitud variable
        character(len=*), intent(in) :: mensaje
        character(len=*), intent(inout) :: buffer_
        character(len=*), intent(in) :: tipo
        integer, intent(in) :: linea
        integer, intent(in) :: columna
        type(Error) :: e
        integer :: n
        
        call initError(mensaje, tipo, linea, columna, e)
        
        ! Añadimos el error al array, extendiendo el array de errores
        if (allocated(errors)) then
            n = size(errors)
            call extendArray(errors)
        else
            allocate(errors(1))
            n = 0
        end if
        errors(n+1) = e

        ! Limpiamos el buffer_
        call clearBuffer(buffer_)
    end subroutine addError

    ! Subrutina para extender un array de errores
    subroutine extendArray(errors)
        implicit none
        type(Error), allocatable, intent(inout) :: errors(:)
        type(Error), allocatable :: temp(:)
        integer :: n, i
        
        n = size(errors)
        allocate(temp(n + 1))
        temp(1:n) = errors
        deallocate(errors)
        errors = temp
    end subroutine extendArray

end module ErrorModule
