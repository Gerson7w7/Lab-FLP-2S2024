module ErrorModule 
    implicit none

    ! Definimos el tipo Error
    type :: Error
        character(len=100) :: mensaje
        character(len=100) :: tipo
        integer :: linea
        integer :: columna
    end type Error

    contains 
    
    subroutine initError(mensaje, tipo, linea, columna, e)
        implicit none
        character(len=*), intent(in) :: mensaje, tipo
        integer, intent(in) :: linea, columna
        type(Error), intent(inout) :: e

        ! limpiar el error
        e%mensaje = ""
        e%tipo = ""
        e%linea = 0
        e%columna = 0

        ! asignar valores al error
        e%mensaje = trim(mensaje)
        e%tipo = trim(tipo)
        e%linea = linea
        e%columna = columna
    end subroutine initError

    subroutine addError(errors, mensaje, buffer_, tipo, linea, columna)
        implicit none 
        type(Error), allocatable, intent(inout) :: errors(:)
        character(len=*), intent(inout) :: buffer_, tipo, mensaje
        integer, intent(in) :: linea, columna
        type(Error) :: e
        integer :: n

        ! inicializamos el error
        call initError(mensaje, tipo, linea, columna, e)

        ! añadir el error al array
        if (allocated(errors)) then
            n = size(errors)
            call extendArray(errors)
        else
            allocate(errors(1))
            n = 0
        end if

        ! añadir el errors al array
        errors(n+1) = e
    end subroutine addError

    subroutine extendArray(errors)
        implicit none
        type(Error), allocatable, intent(inout) :: errors(:)
        type(Error), allocatable :: temp(:)
        integer :: n

        ! extender el array
        n = size(errors)
        allocate(temp(n+1))
        temp(1:n) = errors
        deallocate(errors)
        errors = temp
    end subroutine extendArray

end module ErrorModule