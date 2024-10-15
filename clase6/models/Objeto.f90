module ObjetoModule
    use Utils
    implicit none

    ! Definición del tipo Objeto
    type :: Objeto
        character(len=100) :: nombre
        character(len=100) :: variable
        character(len=100), allocatable :: parametros(:)
    end type Objeto

contains

    ! Subrutina para añadir un Token a un array de Tokens
    subroutine addObjeto(objetos)
        implicit none
        type(Objeto), allocatable, intent(inout) :: objetos(:)
        type(Objeto) :: o
        integer :: n

        ! inicializamos el objeto
        o%nombre = ''
        o%variable = ''
        allocate(o%parametros(0))
        
        ! Añadimos el token al array, extendiendo el array de tokens
        if (allocated(objetos)) then
            n = size(objetos)
            call extendArray(objetos)
        else
            allocate(objetos(1))
            n = 0
        end if
        objetos(n+1) = o
    end subroutine addObjeto

    subroutine extendArray(objetos)
        implicit none
        type(Objeto), allocatable, intent(inout) :: objetos(:)
        type(Objeto), allocatable :: temp(:)
        integer :: n

        n = size(objetos)
        allocate(temp(n + 1))
        temp(1:n) = objetos
        deallocate(objetos)
        objetos = temp
    end subroutine extendArray

    ! function crearDB(objeto)
    !     implicit none
    !     type(Objeto), intent(in) :: objeto
    !     character(len=100) :: crearDB 
    !     crearDB = 'use(' // objeto%variable // ');'
    ! end function crearDB

    ! function crearColeccion(objeto)
    !     implicit none
    !     type(Objeto), intent(in) :: objeto
    !     character(len=100) :: crearColeccion
    !     crearColeccion = objeto%parametros(1) // '.createCollection(' // objeto%variable // ');'
    ! end function crearColeccion

    ! function insertarUnico(objeto)
    !     implicit none
    !     type(Objeto), intent(in) :: objeto
    !     character(len=100) :: insertarUnico
    !     insertarUnico = objeto%parametros(1) // '.insertOne(' // objeto%parametros(2) // ');'
    ! end function insertarUnico

end module ObjetoModule
