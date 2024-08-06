program main
    use GameModule
    use leerArchivo
    implicit none 
    integer :: max_lineas, i 
    character(len=100) :: contenido(100)
    character(len=100) :: file_name 
    character(len=1) :: delimitador
    type(Juego), allocatable :: juegos(:)

    ! Initialize variables
    max_lineas = 105
    file_name = 'practica1.lfp'
    delimitador = ','
    allocate(juegos(0))

    ! Leer archivo de entrada 
    call readFile(max_lineas, contenido, file_name)

    ! Leemos linea por linea y separamos por comas 
    do i = 2, max_lineas 
        if (trim(contenido(i) /= '')) then 
            call separar(contenido(i), delimitador, juegos)
        end if
    end do


end program main