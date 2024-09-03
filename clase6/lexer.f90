program main_program 
    use TokenModule
    use ErrorModule
    implicit none
    integer :: i, linea, columna 
    character(len=100) :: line 
    integer :: ios 
    integer :: estado
    character(len=100) :: buffer_ 
    type(Token), allocatable :: tokens(:)
    type(Error), allocatable :: errors(:)
    integer :: line_length

    ! inicializamos los arrays
    allocate(tokens(0))
    allocate(errors(0))

    ! inicializamos el estado
    estado = 0
    buffer_ = ""
    linea = 0
    columna = 0
    ios = 0
    line_length = 0

    ! abrimos el archivo
    open(unit=10, file="entrada.lfp", status="old", action="read", iostat=ios)

    ! leemos el archivo
    do  
        i = 1
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) then
            exit
        end if
        line_length = len_trim(line)
        ! recorremos la linea
        do while (i <= line_length)
            ! print *, "linea: ", line(i:i)
            if (estado == 0) then 
                ! estado 0 
            else if (estado == 1) then 
                ! estado 1
            else if (estado == 2) then
                ! estado 2
            else if (estado == 3) then
                ! estado
            else if (estado == 4) then
                ! estado 
            else if (estado == 5) then
                ! estado 
            else if (estado == 6) then
            ! estado 
            else if (estado == 7) then
            ! estado 
            end if 
            i = i + 1
        end do
        linea = linea + 1
    end do

end program main_program