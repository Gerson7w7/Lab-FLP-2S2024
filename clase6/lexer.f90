program main_program
    use states
    use TokenModule
    use ErrorModule
    use ObjetoModule
    implicit none
    ! Variables
    integer :: i, linea, columna
    character(len=100) :: line
    integer :: ios
    integer :: estado 
    character(len=100) :: buffer_
    type(Token), allocatable :: tokens(:) 
    type(Error), allocatable :: errors(:)
    type(Objeto), allocatable :: instancias(:)
    integer :: line_length

    ! Inicializar el array de tokens
    allocate(tokens(0))
    ! Inicializar el array de errores
    allocate(errors(0))
    ! Inicializar variables
    linea = 1
    columna = 1
    estado = 0
    buffer_ = ''
    line = ''
    ios = 0
    estado = 0
    line_length = 0

    ! Abrimos el archivo entrada.lfp en modo lectura
    open(unit=10, file='entrada.lfp', status='old')

    ! Leemos el archivo línea por línea
    do
        i = 1
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit
        line_length = len_trim(line)
        ! Recorremos la línea
        do while (i <= line_length)
            ! Imprimimos el carácter actual (opcional)
            ! print *, line(i:i), 'estado ', estado
            if (estado == 0) then
                ! Estado 0
                call state0(line(i:i), buffer_, tokens, errors, linea, columna, estado, line_length == i)
            else if (estado == 1) then
                ! Estado 1
                call state1(line(i:i), buffer_, tokens, linea, columna, estado, i)
            else if (estado == 2) then
                ! Estado 2 
                call state2(line(i:i), buffer_, tokens, linea, columna, estado)
            else if (estado == 3) then
                ! Estado 3
                call state3(line(i:i), buffer_, tokens, errors, linea, columna, estado)
            else if (estado == 4) then
                ! Estado 4
                call state4(line(i:i), buffer_, tokens, linea, columna, estado, line_length == i)
            else if (estado == 5) then
                ! Estado 5
                call state5(line(i:i), buffer_, tokens, errors, linea, columna, estado)
            else if (estado == 6) then
                ! Estado 6
                call state6(line(i:i), buffer_, tokens, linea, columna, estado, line_length == i)
            else if (estado == 7) then
                ! Estado 7
                call state7(line(i:i), buffer_, tokens, linea, columna, estado, line_length == i)
            else if (estado == 8) then
                ! Estado 8
                call state8(line(i:i), buffer_, tokens, linea, columna, estado, i)
            end if
            columna = columna + 1
            i = i + 1
        end do
        linea = linea + 1
        columna = 1
    end do

    ! Cerramos el archivo
    close(10)

    ! Opcional: imprimir los tokens generados
    print *, 'Tokens generados:'
    do i = 1, size(tokens)
        print *, 'Token:', tokens(i)%lexema, 'Tipo:', tokens(i)%tipo, 'Linea:', tokens(i)%linea, 'Columna:', tokens(i)%columna
    end do

    ! Opcional: imprimir los errores generados
    print *, 'Errores generados:'
    do i = 1, size(errors)
        print *, 'Error:', errors(i)%mensaje, 'Tipo:', errors(i)%tipo, 'Linea:', errors(i)%linea, 'Columna:', errors(i)%columna
    end do

    ! comienzo del analisis sintactico

end program main_program
