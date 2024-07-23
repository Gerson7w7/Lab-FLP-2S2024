program outputdata
implicit none 
    real, dimension(100) :: x, y, p, q
    integer :: i 

    ! asigno valores a mis arreglos
    do i = 1, 100 
        x(i) = i * 0.1
        y(i) = sin(x(i)) * (1 - cos(x(i)/3.0))
    end do 

    ! pasar los datos a un archivo
    open(1, file='data1.dat', status='new')
    do i = 1, 100
        write(1, *) x(i), y(i)
    end do
    close(1)

    ! abrir el archivo y leer los datos
    open(2, file='data1.dat', status='old')
    do i = 1, 100
        read(2, *) p(i), q(i)
    end do
    close(2)

    ! imprimir los datos
    do i = 1, 100
        print *, p(i), q(i)
    end do

end program outputdata