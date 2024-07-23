program arrayProg 
    real :: numbers(5) ! mi arreglo de 5 elementos
    integer :: matrix(3,3) ! mi matriz de 3x3

    ! asignos valores a mi arreglo
    do i = 1, 5
        numbers(i) = i * 2.0
    end do

    ! asigno valores a mi matriz
    do i = 1, 3
        do j = 1, 3
            matrix(i,j) = i + j
        end do
    end do

    ! imprimo mi arreglo
    print *, "Arreglo de 5 elementos"
    do i = 1, 5
        print *, numbers(i)
    end do

    ! imprimo mi matriz
    print *, "Matriz de 3x3"
    do i = 1, 3
        do j = 1, 3
            print *, matrix(i,j)
        end do
    end do

end program arrayProg