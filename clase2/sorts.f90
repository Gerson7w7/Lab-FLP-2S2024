module sorts 
    implicit none

    contains

    ! subrutina que utiliza el algoritmo de ordenamiento por inserción
    subroutine insert_sort(arr)
        integer, dimension(:) :: arr ! arreglo a ordenar
        integer :: i, j, key ! variables auxiliares
        ! i recorre el arreglo desde el segundo elemento hasta el último
        ! j recorre el arreglo desde el primer elemento hasta el elemento i
        ! key es el elemento que se va a comparar con los elementos anteriores

        do i = 2, size(arr)
            key = arr(i) ! guardamos el elemento actual en key
            j = i - 1 ! j es el elemento anterior a i

            do while (j > 0 .and. arr(j) > key) ! mientra no lleguemos al inicio del arreglo y el valor sea mayor al que queremos insertar
                arr(j + 1) = arr(j) ! movemos el elemento a la derecha
                j = j - 1 ! disminuimos j
            end do 
            arr(j + 1) = key ! insertamos el elemento en la posición correcta
        end do
    end subroutine insert_sort

    subroutine selection_sort(arr)
        integer, dimension(:) :: arr ! arreglo a ordenar
        integer :: i, j, min_index, temp ! variables auxiliares
        ! i recorre el arreglo desde el primer elemento hasta el penúltimo
        ! j recorre el arreglo desde el elemento siguiente a i hasta el último
        ! min_index guarda el índice del elemento más pequeño
        ! temp es una variable auxiliar para intercambiar elementos

        do i = 1, size(arr) - 1 
            min_index = i ! asumimos que el elemento más pequeño es el primero

            do j = i + 1, size(arr) ! recorremos el arreglo desd el siguiente elemento  
                if (arr(j) < arr(min_index)) then ! si encontramos un elemento mas pequenio
                    min_index = j ! actualizamos el índice del elemento más pequeño
                end if
            end do 

            ! intercambiamos los elementos
            temp = arr(i) ! guardamos el elemento actual en temp
            arr(i) = arr(min_index) ! intercambiamos el elemento actual por el más pequeño
            arr(min_index) = temp ! intercambiamos el elemento más pequeño por el actual
        end do
    end subroutine selection_sort

    subroutine bubble_sort(arr)
        integer, dimension(:) :: arr ! arreglo a ordenar
        integer :: i, j, temp ! variables auxiliares
        ! i recorre el arreglo desde el primer elemento hasta el penúltimo
        ! j recorre el arreglo desde el último elemento hasta i
        ! temp es una variable auxiliar para intercambiar elementos

        do i = 1, size(arr) - 1 
            do j = 1, size(arr) - i 
                if (arr(j) > arr(j + 1)) then  
                    temp = arr(j) ! guardamos el elemento actual en temp
                    arr(j) = arr(j + 1) ! intercambiamos el elemento actual por el siguiente
                    arr(j + 1) = temp ! intercambiamos el siguiente por el actual
                end if
            end do
        end do 
    end subroutine bubble_sort

end module sorts