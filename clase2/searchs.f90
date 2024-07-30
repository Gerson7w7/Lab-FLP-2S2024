module searchs 
    implicit none 
    contains

        ! subrutina que utiliza el algoritmo de búsqueda secuencial
        subroutine sequential_search(arr, n, target, index)
            integer, intent(in) :: arr(:) ! arreglo en el que se va a buscar
            integer, intent(in) :: n ! tamaño del arreglo
            integer, intent(in) :: target ! valor a buscar
            integer, intent(out) :: index ! índice del valor encontrado

            integer :: i ! variable auxiliar

            index = -1 ! inicializamos el índice en -1

            do i = 1, n 
                if (arr(i) == target) then ! si encontramos el valor
                    index = i ! guardamos el índice
                    exit ! salimos del bucle
                end if
            end do
        end subroutine sequential_search

        ! subrutina que utiliza el algoritmo de búsqueda binaria
        subroutine binary_search(arr, n, target, index)
            integer, intent(in) :: arr(:) ! arreglo en el que se va a buscar    
            integer, intent(in) :: n ! tamaño del arreglo
            integer, intent(in) :: target ! valor a buscar
            integer, intent(out) :: index ! índice del valor encontrado

            integer :: low, high, mid ! variables auxiliares

            index = -1 ! inicializamos el índice en -1
            low = 1 ! inicializamos el límite inferior
            high = n ! inicializamos el límite superior

            do while (low <= high) ! mientras el limite inferior sea menor o igual al superior
                mid = (low + high) / 2 ! calculamos el punto medio

                if (arr(mid) == target) then ! si encontramos el valor 
                    index = mid ! guardamos el indice 
                    exit 
                else if (arr(mid) < target) then 
                    low = mid + 1 ! actualizamos el límite inferior
                else 
                    high = mid - 1 ! actualizamos el límite superior
                end if
            end do
        end subroutine binary_search
end module searchs 