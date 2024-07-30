program main 
    use sorts
    use searchs
    implicit none 
    integer, dimension(10) :: arr = [5,4,3,2,1,10,7,9,8,6]
    integer :: i ! indice para la búsqueda

    call insert_sort(arr) ! llamamos a la función insert_sort para ordenar el arreglo
    print *, 'Insert sort: ', arr ! imprimimos el arreglo ordenado

    arr = [5,4,3,2,1,10,7,9,8,6] ! reiniciamos el arreglo
    call selection_sort(arr) ! llamamos a la función selection_sort para ordenar el arreglo
    print *, 'Selection sort: ', arr ! imprimimos el arreglo ordenado

    arr = [5,4,3,2,1,10,7,9,8,6] ! reiniciamos el arreglo
    call bubble_sort(arr) ! llamamos a la función bubble_sort para ordenar el arreglo
    print *, 'Bubble sort: ', arr ! imprimimos el arreglo ordenado

    arr = [1,2,3,4,5,6,7,8,9,10] ! reiniciamos el arreglo
    call sequential_search(arr, size(arr), 7, i) ! llamamos a la función sequential_search para buscar el valor 7
    print *, 'Sequential search: ', i ! imprimimos el índice del valor encontrado

    arr = [1,2,3,4,5,6,7,8,9,10] ! reiniciamos el arreglo
    call binary_search(arr, size(arr), 7, i) ! llamamos a la función binary_search para buscar el valor 7
    print *, 'Binary search: ', i ! imprimimos el índice del valor encontrado

end program main