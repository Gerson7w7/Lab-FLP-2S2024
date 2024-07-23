program calling_func    
    real :: a 
    a = area_of_circle(2.0)

    print *, 'El area del circulo con radio 2.0 es: '
    print *, a
end program calling_func

! esta funcion calcula el area de un circulo con radio r
function area_of_circle(r)
implicit none  
    real :: area_of_circle ! el resultado de la funcion
    real :: r ! el radio del circulo
    real :: pi ! el valor de pi

    pi = 4 * atan(1.0)
    area_of_circle = pi * r ** 2 ! calculo del area
end function area_of_circle

