module constants 
implicit none 
    real, parameter :: my_pi = 3.1415 
    real, parameter :: my_e = 2.7183 

contains 
    subroutine show_const()
        print *, "El valor de pi es: ", my_pi 
        print *, "El valor de e es: ", my_e
    end subroutine show_const
end module constants

program main 
use constants
implicit none 
    real :: x, ePowerx, area, radius 
    x = 2.0 
    radius = 7.0
    area = my_pi * radius * radius

    call show_const() 

    print *, "El area de un circulo de radio ", radius, " es: ", area
    print *, "El valor de e es: ", my_e

end program main