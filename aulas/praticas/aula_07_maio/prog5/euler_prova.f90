program euler
    implicit none

    double precision, dimension (:), allocatable :: y 
    double precision :: a, b, h 
    integer :: n, Npoints

    !intervalo de integração
    a=0.d0
    b=10.d0
    Npoints = 1000.d0
    h=(b-a)/dble(Npoints)

    allocate(y(0:Npoints))

    y(0) = 0.d0

    do n = 0, Npoints - 1
        !t = t+h
        ! y(n+1) = y(n)+h*f(t(n), y(n))
        y(n+1) = y(n) + h*f(n*h, y(n))
    end do

    open(unit=123, file='euler_prova.dat', status='unknown')
    do n =0, Npoints 
        write(123,*) n*h, y(n)
    end do
    close(123)

    write(*,*) h, dabs(y(Npoints)-(dexp(2.d0-8.d0/3.d0)))
    !cálculo do erro global em y(2)

    deallocate(y)

contains 


    function f(t, y)
        implicit none 

        double precision :: f, t, y 

        f = (1.d0 - y - 2.d-1 * y*y)

        return 
    end function f

        function f_new(y, h)
        implicit none
        double precision :: f_new, y, h
        f_new = dcos(y + h/2.d0 * dsqrt(8.d0*dcos(y) + 4.d0*dsqrt(2.d0)))
        return
    end function f_new
    


end program euler