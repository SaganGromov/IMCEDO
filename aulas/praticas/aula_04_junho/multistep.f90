program multistep

    implicit none 


    double precision, dimension(:), allocatable :: yeuler, yab3
    double precision :: a, b, h
    integer :: n, Npoints 


    Npoints = 320
    allocate(yeuler(0:Npoints))
    allocate(yab3(0:Npoints))

    a = 0.d0
    b = 2.d0 
    h = (b-a)/dble(Npoints)

    ! yab3(0) = 1.d0 
    ! yab3(1) = yab3(0) + h*f(0.d0, yab3(0)) !Euler para inicializar y(1)
    ! yab3(2) = yab3(1) + h*f(h, yab3(1))

    yab3(0) = 1.d0
    yab3(1) = dexp(h-2.d0*h*h/3.d0)  
    yab3(2) = dexp(2.d0*h-2.d0*4.d0*h*h/3.d0)  
    !     y(n+2) = y(n+1) + h*(1.5d0*f((n+1)*h, y(n+1)) - 0.5d0*f(n*h, y(n)))
    ! end do
    !ordem 2 do método de Adams-Bashforth

    do n = 0, Npoints - 3
        yab3(n+3) = yab3(n+2) + h*(23.d0/12.d0*f((n+2)*h, yab3(n+2)) - 16.d0/12.d0*f((n+1)*h, yab3(n+1)) + 5.d0/12.d0*f(n*h, yab3(n)))
    end do

    yeuler(0) = 1.d0 
    do n = 0, Npoints - 1
        yeuler(n+1) = yeuler(n) + h*f(n*h, yeuler(n))
    end do

    open(unit=123, file = 'saida.dat', status = 'unknown')
    do n=0,Npoints 
        write(123, *) n*h, yab3(n), yeuler(n)
    end do 
    close(unit=123)

    write(*, *) h, dabs(yab3(Npoints) - dexp(-2.d0/3.d0))

    deallocate(yeuler)
    deallocate(yab3)
    

contains 

    function f(t, y)
        implicit none
        double precision :: t, y
        double precision :: f

        f = (1.d0 - 4.d0/3.d0*t)*y

    end function f


end program multistep