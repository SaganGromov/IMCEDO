program eulerimp 
    implicit none 

    double precision, dimension(:), allocatable :: y
    double precision :: h, a, b
    integer :: Npoints, n, k

    a=0.d0
    b=2.d0
    Npoints = 20
    h = (b-a)/dble(Npoints)

    allocate(y(0:Npoints))  ! Alocação do vetor y


    y(0) = 1.d0 ! Condição inicial
    do n = 0, Npoints - 1
        ! yold = y(n-1) 
        ! ynew = yold  ! chute inicial para ynew
        y(n+1) = y(n)  ! chute inicial para ynew
        do k = 1, 10
            y(n+1) = y(n+1) - g(y(n+1), y(n), h, (n+1)*h)/glinha(y(n+1),y(n), h, (n+1)*h) ! Método de Newton
        end do 
        ! y(n+1) = ynew
    end do


    open(unit=123, file='saida.dat', status='unknown')  

    do n = 0, Npoints
        write(123,*) n*h, y(n)
    end do
    close(123)
    deallocate(y)  ! Liberação do vetor y
contains 

    function glinha(ynew, yold, h, tnew)
        implicit none 

        double precision :: glinha, ynew, yold, h, tnew

        glinha = 1.d0 - h * (1.d0 - 4.d0 * tnew/3.d0)  ! Derivada de g

        return 
    end function glinha

    function g(ynew, yold, h, tnew)
        implicit none 

        double precision :: g, ynew, yold, h, tnew

        g = ynew - yold - h*f(tnew, ynew) ! Euler implicito

        return 
    end function g 

    function f(t, y)
        implicit none 
        double precision :: f, t, y

        f = (1.d0 - 4.d0 * t/3.d0) * y ! definição da EDO

        return 

    end function f
end program eulerimp