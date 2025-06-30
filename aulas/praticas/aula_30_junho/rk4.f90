program rk4

    double precision , dimension(:), allocatable :: y 
    integer :: Nfim, n
    double precision :: h, k1, k2, k3, k4
    
    Nfim = 20
    h = 2.d0/dble(Nfim)
    allocate(y(0:Nfim))

    y(0) = 1.d0  ! Initial condition y(0) = 1
    do n = 0, Nfim - 1
        k1 = f(n*h, y(n))
        k2 = f(n*h + 0.5d0 * h, y(n) + 0.5d0 * h * k1)
        k3 = f(n*h + 0.5d0 * h, y(n) + 0.5d0 * h * k2)
        k4 = f((n+1)*h, y(n) + h * k3)
        y(n+1) = y(n) + h/6.d0 * (k1 + 2.d0 * k2 + 2.d0 * k3 + k4)
    end do

    open(unit = 123, file = 'saida.dat', status = 'unknown')
    do n = 0, Nfim
        write(*,*) n*h, y(n)
    end do
    close(unit = 123)

    implicit none

contains 
    
    function f(t, y)

        implicit none 

        f = (1.d0 - 4.d0 * t/3.d0) * y

        !y' = (1 - 4t/3)y, y(0) = 1

        return 
    
    end function f

end program rk4