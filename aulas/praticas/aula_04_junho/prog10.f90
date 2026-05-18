program multistep2
    implicit none 

    double precision, dimension(:), allocatable :: y, yold, ybackup 
    integer :: n, Npoints, dim 
    double precision :: a, b, h
    
    dim = 2 


    Npoints = 40
    allocate(y(dim))
    allocate(yold(dim))
    allocate(ybackup(dim))

    a = 0.d0 
    b = 4.d0
    h = (b-a)/dble(Npoints)

    open(unit=123, file = 'saida10.dat', status = 'unknown')
    yold = 1.5d0 !condição inicial
    y = yold + h*f(dim, 0.d0, yold) ! inicialização por Euler
    write(123, *) 0.d0, yold
    write(123, *) h, y

    do n = 0, Npoints - 2

        ybackup = y
        y = y + h*(1.5d0 * f(dim, (n+1)*h, y) - 0.5*f(dim, n*h, yold)) ! passo AB2 - preditor 

        y = ybackup + h*(5.d0*f(dim, (n+2)*h, y) + 8.d0 * f(dim, (n+1)*h, ybackup) - f(dim, n*h, yold))/12.d0 ! passo AM2 - corretor


        yold = ybackup
        write(123, *) (n+2)*h, y
    end do
    close(unit=123)

    deallocate(y)
    deallocate(yold)
    deallocate(ybackup)

contains 
    function f(dim, t, y)

        implicit none 

        double precision :: t 
        double precision, dimension(dim) :: y, f 
        integer :: dim 
        f(1) = 2.d0 * y(1) - y(1) * y(2)
        f(2) = -9.d0*y(2) + 3.d0 * y(1) * y(2)

        return 
    end function f

end program