program iterativo
    implicit none
    
    integer :: i
    double precision, dimension(:), allocatable :: x

    allocate(x(0:100))
    x(0) =  -18.d0

    do i = 0,99
        x(i+1) = g(x(i))
    end do

    open(unit=123, file='saida.dat', status='unknown')
    do i = 0,100
        write(123,*) i, x(i)
    end do
    close(unit=123)
contains
    function g(x)
        implicit none
        double precision :: g, x
        g = x - f(x)/flinha(x)
        return
    end function g

    function f(x)
        implicit none
        double precision :: f, x
        f = x*x*x - x - 1.d0
        return
    end function f

    function flinha (x)
        implicit none
        double precision :: flinha, x
        flinha = 3.d0*x**2 - 1.d0
        return
    end function flinha
end program iterativo