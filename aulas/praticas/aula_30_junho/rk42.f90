program rk42

    implicit none

    double precision, dimension(:), allocatable :: y, k1, k2, k3, k4
    double precision :: h
    integer :: Nfim, n, tam

    tam = 2
    allocate(y(tam), k1(tam), k2(tam), k3(tam), k4(tam))
    Nfim = 100
    h = 5.d0/dble(Nfim)

    y(1) = 1.5d0
    y(2) = 1.5d0 
    open(unit=123, file='RK42.dat', status='unknown')
    write(123,*) 0.0, y
    do n = 0, Nfim - 1
        k1 = f(tam, n*h, y)
        k2 = f(tam, n*h + h/2.d0, y + h * k1 * 0.5d0) 
        k3 = f(tam, n*h + h/2.d0, y + h * k2 * 0.5d0)
        k4 = f(tam, n*h + h, y + h * k3)
        y = y + h * (k1 + 2.d0 * k2 + 2.d0 * k3 + k4) / 6.d0
        write(123,*) (n+1)*h, y

    end do
    close(123)

contains

    function f(tam, t, x)

        implicit none 

        integer :: tam 
        double precision :: t 
        double precision, dimension(tam) :: x, f

        f(1) = 2.d0*x(1) - x(1) * x(2)
        f(2) = -9.d0 * x(2) + 3.d0 * x(1) * x(2)
    return 
    end function f
end program rk42