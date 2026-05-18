program euler_method
    implicit none
    integer, parameter :: N = 1000
    double precision :: a, b, h, x
    double precision, dimension(0:N) :: y, y_2
    integer :: n

    a = 0.d0
    b = 3
    h = (b - a) / dble(N)

    y(0)   = 1.d-1 ! y
    y_2(0) = 0.d0 !y'

    do n = 0, N - 1
        x = a + n * h

        ! Explicit Euler update:
        y(n+1)   = y(n)   + h * y_2(n)
        y_2(n+1) = y_2(n) + h * f2(x, y(n), y_2(n))
    end do

    open(unit=123, file='pendulo_euler.dat', status='replace')
    do n = 0, N
        write(123,*) a + n * h, y(n), y_2(n)
    end do
    close(123)

contains

    function f2(t, y1, y2) result(res)
        implicit none
        double precision, intent(in) :: t, y1, y2
        double precision :: res
        res = -1.d-1 * t + 1.d-1 * y2 * y2 - sin(10*y1)
    end function f2

end program euler_method
