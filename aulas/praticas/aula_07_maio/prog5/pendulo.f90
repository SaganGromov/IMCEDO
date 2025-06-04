program ponto_medio
    implicit none
    integer, parameter :: N = 1000
    double precision :: a, b, h, x
    double precision, dimension(0:N) :: y, y_2
    double precision :: y_mid, y_2_mid
    integer :: n

    a = 0.d0
    b = 10.d0
    h = (b - a) / dble(N)

    y(0) = 3.d0 * atan(1.d0)  ! pi/4
    y_2(0) = 0.d0

    do n = 0, N - 1
        x = a + n * h

        ! Midpoint estimates
        y_mid    = y(n)    + 0.5d0 * h * y_2(n)
        y_2_mid  = y_2(n)  + 0.5d0 * h * f2(x, y(n), y_2(n))

        ! Full step using midpoint evaluation
        y(n+1)   = y(n)    + h * f1(x + 0.5d0 * h, y_mid, y_2_mid)
        y_2(n+1) = y_2(n)  + h * f2(x + 0.5d0 * h, y_mid, y_2_mid)
    end do

    open(unit=123, file='pendulo.dat', status='replace')
    do n = 0, N
        write(123,*) a + n * h, y(n), y_2(n)
    end do
    close(123)

contains

    function f1(t, y1, y2) result(res)
        implicit none
        double precision, intent(in) :: t, y1, y2
        double precision :: res
        res = y2
    end function f1

    function f2(t, y1, y2) result(res)
        implicit none
        double precision, intent(in) :: t, y1, y2
        double precision :: res
        res = -4.d0 * sin(y1)
    end function f2

end program ponto_medio
