program iterativo
    implicit none
    
    integer :: i
    double precision, dimension(:), allocatable :: x

    allocate(x(0:100))
    x(0) =  1.5d0

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
        g = (x + 1.d0)** (1.0 / 3.0)
        return
    end function g
end program iterativo