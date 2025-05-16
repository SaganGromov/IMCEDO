program trap

  implicit none

  double precision, dimension(:), allocatable :: y
  double precision :: a, b, h
  integer :: n, k, Npoints
  
  a = 0.d0
  b = 2.d0
  Npoints = 320
  h = (b-a)/dble(Npoints)

  allocate(y(0:Npoints))

  y(0) = 1.d0 
  do n = 0, Npoints-1
     y(n+1) = y(n) !chute inicial
     do k = 1, 10 !laço Newton-Raphson
        y(n+1) = y(n+1)- g(y(n+1), y(n), h, (n+1)*h, n*h)/glinha((n+1)*h,h)
     end do
  end do

  write(*,*) h, dabs(y(Npoints)-dexp(-2.d0/3.d0))

  !open(unit=123,file= 'saida.dat', status='unknown')
  !do n = 0, Npoints
  !   write(123,*) n*h,y(n)
  !end do
  !close(unit=123)

  deallocate(y)
  
contains

  function g(ynew, yold, h, tnew, told)

    implicit none

    double precision :: ynew, yold, h, tnew, told, g
    
    g = ynew - yold - 0.5d0*h*(f(told,yold)+f(tnew,ynew))
    
    return
    
  end function g

  function glinha(t, h)

    implicit none

    double precision :: glinha, h, t

    glinha = 1.d0-0.5d0*h*(1.d0-4.d0*t/3.d0)
    
    return
    
  end function glinha
  

  function f(t,y)

    implicit none

    double precision :: f,t,y
    
    f = (1.d0-4.d0*t/3.d0)*y !definição da EDO
    
    return

  end function f
  
end program trap
