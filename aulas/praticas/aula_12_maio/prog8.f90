program lotka

  implicit none

  !double precision, dimension (:), allocatable :: u, v
  double precision, dimension (:), allocatable :: y
  double precision :: a, b, h
  integer :: n, Npoints, tamanho
  
  a = 0.d0
  b = 8.d0
  Npoints = 2560
  h = (b-a)/dble(Npoints)

  tamanho = 2  !tamanho --> núm de equações
  
  allocate(y(tamanho))

  open(unit=123,file='saida.dat',status='unknown')
  y(1) = 1.5d0
  y(2) = 1.5d0
  write(123,*) 0.d0*h,y
  do n = 0, Npoints !método de euler
     y = y + h*f(tamanho,y)
     write(123,*) (n+1)*h,y
  end do
  close(unit=123)

  deallocate(y)
  
contains

  function f(tamanho,y)

    implicit none

    double precision, dimension (tamanho) :: y, f
    integer :: tamanho

    f(1) = -9.d0*y(1) + 3.d0*y(1)*y(2)
    f(2) = 2.d0*y(2) - y(1)*y(2)
    
    return
    
  end function f

  function integral(u,v)

    implicit none

    double precision :: u, v, integral

    integral = 9.d0*dlog(u)-3.d0*u+2.d0*dlog(v)-v
    
    return
    
  end function integral
  
end program lotka


  
  !allocate(u(0:Npoints))
  !allocate(v(0:Npoints))
  !código tosco
  !u(0) = 1.5d0
  !v(0) = 1.5d0
  !do n=0,Npoints-1
  !   u(n+1) = u(n) + h*(-9.d0*u(n)+3.d0*u(n)*v(n))
  !   v(n+1) = v(n) + h*(2.d0*v(n)-u(n)*v(n))
  !end do
  !deallocate(u)
  !deallocate(v)
  !do n=0,Npoints
  !   write(123,*) n*h,u(n),v(n),integral(u(n),v(n))
  !end do
