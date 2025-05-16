program prog1

  implicit none

  !real :: x
  integer :: i

  !write(*,*) 'Hello!' 


  !write(*,*) 'Insira um número:'
  !read(*,*) x
  !write(*,*) 'O seu quadrado é:', x*x

  !do i=1,10
  !   write(*,*) 'O quadrado de', i, 'é:', i*i     
  !end do

  open(unit=123,file='saida.dat',status='unknown')

  do i=1,100
     write(123,*)0.1*i, sqrt(bonita(0.1*i))
  end do

  close(unit=123)

contains

  function bonita(feia)

    implicit none

    real :: feia, bonita

    bonita = feia**0.8-sin(feia)

    return    
    
  end function bonita


  
end program prog1