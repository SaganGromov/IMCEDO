program prog2

  implicit none

  real, dimension(:), allocatable :: res
  integer :: i,j,N
  real :: x
  real, dimension(:,:), allocatable :: a

  N = 3

  !allocate(res(N))
  !  
  !do i=1,N
  !   x = 0.1*i
  !   res(i) =  bonita(x)
  !end do
  !
  !do i=1,N
  !   write(*,*) res(i)
  !end do

  allocate(a(N,N))
  
  call preenchematriz(N,a)

  do i=1,N
     do j=1,N
        write(*,*) i,j,a(i,j)
     end do
  end do
  
  !deallocate(res)
  deallocate(a)
  
contains

  function bonita(x)

    implicit none

    real :: bonita, x

    bonita = x**0.8-sin(x)

    return
    
  end function bonita

  subroutine preenchematriz(tamanho,saida)

    implicit none

    integer :: tamanho, i, j
    real, dimension(tamanho,tamanho) :: saida

    do i=1,tamanho
       do j=1,tamanho
          saida(i,j) = bonita(1.0*(i+j))*bonita(2.0*(i+j))
       end do
    end do

    return
    
  end subroutine preenchematriz

  
end program prog2