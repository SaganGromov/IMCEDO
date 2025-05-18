program exercicio

  implicit none

  double precision, dimension (3) :: xold, x
  double precision, dimension (3,3) :: a
  integer :: n

  a(1,1) = -14.d0/11.d0
  a(1,2) = -8.d0/11.d0
  a(1,3) = -42.d0/11.d0
  a(2,1) = 25.d0/11.d0
  a(2,2) =-47.d0/11.d0
  a(2,3) =108.d0/11.d0
  a(3,1) =13.d0/11.d0
  a(3,2) =-13.d0/11.d0
  a(3,3) =50.d0/11.d0
  
  x(1) = 1.d0
  x(2) = 1.d0
  x(3) = 1.d0
  
  do n = 1, 100
     xold = x
     x = mult(A,x,3)
     write(*,*) "Iteration:", n
     write(*,*) "Current vector x: ", x
     write(*,*) "Previous vector xold: ", xold
     write(*,*) "Ratios x(i)/xold(i): ", x(1)/xold(1), x(2)/xold(2), x(3)/xold(3)
     write(*,*) "Normalized vector: ", x/((x(1)/xold(1))**n)
     write(*,*) "----------------------------------------"
  end do

contains

  function mult(a,x,N)

    implicit none

    double precision, dimension (N) :: mult, x
    double precision, dimension (N,N) :: a
    double precision :: soma
    integer :: i,k,N
    
    do i=1,N
       soma = 0.d0
       do k=1,N
          soma = soma+a(i,k)*x(k)
       end do
       mult(i) = soma
    end do

    return
    
  end function mult
  
end program exercicio
