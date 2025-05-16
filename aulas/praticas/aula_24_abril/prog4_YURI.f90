program newton

  implicit none

  double precision, dimension (:), allocatable :: x, y
  integer :: k, N
  double precision :: erro

  N = 2

  allocate(x(N))
  allocate(y(N))
  
  x(2) = 7.5d0 !chutes iniciais
  x(1) = 0.5d0
  do k=1,10
     call sistema(N,jac(N,x),f(N,x),y,erro) !resolve sistema para y
     x = x - y !avança no NR
     write(*,*) x, f(N,x),erro !imprime na tela
  end do

  deallocate(x)
  deallocate(y)

contains

  function f(N,x)

    implicit none

    double precision, dimension (N) :: f, x
    integer :: N

    f(2) = x(1)*x(1) + x(2)*x(2) - 64.d0
    f(1) = x(1)*x(2) - 3.d0
    
    return
    
  end function f


  function jac(N,x)

    implicit none

    double precision, dimension (N,N) :: jac
    double precision, dimension (N) :: x
    integer :: N
    
    jac(2,1) = 2.d0*x(1)
    jac(2,2) = 2.d0*x(2)
    jac(1,1) = x(2)
    jac(1,2) = x(1)
    
    return

  end function jac

  subroutine sistema(N,a,b,x,erro)

    implicit none

    double precision, dimension(N), intent (out) :: x
    double precision, dimension(N), intent (in) :: b
    double precision, dimension(N,N), intent (in) :: a
    double precision :: soma, erro, tol
    integer :: i,j,k,N
    
    x = 0.d0
    tol = 0.000000001d0
    erro = 190.d0
    k = 0
    
    do while (erro > tol)
       k = k + 1

       do i=1,N !Gauss-Seidel
          soma = 0.d0
          do j=1,N
             if(j.ne.i) then 
                soma = soma +a(i,j)*x(j)
             end if
          end do
          x(i) = (b(i) - soma)/a(i,i)
       end do !Fim Gauss-Seidel
       
       erro = 0.d0 !cálculo erro
       do i=1,N
          soma = 0.d0
          do j=1,N
             soma = soma + a(i,j)*x(j)
          end do
          soma = soma - b(i)
          erro = erro + soma*soma
       end do
       erro = dsqrt(erro)!Fim cálculo erro
       
       !write(*,*) k, x, erro!, tol
    end do

    !read(*,*)
    
    return
    
  end subroutine sistema
  
  
end program newton