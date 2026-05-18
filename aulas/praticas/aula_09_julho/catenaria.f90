program pvc_tiro
    implicit none

    double precision, dimension (:), allocatable :: y, p
    double precision :: erro, tol, eta, h, a, yb, x
    integer :: k, n, Nfim, ordem 
    
    ordem = 2

    allocate(y(ordem), p(ordem))

    Nfim=50
    h=1.d0/dble(Nfim)

    a = 1.5d0
    yb = 3.5d0

    tol=1.d-8
    k=0
    eta=2.d0
    erro=1983.d0


    do while (erro.ge.tol)
        k = k + 1
        open(unit=123, file='saida.dat', status='unknown')
        y(1)= 3.5d0
        y(2) = eta ! chute !

        p(1) = 0.d0
        p(2) = 1.d0

        write(123, *) 0.d0, y
        do n=0, Nfim -1
            y = y + h * f(ordem, a, x, y)
            p = p + h * fp(ordem, a, x, y, p)
            write(123, *) (n+1)*h, y
        end do
        close(unit=123)
        
        eta = eta - phi(y(1), yb)/philinha(p(1)) !Newton-Raphson
        erro = dabs(phi(y(1), yb))
        write(*,*) 'Iteracao:', k, 'Erro:', erro
    end do

    deallocate(y, p)

contains 

    function f(ordem, a, x, y)
        implicit none

        double precision, dimension (ordem) :: f, y
        double precision :: x, a
        integer :: ordem

        f(1) = y(2)
        f(2) = a*(dsqrt(1.d0+y(2)*y(2)))


    end function f

    function fp(ordem, a, x, y,p)

        implicit none

        double precision, dimension (ordem) :: fp, p, y
        double precision :: x, a
        integer :: ordem


        fp(1) = p(2)
        fp(2) = a*y(2)*p(2)/dsqrt(1.d0+y(2)*y(2))

    end function fp

    function phi(y, yb)
        implicit none

        double precision :: y, yb, phi

        phi = y - yb

        return

    end function phi

    function philinha(p)
        implicit none

        double precision :: p
        double precision :: philinha

        philinha = p

        return

    end function philinha

end program pvc_tiro