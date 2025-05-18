program iteracoes
  implicit none
  integer, parameter :: nmax = 20      ! nº de iterações a exibir
  double precision   :: x0             ! chute inicial
  integer            :: k

  !--------------------------------------------------------------------
  x0 = 1.0d0   ! <<< ALTERE AQUI O CHUTE INICIAL                        |
  !--------------------------------------------------------------------

  print *, 'Iterando a partir de x0 =', x0
  print *, '----------------------------------------------------------'

  ! ---------- MÉTODO (i) --------------------------------------------
  call roda('Metodo (i)', x0, nmax, g1)

  ! ---------- MÉTODO (ii) -------------------------------------------
  call roda('Metodo (ii)', x0, nmax, g2)

  ! ---------- MÉTODO (iii) ------------------------------------------
  call roda('Metodo (iii)', x0, nmax, g3)

  ! ---------- MÉTODO (iv) (Newton) ----------------------------------
  call roda('Metodo (iv)', x0, nmax, g4)

  ! ---------- MÉTODO (v) --------------------------------------------
  call roda('Metodo (v)', x0, nmax, g5)

contains
  !--------- funções g_i(x)  -----------------------------------------
  double precision function g1(x)
    double precision, intent(in) :: x
    g1 = x - (x**3 - 4.d0*x**2 + 10.d0)
  end function g1

  double precision function g2(x)
    double precision, intent(in) :: x
    double precision :: rad
    rad = 10.d0/x - 4.d0*x
    if (rad < 0.d0) then
       print *, '   RADICANDO NEGATIVO -> interrompendo'
       g2 = x   ! devolve o próprio x só pra parar de explodir
    else
       g2 = sqrt(rad)
    end if
  end function g2

  double precision function g3(x)
    double precision, intent(in) :: x
    double precision :: rad
    rad = 10.d0/(4.d0 + x)
    if (rad < 0.d0) then
       print *, '   RADICANDO NEGATIVO -> interrompendo'
       g3 = x
    else
       g3 = sqrt(rad)
    end if
  end function g3

  double precision function g4(x)
    double precision, intent(in) :: x
    double precision :: f, fp
    f  = x**3 + 4.d0*x**2 - 10.d0
    fp = 3.d0*x**2 + 8.d0*x
    g4 = x - f / fp          ! passo de Newton
  end function g4

  double precision function g5(x)
    double precision, intent(in) :: x
    double precision :: rad
    rad = 10.d0 - x**3
    if (rad < 0.d0) then
       print *, '   RADICANDO NEGATIVO -> interrompendo'
       g5 = x
    else
       g5 = 0.5d0 * sqrt(rad)
    end if
  end function g5

  !--------- sub-rotina que realiza as iterações ----------------------
  subroutine roda(titulo, xini, n, g)
    character(len=*), intent(in)          :: titulo
    double precision , intent(in)         :: xini
    integer           , intent(in)        :: n
    interface
       double precision function g(x)
         double precision, intent(in) :: x
       end function g
    end interface
    double precision :: x
    integer          :: k

    print *, titulo
    x = xini
    do k = 1, n
       x = g(x)
       print '(I3,2X,F24.16)', k, x
    end do
    print *, '----------------------------------------------------------'
  end subroutine roda
end program iteracoes

