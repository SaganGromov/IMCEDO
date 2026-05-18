! rk4_module.f90
!
! Generic fixed–step classical Runge–Kutta integrator
! ---------------------------------------------------
module rk4_module
   use iso_fortran_env, only : real64, int64
   implicit none
   private
   public :: rk4_integrate

   ! Prototype for the RHS f(t,y)
   abstract interface
      pure function rhs_fun(t,y) result(f)
         import :: real64
         real(real64), intent(in) :: t, y
         real(real64)            :: f
      end function rhs_fun
   end interface

contains
   !--------------------------------------------------------------------
   ! Integrate y' = f(t,y),  y(t0) = y0  on [t0 , t_final]  with nsteps
   ! Output: y_hist(1) = y0, … , y_hist(nsteps+1) = y(t_final)
   !--------------------------------------------------------------------
   subroutine rk4_integrate(rhs, t0, y0, t_final, nsteps, y_hist)
      procedure(rhs_fun)          :: rhs
      real(real64), intent(in)    :: t0, y0, t_final
      integer(int64), intent(in)  :: nsteps
      real(real64), intent(out)   :: y_hist(:)   ! size ≥ nsteps+1

      real(real64) :: h, t, y
      real(real64) :: k1, k2, k3, k4
      integer(int64) :: n

      h = (t_final - t0) / real(nsteps, real64)
      t = t0
      y = y0
      y_hist(1) = y                          ! n = 0

      do n = 1_int64, nsteps
         k1 = rhs(t,               y)
         k2 = rhs(t + 0.5_real64*h, y + 0.5_real64*h*k1)
         k3 = rhs(t + 0.5_real64*h, y + 0.5_real64*h*k2)
         k4 = rhs(t + h,            y + h*k3)
         y  = y + h*(k1 + 2.0_real64*k2 + 2.0_real64*k3 + k4)/6.0_real64
         t  = t + h
         y_hist(n+1) = y
      end do
   end subroutine rk4_integrate
end module rk4_module
