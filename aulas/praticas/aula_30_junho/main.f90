! main.f90
!
! Convergence study for the classical RK-4 scheme applied to
!    y' = (1 – 4t/3) y ,  y(0) = 1  on [0,2]
! -----------------------------------------------------------
program main
   use iso_fortran_env, only : real64, int64
   use rk4_module,        only : rk4_integrate
   implicit none

!────────────────── user-tunable constants ──────────────────
   integer(int64), parameter :: n0      = 20_int64   ! base mesh
   integer,        parameter :: i_min   = 5
   integer,        parameter :: i_max   = 21
   real(real64),   parameter :: t0      = 0.0_real64
   real(real64),   parameter :: t_final = 2.0_real64
   real(real64),   parameter :: y0      = 1.0_real64
!─────────────────────────────────────────────────────────────

   integer(int64)           :: max_steps, nsteps
   real(real64), allocatable :: y_hist(:)
   real(real64)             :: h, err, trueZ
   integer                  :: i, unit_stage, unit_conv

!---------- exact solution and RHS (kept local) --------------
   interface
      pure function rhs(t,y) result(f)
         use iso_fortran_env, only : real64
         real(real64), intent(in) :: t, y
         real(real64)            :: f
      end function rhs
      pure function exact(t) result(y)
         use iso_fortran_env, only : real64
         real(real64), intent(in) :: t
         real(real64)            :: y
      end function exact
   end interface
!-------------------------------------------------------------

   max_steps = n0 * 2_int64**i_max
   allocate(y_hist(max_steps+1))

   open(newunit=unit_stage, file='saidaEstagios.dat', status='replace', action='write')
   open(newunit=unit_conv,  file='ordem.dat',        status='replace', action='write')

!── 1. Single run (N = n0) – write full trajectory ───────────
   nsteps = n0
   call rk4_integrate(rhs, t0, y0, t_final, nsteps, y_hist(:nsteps+1))
   do i = 0, nsteps
      h = (t_final - t0) / real(nsteps, real64)      ! current time step
      write(unit_stage,'(1PE24.16,1X,1PE24.16)') &
           t0 + real(i,real64)*h,  y_hist(i+1)
   end do

!── 2. Convergence loop ──────────────────────────────────────
   trueZ = exact(t_final)

   do i = i_min, i_max
      nsteps = n0 * 2_int64**i
      call rk4_integrate(rhs, t0, y0, t_final, nsteps, y_hist(:nsteps+1))

      h   = (t_final - t0) / real(nsteps, real64)
      err = abs(y_hist(nsteps+1) - trueZ)

      write(unit_conv,'(1PE24.16,1X,1PE24.16)') h, err

      if (err < 1.0e-15_real64) exit   ! stop when round-off dominates
   end do

   close(unit_stage)
   close(unit_conv)

contains
   pure function rhs(t,y) result(f)
      real(real64), intent(in) :: t, y
      real(real64)             :: f
      f = (1.0_real64 - (4.0_real64/3.0_real64)*t) * y
   end function rhs

   pure function exact(t) result(y)
      real(real64), intent(in) :: t
      real(real64)             :: y
      y = exp( t - (2.0_real64/3.0_real64)*t*t )
   end function exact
end program main
