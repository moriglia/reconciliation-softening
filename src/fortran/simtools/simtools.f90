! SPDX-License-Identifier: GPL-3.0-or-later
! Copyright (C) 2024  Marco Origlia

!    This program is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.

!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.

!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see <https://www.gnu.org/licenses/>.
module simtools
  use iso_c_binding, only: c_int, c_long, c_bool, c_double, c_float
  implicit none

  private
  public :: sim_direct

  interface sim_direct_reconciliation
     module subroutine sim_direct(&
          nedges, e_to_v, e_to_c, dec_iters, &
          nsnr, snr_dB_range, nloops, bps, min_ferr,&
          ber, fer) bind(C)
       integer(kind=c_int), intent(in) :: nedges, nsnr, nloops, dec_iters, bps, min_ferr
       integer(kind=c_int), dimension(nedges), intent(in) :: e_to_c, e_to_v
       real(kind=c_double), dimension(nsnr), intent(in) :: snr_dB_range
       real(kind=c_double), dimension(nsnr), intent(out) :: ber, fer
       
     end subroutine sim_direct
  end interface sim_direct_reconciliation
  
end module simtools
