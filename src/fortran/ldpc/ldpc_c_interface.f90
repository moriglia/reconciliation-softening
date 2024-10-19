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
submodule (ldpc) ldpc_c_interface

contains
  module subroutine c_decode_from_edge_list(e_to_v, e_to_c, Nedges, &
       llr, N, synd, Cnum, final_llr, iter_count) bind(c)
    integer(kind=c_int), intent(in) :: Nedges
    integer(kind=c_int), intent(in) :: e_to_v(Nedges)
    integer(kind=c_int), intent(in) :: e_to_c(Nedges)
    
    real(kind=c_double), intent(in) :: llr(N)
    integer(kind=c_int), intent(in) :: N
    logical(kind=c_bool), intent(in) :: synd(Cnum)
    integer(kind=c_int), intent(in) :: Cnum
    real(kind=c_double), intent(out) :: final_llr(N)
    integer(kind=c_int), intent(inout) :: iter_count

    

    type(decoder_t) :: decoder

    call decoder_from_edge_table(e_to_v, e_to_c, Nedges, decoder)
    call decode(decoder, llr, synd, final_llr, iter_count)
  end subroutine c_decode_from_edge_list

end submodule ldpc_c_interface

    
