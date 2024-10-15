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
submodule (ldpc) ldpc_checker
  ! Checking if the LLRs satisfy the parity of the syndrome
contains

  module function check_llr_node(c_to_v, cnode_index, llr, s) result(check)
    type(edge_list_t), target, intent(in) :: c_to_v(:)
    integer, intent(in) :: cnode_index
    real(kind=c_double), intent(in) :: llr(:)
    logical(kind=c_bool), intent(in) :: s
    logical :: check
    
    integer :: i
    type(edge_list_t), pointer :: edge_list_ptr

    check = s
    edge_list_ptr => c_to_v(cnode_index)

    do i = 1, edge_list_ptr%N
       if (llr(edge_list_ptr%edge_list(i)) .lt. 0) then
          check = .not. check
       end if
    end do
  end function check_llr_node
  
  module function check_llr(decoder, llr, synd) result (check)
    type(decoder_t), intent(in) :: decoder
    real(kind=c_double), intent(in) :: llr(:)
    logical(kind=c_bool), intent(in) :: synd(:)
    logical :: check


    integer :: i

    check = .true.
    do i = 1, decoder%cnum
       if (.not. check_llr_node(decoder%c_to_v, i, llr, synd(i))) then
          check = .false.
          exit
       end if
    end do
    
  end function check_llr
end submodule ldpc_checker
