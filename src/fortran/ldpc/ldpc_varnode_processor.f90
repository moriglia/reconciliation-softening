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
submodule (ldpc) ldpc_varnode_processor
  ! Message passing at Variable node
contains

  pure module subroutine process_varnode(edge_group, vnode_index, Ne, message_c_to_v, message_v_to_c, llr, upd_llr)
    type(edge_list_t), intent(in) :: edge_group
    integer, intent(in) :: vnode_index
    integer, intent(in) :: Ne
    real(kind=c_double), intent(in) :: message_c_to_v(Ne)
    real(kind=c_double), intent(out) :: message_v_to_c(Ne)
    real(kind=c_double), intent(in) :: llr(:)
    real(kind=c_double), intent(out) :: upd_llr(:)

    type(edge_list_t), pointer :: edge_list_ptr
    integer :: N_cn ! number of edges connected to THIS vnode

    integer :: i
    
    !    edge_list_ptr => v_to_e(vnode_index)

    N_cn = edge_group%N

    upd_llr(vnode_index) = llr(vnode_index)
    do i = 1, N_cn
       upd_llr(vnode_index) = upd_llr(vnode_index) + message_c_to_v(edge_group%edge_list(i))
    end do

    do i = 1, N_cn
       message_v_to_c(edge_group%edge_list(i)) = upd_llr(vnode_index)-message_c_to_v(edge_group%edge_list(i))
    end do
    
  end subroutine process_varnode


end submodule ldpc_varnode_processor
