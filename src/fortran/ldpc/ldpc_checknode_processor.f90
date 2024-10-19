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
submodule (ldpc) ldpc_checknode_processor
  ! Message passing at variable checknode
contains

  ! module subroutine create_checknode_buffer(c_to_e, cnum, buffer_list)
  !   integer, intent(in) :: cnum
  !   type(edge_list_t), intent(in) :: c_to_e(cnum)
  !   type(buffer_t), intent(out) :: buffer_list(cnum)

  !   ! type(buffer_t), pointer :: el_ptr

  !   integer :: i, N

  !   do i = 1, cnum
  !      ! el_ptr => buffer_list(i)
  !      N = c_to_e(i)%N - 1
  !      buffer_list(i)%N = N
  !      allocate(buffer_list(i)%F(N))
  !      allocate(buffer_list(i)%B(N))
  !   end do

  !   print *, buffer_list(1)%F(1)
       
  ! end subroutine create_checknode_buffer

  
  module subroutine process_checknode(edge_group, Ne, s, message_c_to_v, message_v_to_c)
    type(edge_list_t),  intent(in) :: edge_group
    ! integer, intent(in) :: cnode_index
    integer, intent(in) :: Ne
    logical(kind=c_bool), intent(in) :: s
    real(kind=c_double), intent(out) :: message_c_to_v(Ne)
    real(kind=c_double), intent(in) :: message_v_to_c(Ne)

    integer :: N_buff
    real(kind=c_double) :: F(edge_group%N-1)
    real(kind=c_double) :: B(edge_group%N-1)

    integer :: i, j
    real :: prefactor

    N_buff = edge_group%N-1

    ! The usage of 2 buffers F (forward) and B (backward)
    ! Reduces the complexity of the message passing task
    ! from d_c * (d_c - 1) to 3 * (d_c - 2), with d_c the
    ! degree of the current checknode

    F(1)      = message_v_to_c(edge_group%edge_list(1))
    B(N_buff) = message_v_to_c(edge_group%edge_list(edge_group%N))
    j = 1
    do i = 2, N_buff
       F(i) = box_plus(F(j), message_v_to_c(edge_group%edge_list(i)))
       j = i
    end do

    j = N_buff
    do i = N_buff-1, 1, -1
       B(i) = box_plus(B(j), message_v_to_c(edge_group%edge_list(i)))
       j = i
    end do

    if (s) then
       prefactor = -1.0
    else
       prefactor = 1.0
    end if
    
    message_c_to_v(edge_group%edge_list(1)) = prefactor * B(1)
    message_c_to_v(edge_group%edge_list(edge_group%N)) = prefactor * F(N_buff)

    j = 1
    do i = 2, N_buff
       message_c_to_v(edge_group%edge_list(i)) = prefactor * box_plus(F(j), B(i))
       j = i
    end do
    
  end subroutine process_checknode


  pure function box_plus(a, b) result (c)
    real(kind=c_double), intent(in) :: a
    real(kind=c_double), intent(in) :: b
    real(kind=c_double) :: c
    
    c = dsign(min(dabs(a), dabs(b)), a * b) + log(1 + exp(-dabs(a+b))) - log(1 + exp(-dabs(a-b)))
  end function box_plus
end submodule ldpc_checknode_processor
