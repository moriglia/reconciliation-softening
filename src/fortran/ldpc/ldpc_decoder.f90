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
submodule (ldpc) ldpc_decoder
  ! Build decoder descriptor from edge list
contains

  module subroutine decoder_from_edge_table(e_to_v, e_to_c, N, decoder)
    integer(kind=c_int), intent(in) :: N
    integer(kind=c_int), intent(in) :: e_to_v(N)
    integer(kind=c_int), intent(in) :: e_to_c(N)

    type(decoder_t), intent(out) :: decoder

    integer :: i, j
    
    ! assumes v/c-node IDs start from 0 in the given array
    decoder%vnum = maxval(e_to_v) + 1
    decoder%cnum = maxval(e_to_c) + 1

    decoder%edge_num = N
    
    call invert_table(e_to_v, N, decoder%v_to_e)
    call invert_table(e_to_c, N, decoder%c_to_e)

    allocate(decoder%c_to_v(decoder%cnum))
    allocate(decoder%v_to_c(decoder%vnum))

    do i = 1, decoder%cnum
       decoder%c_to_v(i)%N = decoder%c_to_e(i)%N
       allocate(decoder%c_to_v(i)%edge_list(decoder%c_to_v(i)%N))
       do j = 1, decoder%c_to_v(i)%N
          decoder%c_to_v(i)%edge_list(j) = e_to_v(decoder%c_to_e(i)%edge_list(j)) + 1
       end do
    end do

    do i = 1, decoder%cnum
       decoder%v_to_c(i)%N = decoder%v_to_e(i)%N
       allocate(decoder%v_to_c(i)%edge_list(decoder%v_to_c(i)%N))
       do j = 1, decoder%v_to_c(i)%N
          decoder%v_to_c(i)%edge_list(j) = e_to_c(decoder%c_to_e(i)%edge_list(j)) + 1
       end do
    end do
  end subroutine decoder_from_edge_table

  
  subroutine invert_table(e_to_x, N, x_to_e)
    integer, intent(in) :: N
    integer, intent(in) :: e_to_x(N)

    type(edge_list_t), allocatable, intent(inout) :: x_to_e(:)
    
    integer :: i, xnum
    integer, allocatable :: last_index(:)


    xnum = maxval(e_to_x) + 1

    allocate(x_to_e(xnum))
    allocate(last_index(xnum))

    last_index(:) = 0

    do i = 1, N
       last_index(e_to_x(i)+1) = last_index(e_to_x(i)+1) + 1
    end do

    do i = 1, xnum
       x_to_e(i)%N = last_index(i)
       allocate(x_to_e(i)%edge_list(last_index(i)))
    end do

    do i = 1, N
       x_to_e(e_to_x(i)+1)%edge_list(last_index(e_to_x(i)+1)) = i
       last_index(e_to_x(i)+1) = last_index(e_to_x(i)+1) - 1
    end do
    
    deallocate(last_index)

  end subroutine invert_table


end submodule ldpc_decoder
