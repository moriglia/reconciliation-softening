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
module ldpc
  ! Syndrome-based soft LDPC decoding

  ! To get a decoder descriptor, call `decoder_from_edge_table` with the edge list as arguments,
  ! then feed the LLRs to the `decode` subroutine
  use iso_c_binding, only: c_int, c_double, c_bool
  implicit none
  
  private

  public :: edge_list_t, decoder_t, decoder_from_edge_table, decode, word_to_syndrome, c_decode_from_edge_list

  type :: edge_list_t
     integer(kind=c_int) :: N ! = 1
     integer(kind=c_int), allocatable :: edge_list(:)
  end type edge_list_t
  
  type, public :: decoder_t ! (vnum, cnum)
     integer :: vnum ! = 0
     integer :: cnum ! = 0
     integer :: edge_num ! = 0
     type(edge_list_t), allocatable :: v_to_e(:)
     type(edge_list_t), allocatable :: c_to_e(:)
     type(edge_list_t), allocatable :: c_to_v(:)
     type(edge_list_t), allocatable :: v_to_c(:)
  end type decoder_t

  ! type, public :: buffer_t
  !    integer :: N
  !    real(kind=c_double), allocatable :: F(:)
  !    real(kind=c_double), allocatable :: B(:)
  ! end type buffer_t

  interface ldpc_decoder_routines
     module subroutine decoder_from_edge_table(e_to_v, e_to_c, N, decoder)
       integer(kind=c_int), intent(in) :: N
       integer(kind=c_int), intent(in) :: e_to_v(N)
       integer(kind=c_int), intent(in) :: e_to_c(N)
       
       type(decoder_t), intent(out) :: decoder
     end subroutine decoder_from_edge_table
  end interface ldpc_decoder_routines

  interface ldpc_checknode_processor_routines
     ! module subroutine create_checknode_buffer(c_to_e, cnum, buffer_list)
     !   integer, intent(in) :: cnum
     !   type(edge_list_t), intent(in) :: c_to_e(cnum)
     !   type(buffer_t), intent(out) :: buffer_list(cnum)
     ! end subroutine create_checknode_buffer

     module subroutine process_checknode(edge_group, Ne, s, message_c_to_v, message_v_to_c)
       type(edge_list_t), intent(in) :: edge_group
       ! integer, intent(in) :: cnode_index
       integer, intent(in) :: Ne
       logical(kind=c_bool), intent(in) :: s
       real(kind=c_double), intent(out) :: message_c_to_v(Ne)
       real(kind=c_double), intent(in) :: message_v_to_c(Ne)
       ! type(buffer_t), intent(out) :: buffer_list(:)
     end subroutine process_checknode
  end interface ldpc_checknode_processor_routines

  interface ldpc_varnode_processor_routines
     pure module subroutine process_varnode(edge_group, vnode_index, Ne, message_c_to_v, message_v_to_c, llr, upd_llr, vnum)
       type(edge_list_t), intent(in) :: edge_group
       integer, intent(in) :: vnode_index
       integer, intent(in) :: Ne
       real(kind=c_double), intent(in) :: message_c_to_v(Ne)
       real(kind=c_double), intent(out) :: message_v_to_c(Ne)
       integer, intent(in) :: vnum
       real(kind=c_double), intent(in) :: llr(vnum)
       real(kind=c_double), intent(out) :: upd_llr(vnum)
     end subroutine process_varnode
  end interface ldpc_varnode_processor_routines

  interface ldpc_checker_routines
     module function check_llr(decoder, llr, synd) result (check)
       type(decoder_t), intent(in) :: decoder
       real(kind=c_double), intent(in) :: llr(:)
       logical(kind=c_bool), intent(in) :: synd(:)
       logical :: check
     end function check_llr
     
  end interface ldpc_checker_routines

  interface ldpc_syndrome_constructor_routines
     module subroutine word_to_syndrome(decoder, word, synd)
       type(decoder_t), intent(in) :: decoder
       logical(kind=c_bool), intent(in) :: word(decoder%vnum)
       logical(kind=c_bool), intent(out) :: synd(decoder%cnum)
     end subroutine word_to_syndrome
  end interface ldpc_syndrome_constructor_routines

  interface
    module subroutine c_decode_from_edge_list(e_to_v, e_to_c, Nedges, &
          llr, N, synd, Cnum, final_llr, iter_count) bind(c)
       integer(kind=c_int), intent(in) :: Nedges
       integer(kind=c_int), intent(in) :: e_to_v(Nedges)
       integer(kind=c_int), intent(in) :: e_to_c(Nedges)
       
       integer(kind=c_int), intent(in) :: N
       real(kind=c_double), intent(in) :: llr(N)
       integer(kind=c_int), intent(in) :: Cnum
       logical(kind=c_bool), intent(in) :: synd(Cnum)
       real(kind=c_double), intent(out) :: final_llr(N)
       integer(kind=c_int), intent(inout) :: iter_count
     end subroutine c_decode_from_edge_list
  end interface
  
  
contains
  
  subroutine decode(decoder, llr, synd, final_llr, iter_count)
    type(decoder_t), intent(in) :: decoder
    real(kind=c_double), intent(in) :: llr(decoder%vnum)
    logical(kind=c_bool), intent(in) :: synd(decoder%cnum)
    real(kind=c_double), intent(out) :: final_llr(decoder%vnum)
    integer(kind=c_int), intent(inout) :: iter_count
    
    real(kind=c_double) :: message_c_to_v(decoder%edge_num), message_v_to_c(decoder%edge_num)
    
    integer :: i, j

    if (check_llr(decoder, llr, synd)) then
       iter_count = 0
       final_llr(:) = llr(:)
       return
    end if

    message_c_to_v(:) = 0
    
    do i = 1, decoder%vnum
       call process_varnode(decoder%v_to_e(i), i, decoder%edge_num, message_c_to_v, message_v_to_c, llr, final_llr, decoder%vnum)
    end do

    do j = 1, iter_count
       do i = 1, decoder%cnum
          call process_checknode(decoder%c_to_e(i), decoder%edge_num, synd(i), message_c_to_v, message_v_to_c)
       end do

       do i = 1, decoder%vnum
          call process_varnode(decoder%v_to_e(i), i, decoder%edge_num, message_c_to_v, message_v_to_c, llr, final_llr, decoder%vnum)
       end do

       if (check_llr(decoder, llr, synd)) then
          iter_count = j
          return
       end if
    end do

    iter_count = -1
    
    
  end subroutine decode
end module ldpc
