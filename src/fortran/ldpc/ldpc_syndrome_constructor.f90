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
submodule (ldpc) ldpc_syndrome_constructor

contains
  module subroutine word_to_syndrome(decoder, word, synd)
    type(decoder_t), intent(in) :: decoder
    logical(kind=c_bool), intent(in) :: word(decoder%vnum)
    logical(kind=c_bool), intent(out) :: synd(decoder%cnum)

    integer :: i, j, c
    
    synd(:) = .false.

    do i = 1, decoder%vnum
       if (word(i)) then
          do j = 1, decoder%v_to_e(i)%N
             c = decoder%v_to_c(i)%edge_list(j)
             synd(c) = .not. synd(c)
          end do
       end if
    end do
  end subroutine word_to_syndrome
end submodule ldpc_syndrome_constructor
