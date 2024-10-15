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
submodule (simtools) simtools_direct_reconciliation
  use ldpc, only : decoder_t, buffer_t, decoder_from_edge_table, decode, create_checknode_buffer, word_to_syndrome
  use stdlib_stats_distribution_uniform, only: rvs_uniform
  use stdlib_stats_distribution_normal, only: rvs_normal
  implicit none
  
  real, parameter :: step = 2
  
contains
  module subroutine sim_direct(&
       nedges, e_to_v, e_to_c, dec_iters, &
       nsnr, snr_dB_range, nloops, bps, min_ferr,&
       ber, fer) bind(C)
    integer(kind=c_int), intent(in) :: nedges, nsnr, nloops, dec_iters, bps, min_ferr
    integer(kind=c_int), dimension(nedges), intent(in) :: e_to_c, e_to_v
    real(kind=c_double), dimension(nsnr), intent(in) :: snr_dB_range
    real(kind=c_double), dimension(nsnr), intent(out) :: ber, fer

    ! OTHER PARAMETERS:
    type(decoder_t) :: decoder
    integer(kind=c_int), allocatable :: count_berr(:)[:]
    integer(kind=c_int), allocatable :: count_ferr(:)[:]
    integer(kind=c_int), allocatable :: count_words(:)[:]
    logical, allocatable :: done[:]

    integer :: M, N, K, N_symb
    real(kind=c_double)    :: const_base, Es, N0, twoN0
    real :: sigma_noise
    
    integer :: i_snr, i_sim, i_bit, i_x, i
    integer, allocatable :: x_i(:)
    real(kind=c_double), allocatable :: y(:), constellation(:)
    real(kind=c_double), allocatable :: llr(:), final_llr(:)
    real(kind=c_double), allocatable :: bufferFrac(:,:)
    real(kind=c_double), allocatable :: message_c_to_v(:), message_v_to_c(:)
    type(buffer_t), allocatable :: buffer_list(:)
    logical(kind=c_bool), allocatable :: synd(:)
    logical(kind=c_bool), allocatable :: word(:)
    integer :: new_errors, d_iter
    
    ! INITIALIZE ERROR COUNTERS
    if (this_image() == 1) then
       allocate(count_berr(nsnr)[*])
       allocate(count_ferr(nsnr)[*])
       allocate(count_words(nsnr)[*])
       count_berr(:) = 0
       count_berr(:) = 0
       count_words(:) = 0
    end if
    allocate(done[*])
    done = .false.
    sync all
    
    ! INITIALIZE DECODER
    call decoder_from_edge_table(e_to_v, e_to_c, nedges, decoder)

    ! INITIALIZE MODULATION AND CODING PARAMETERS
    M = ishft(1, bps)
    N = decoder%vnum
    K = N - decoder%cnum
    N_symb = N / M
    d_iter = dec_iters

    Es = 0
    const_base = (1-M)/2 * step
    do i = 0, ishft(M, -1) - 1 ! 0, ..., M/2-1
       Es = Es + (const_base + i*step)**2
    end do
    Es = 2*Es
    constellation = [(const_base + i*step, i=0, M-1)]

    ! INITIALIZE BUFFERS
    allocate(bufferFrac(2, bps))
    allocate(message_c_to_v(decoder%edge_num))
    allocate(message_v_to_c(decoder%edge_num))
    call create_checknode_buffer(decoder%c_to_e, decoder%cnum, buffer_list)
    allocate(word(0:N-1))
    allocate(x_i(0:N_symb-1))
    
    snrloop : do i_snr = 1, nsnr
       twoN0 = Es*(10**(-snr_dB_range(i_snr)/10))
       N0 = twoN0/2
       sigma_noise = sqrt(N0)
       simloop : do i_sim = 1, nloops
          x_i = rvs_uniform(0, M-1, N_symb)
          y = rvs_normal(0.0, sigma_noise, N_symb)
          y = y + const_base + step*x_i

          do i_x = 0, N_symb-1
             do i_bit = 0, bps-1
                if (iand((x_i(i_x)*(x_i(i_x)+1)), 3) /= 0) then
                   word(i_x*bps + i_bit) = .true.
                else
                   word(i_x*bps + i_bit) = .false.
                end if
                x_i(i_x) = ishft(x_i(i_x), -1)
             end do
          end do

          call word_to_syndrome(decoder, word, synd)
          
          call y_to_llr(N_symb, N, bps, M,  y, twoN0, constellation, bufferFrac, llr)

          call decode(decoder, llr, synd, final_llr, &
               d_iter, message_c_to_v, message_v_to_c, &
               buffer_list)

          new_errors = 0
          do i = 0, K-1
             if (word(i) .xor. (final_llr(i)<0)) then
                new_errors = new_errors + 1
             end if
          end do
          
          if (new_errors > 0) then
             critical
               count_berr(i_snr)[1] = count_berr(i_snr)[1] + new_errors
               count_ferr(i_snr)[1] = count_ferr(i_snr)[1] + 1
             end critical
          end if
          
          critical
            count_words(i_snr)[1] = count_words(i_snr)[1] + 1
          end critical
          if ((count_ferr(i_snr)[1] > min_ferr) .and. ( count_words(i_snr)[1] > nloops/10)) then
             exit simloop
          end if
       end do simloop
    end do snrloop

    done = .true.
 
    if (this_image() == 1) then
       do i = 2, num_images()
          do while(.not. done[i])
             call sleep(1)
          end do
       end do

       ber = count_berr/count_words/K
       fer = count_ferr/count_words
       
    end if    
    
  end subroutine sim_direct

  module subroutine y_to_llr(N_symb, N, bps, M,  y, twoSigmaSquare, constellation, bufferFrac, llr)
    integer, intent(in) :: N_symb, N, bps, M ! these are redundant, passed for avoiding re-evaluation
    real(kind=c_double), intent(in), dimension(0:N_symb-1) :: y
    real(kind=c_double), intent(in) :: twoSigmaSquare
    real(kind=c_double), intent(in) :: constellation(M)
    real(kind=c_double), intent(out) :: bufferFrac(2,bps)
    real(kind=c_double), intent(out), dimension(0:N-1) :: llr
    
    integer :: i_sym, i_bit, i_y
    integer :: mod_index
    real :: addendum


    do i_y = 0, N_symb-1
       bufferFrac(:,:) = 0
       do i_sym = 1, M
          addendum = exp(-((y(i_y) - constellation(i_sym))**2)/twoSigmaSquare)
          mod_index = i_sym - 1
          do i_bit = 1, bps
             if (iand((mod_index*(mod_index+1)), b'11') /= 0) then
                bufferFrac(2, i_bit) = bufferFrac(2, i_bit) + addendum
             else
                bufferFrac(1, i_bit) = bufferFrac(1, i_bit) + addendum
             end if
             mod_index = ishft(mod_index, -1)
          end do
       end do
       llr(i_y*bps : (i_y+1)*bps - 1) = log(bufferFrac(1,:)) - log(bufferFrac(2,:))
    end do
  end subroutine y_to_llr
end submodule simtools_direct_reconciliation
