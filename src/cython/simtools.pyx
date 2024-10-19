from cython.view cimport array as cvarray


cdef extern void sim_direct(int * nedges,
                            int * e_to_v,
                            int * e_to_c,
                            int * dec_iters,
                            int * nsnr,
                            double * snr_dB_range,
                            int * bps,
                            int * nloops,
                            int * min_ferr,
                            double * ber,
                            double * fer) nogil


cpdef tuple ber_reconciliation_direct(int [:] e_to_v, int [:] e_to_c, int dec_iters,
                                      double [:] snr_dB_range, int bps,
                                      int nloops, int min_ferr):
    cdef double [:] ber
    cdef double [:] fer
    cdef int nedges, nsnr

    nedges = e_to_v.size
    nsnr = snr_dB_range.size
    ber = cvarray(shape=(nsnr,), itemsize=sizeof(double), format="d")
    fer = cvarray(shape=(nsnr,), itemsize=sizeof(double), format="d")
    
    sim_direct(&nedges,
               &e_to_v[0],
               &e_to_c[0],
               &dec_iters,
               &nsnr,
               &snr_dB_range[0],
               &bps,
               &nloops,
               &min_ferr,
               &ber[0],
               &fer[0])

    return (ber, fer)
