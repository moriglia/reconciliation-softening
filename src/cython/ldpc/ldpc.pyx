from cython.view cimport array as cvarray

cdef extern void c_decode_from_edge_list(int           * e_to_v,
                                         int           * e_to_c,
                                         int           * Nedges,
                                         double        * llr,
                                         int           * N,
                                         unsigned char * synd,
                                         int           * Cnum,
                                         double        * final_llr,
                                         int           * iter_count)


cpdef double [:] decode_from_edge_list(int           [:] e_to_v,
                                       int           [:] e_to_c,
                                       double        [:] llr,
                                       unsigned char [:] synd,
                                       int           iter_count):
    cdef double [:] final_llr
    cdef int N, Cnum, Nedges

    N = llr.size
    Cnum = synd.size
    Nedges = e_to_v.size

    final_llr = cvarray(shape=(N,), itemsize=sizeof(double), format="d")
    
    c_decode_from_edge_list(&e_to_v[0],   
                            &e_to_c[0],   
                            &Nedges,   
                            &llr[0],      
                            &N,        
                            &synd[0],     
                            &Cnum,     
                            &final_llr[0],
                            &iter_count)

    return final_llr
