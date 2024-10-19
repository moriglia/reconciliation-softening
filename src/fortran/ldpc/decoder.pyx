from cython.view cimport array as cvarray

cdef extern void __ldpc_MOD_decode(long *a, long* b, long* n, long* res)



cpdef long [:] test():
    cdef long [:] a
    cdef long [:] b
    cdef long [:] r

    cdef long n = 1000
    cdef long i
    
    a = cvarray(shape=(n,), itemsize=sizeof(long), format="l")
    b = cvarray(shape=(n,), itemsize=sizeof(long), format="l")
    r = cvarray(shape=(n,), itemsize=sizeof(long), format="l")

    for i in range(n):
        a[i] = (n-i) // 10
        b[i] = (n-i)>>4


    __ldpc_MOD_decode(&a[0],
                      &b[0],
                      &n   ,
                      &r[0])


    for i in range(n):
        if (r[i] != a[i] + b[i]**2):
            print(f"Error at index {i}: a = {a[i]}, b = {b[i]}, r = {r[i]}")

    print(a[0], b[0], r[0])
    print("End program")
    return r


