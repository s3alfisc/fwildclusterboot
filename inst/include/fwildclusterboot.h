#include <RcppArmadillo.h>
#define NDEBUG 
#include <RcppEigen.h>
#ifdef _OPENMP
#include <omp.h>
#else
#define omp_get_max_threads() 0
#define EIGEN_DONT_PARALLELIZE
#endif