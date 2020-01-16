/**
 * \file cowichan_openmp/vecdiff.cpp
 * \brief OpenMP vecdiff implementation.
 * \see CowichanOpenMP::vecdiff
 */

#include "cowichan_openmp.hpp"

real CowichanOpenMP::vecdiff (Vector actual, Vector computed)
{
  index_t i;
  real diff;
  real maxDiff;
  maxDiff = (real)fabs((double)(actual[0] - computed[0]));


#pragma omp parallel private(diff)
  {
	 //index_t thread_num = omp_get_thread_num();
	#pragma omp for schedule(static)
		for (i = 1; i < n; i++) { 
			diff = (real)fabs((double)(actual[i] - computed[i]));
			__transaction_atomic { 
			if (maxDiff < diff) {
				maxDiff = diff;
			  }
			}
		}
  }
  //printf("maxDiff =%f \n", maxDiff);
  return maxDiff;
}

