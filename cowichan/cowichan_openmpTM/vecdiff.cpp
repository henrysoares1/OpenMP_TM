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


#pragma omp parallel private(diff, maxDiff)
  {
	#pragma omp for schedule(static)
		for (i = 1; i < n; i++) {
			__transaction_atomic {
			diff = (real)fabs((double)(actual[i] - computed[i]));
			if (maxDiff < diff) {
				maxDiff = diff;
			  }
			}
		}
  }
  return maxDiff;
}

