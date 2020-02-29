/**
 * \file cowichan_openmp/vecdiff.cpp
 * \brief OpenMP vecdiff implementation.
 * \see CowichanOpenMP::vecdiff
 */

#include "cowichan_openmp.hpp"
#include "tinySTM.h"

real CowichanOpenMP::vecdiff (Vector actual, Vector computed)
{
  index_t i;
  real diff;
  real maxDiff;
  maxDiff = (real)fabs((double)(actual[0] - computed[0]));

  STM_GLOBAL_INITIALIZE();
  
#pragma omp parallel private(diff)
  {
  	STM_INITIALIZE_THREAD();
	 //index_t thread_num = omp_get_thread_num();
	#pragma omp for schedule(static)
		for (i = 1; i < n; i++) { 
                      	STM_START_TRANSACTION();
			diff = (real)fabs((double)(actual[i] - computed[i]));
			if (stm_load_float(&maxDiff) < diff) {
				stm_store_float(&maxDiff, diff);
				//maxDiff = diff;
			  }
			STM_TRY_COMMIT();
		}
        STM_FINALIZE_THREAD();
  }

  //STM_PRINT_STATISTICS();
  STM_GLOBAL_FINALIZE();
  //printf("maxDiff =%f \n", maxDiff);
  return maxDiff;
}

