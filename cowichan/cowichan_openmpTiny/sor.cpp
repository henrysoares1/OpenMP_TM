/**
 * \file cowichan_openmp/sor.cpp
 * \brief OpenMP sor implementation.
 * \see CowichanOpenMP::sor
 */

#include "cowichan_openmp.hpp"
#include "tinySTM.h"

void CowichanOpenMP::sor (Matrix matrix, Vector target, Vector solution)
{
  index_t r, c;
  index_t t;
  real sum;
  real oldSolution;
  real diff, maxDiff;

  // initialize
  for (r = 0; r < n; r++) {
    solution[r] = 1.0;
  }
  maxDiff = (real)(2 * SOR_TOLERANCE); // to forestall early exit
  STM_GLOBAL_INITIALIZE();
  for (t = 0; (t < SOR_MAX_ITERS) && (maxDiff >= SOR_TOLERANCE); t++) {
	//printf("maxDiff: %f \n", maxDiff);
    maxDiff = 0.0;

    

#pragma omp parallel private(oldSolution, diff, sum, c)
    {
    	STM_INITIALIZE_THREAD();
#pragma omp for schedule(static)
      for (r = 0; r < n; r++) {
			// compute sum
			sum = 0.0;
			for (c = 0; c < r; c++) {
			  sum += MATRIX_SQUARE(matrix, r, c) * solution[c];
			}
			for (c = r + 1; c < n; c++) {
			  sum += MATRIX_SQUARE(matrix, r, c) * solution[c];
			}

			// calculate new solution
			oldSolution = solution[r];
			solution[r] = (real)((1.0 - SOR_OMEGA) * oldSolution + SOR_OMEGA *
				(target[r] - sum) / MATRIX_SQUARE(matrix, r, r));

			// compute difference
			diff = (real)fabs((double)(oldSolution - solution[r]));
			STM_START_TRANSACTION();
				if (diff > stm_load_float(&maxDiff)){
					stm_store_float(&maxDiff, diff);
					//maxDiff = diff;
				}
				STM_TRY_COMMIT();
			
		}
		STM_FINALIZE_THREAD();
	  }
  }
  //STM_PRINT_STATISTICS();
  STM_GLOBAL_FINALIZE();

}

