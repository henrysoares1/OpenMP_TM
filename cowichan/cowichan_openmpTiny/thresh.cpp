/**
 * \file cowichan_openmp/thresh.cpp
 * \brief OpenMP thresh implementation.
 * \see CowichanOpenMP::thresh
 */

#include "cowichan_openmp.hpp"
#include "tinySTM.h"

/**
 * Works only on positive input.
 */
void CowichanOpenMP::thresh(IntMatrix matrix, BoolMatrix mask) {

  index_t* hist = NULL; // histogram
  index_t i;
  index_t newValue;
  index_t r, c;
  INT_TYPE vMax; // max value in matrix
  index_t retain; // selection

  // find max value in matrix
  vMax = 0;
  STM_GLOBAL_INITIALIZE();
#pragma omp parallel
	{
    STM_INITIALIZE_THREAD();
	#pragma omp parallel for schedule(static)
		for (r = 0; r < nr; r++) {
			//__transaction_relaxed {
		#pragma omp parallel for schedule(static)
		for (c = 0; c < nc; c++) {
		   STM_START_TRANSACTION();
			if (stm_load_u32(&vMax) < MATRIX_RECT(matrix, r, c)) {
				stm_store_u32(&vMax,MATRIX_RECT(matrix, r, c));
					}
        STM_TRY_COMMIT();
				}
			//}
		}
  STM_FINALIZE_THREAD();
	}
	//printf("vMax: %d \n", vMax);
  // initialize histogram
  try {
    hist = NEW_VECTOR_SZ(index_t, vMax + 1);
  }
  catch (...) {out_of_memory();}

#pragma omp parallel for schedule(static)
  for (i = 0; i <= (index_t)vMax; i++) {
    hist[i] = 0;
  }

  // count
#pragma omp parallel
  {
    STM_INITIALIZE_THREAD();
	#pragma omp for schedule(static)
		for (r = 0; r < nr; r++) {
		
			#pragma omp parallel for schedule(static) private(newValue)
				  for (c = 0; c < nc; c++) {
            STM_START_TRANSACTION();
            newValue = stm_load_long(&hist[MATRIX_RECT(matrix, r, c)]) + 1;
            stm_store_long(&hist[MATRIX_RECT(matrix, r, c)], newValue);
            //TM_STORE_LONG(&hist[MATRIX_RECT(matrix, r, c)] , hist[MATRIX_RECT(matrix, r, c)]++)
				    //__transaction_atomic{hist[MATRIX_RECT(matrix, r, c)]++;}
            STM_TRY_COMMIT();
				  }
		}
  STM_FINALIZE_THREAD();
  }

  // include
  retain = (index_t)(threshPercent * nc * nr);
  for (i = vMax; ((i >= 0) && (retain > 0)); i--) {
    retain -= hist[i];
    //printf("hist: %ld \n", hist[i]);
  }
  //printf("retain: %ld \n", retain);
  retain = i;

  delete [] hist;

  // threshold
#pragma omp parallel for schedule(static)
  for (r = 0; r < nr; r++) {
#pragma omp parallel for schedule(static)
    for (c = 0; c < nc; c++) {
      MATRIX_RECT(mask, r, c) = ((index_t)MATRIX_RECT(matrix, r, c)) > retain;
    }
  }
//STM_PRINT_STATISTICS();
STM_GLOBAL_FINALIZE();
}

