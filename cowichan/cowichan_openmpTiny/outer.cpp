/**
 * \file cowichan_openmp/outer.cpp
 * \brief OpenMP outer implementation.
 * \see CowichanOpenMP::outer
 */

#include "cowichan_openmp.hpp"
#include "tinySTM.h"

void CowichanOpenMP::outer (PointVector points, Matrix matrix, Vector vector){
  Point zeroPoint(0.0, 0.0);
  real d; // distance between points
  real dMax; // maximum distance
  index_t r, c; // loop indices

  // all elements except matrix diagonal
  STM_GLOBAL_INITIALIZE();
#pragma omp parallel private(d)
  {
    STM_INITIALIZE_THREAD();
	  d = 0.0;
#pragma omp for schedule(guided)
    for (r = 0; r < n; r++) {

      vector[r] = Point::distance (points[r], zeroPoint);
#pragma omp parallel for schedule(static)
      for (c = 0; c < r; c++) {
        STM_START_TRANSACTION();
        d = Point::distance (points[r], points[c]);
			if (d > stm_load_float(&dMax)) {
        stm_store_float(&dMax, d);
			   //dMax = d;
			}
      
        MATRIX_SQUARE(matrix, r, c) = MATRIX_SQUARE(matrix, c, r) = d;
        STM_TRY_COMMIT();
      }
    
	}
  STM_FINALIZE_THREAD();
  }
  //printf("dMax: %f \n", dMax);
  //STM_PRINT_STATISTICS();
  STM_GLOBAL_FINALIZE();
  // matrix diagonal
  dMax *= n;
#pragma omp parallel for schedule(static)
  for (r = 0; r < n; r++) {
    MATRIX_SQUARE(matrix, r, r) = dMax;
  }
}

