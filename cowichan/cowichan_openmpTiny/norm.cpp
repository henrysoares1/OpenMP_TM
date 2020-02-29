/**
 * \file cowichan_openmp/norm.cpp
 * \brief OpenMP norm implementation.
 * \see CowichanOpenMP::norm
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <signal.h>
#include "cowichan_openmp.hpp"
#include "tinySTM.h"

namespace cowichan_openmp
{

/**
 * Find min/max x/y coordinates.
 * \param points point vector.
 * \param n number of points.
 * \param minPoint min x/y values.
 * \param maxPoint max x/y values.
 */
void findMinMax(PointVector points, index_t n, Point* minPoint, Point* maxPoint);

}

/*****************************************************************************/

void CowichanOpenMP::norm (PointVector pointsIn, PointVector pointsOut)
{
  Point minPoint, maxPoint;
  real sclX, sclY; // scaling factors
  index_t i;

  // compute scaling factors
  STM_GLOBAL_INITIALIZE();
  findMinMax(pointsIn, n, &minPoint, &maxPoint);
  //STM_PRINT_STATISTICS();
  STM_GLOBAL_FINALIZE();

  sclX = (real)((maxPoint.x == minPoint.x) ?
      0.0 : 1.0 / (maxPoint.x - minPoint.x));
  sclY = (real)((maxPoint.y == minPoint.y) ?
  	
      0.0 : 1.0 / (maxPoint.y - minPoint.y));

  // scale
#pragma omp parallel for schedule(static)
  for (i = 0; i < n; i++) {
    pointsOut[i].x = sclX * (pointsIn[i].x - minPoint.x);
    pointsOut[i].y = sclY * (pointsIn[i].y - minPoint.y);
  }
}

/*****************************************************************************/

namespace cowichan_openmp
{

void findMinMax(PointVector points, index_t n, Point* minPoint, Point* maxPoint) {

 minPoint->x = points[0].x;
 minPoint->y = points[0].y;
 maxPoint->x = points[0].x;
 maxPoint->y = points[0].y;
 #pragma omp parallel
 {
 	STM_INITIALIZE_THREAD();
	 //index_t thread_num = omp_get_thread_num();
 #pragma omp for schedule(static)	
		 for (index_t i = 0; i < n; i++) {
                        STM_START_TRANSACTION();
				
			if (stm_load_float(&minPoint->x) > points[i].x) {
				stm_store_float(&minPoint->x, points[i].x);
			}			
			if (stm_load_float(&minPoint->y) > points[i].y) {
				stm_store_float(&minPoint->y, points[i].y);
			}
			if (stm_load_float(&maxPoint->x) < points[i].x) {
				stm_store_float(&maxPoint->x, points[i].x);
			}
			if (stm_load_float(&maxPoint->y) < points[i].y) {
				stm_store_float(&maxPoint->y, points[i].y);
			}
			STM_TRY_COMMIT();
		}
		STM_FINALIZE_THREAD();
  }
//printf("maxPoint x:%lf minPoint x:%lf maxPoint y:%lf minPoint y:%lf \n", maxPoint->x, minPoint->x, maxPoint->y, minPoint->y);
}

}