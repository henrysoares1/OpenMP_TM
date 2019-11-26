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
#include "tinySTM/include/stm.h"
#include "tinySTM/include/mod_mem.h"
#include "tinySTM/include/mod_stats.h"

#define STM_STARTUP()     \
        stm_init();       \
        mod_mem_init(0);  \
        mod_stats_init() 

#define STM_START()  \
        sigjmp_buf * checkPoint = stm_start((stm_tx_attr_t) {0}); \
        sigsetjmp(*checkPoint, 0); 

#define STM_READ(endereco)          stm_load((stm_word_t *)endereco)
#define STM_READ_POINT(	endereco)     (float) STM_READ(endereco)
#define STM_STORE(endereco, valor)  stm_store((stm_word_t *)endereco, (stm_word_t)valor)
#define STM_STATS()     \
    unsigned long stat;     \
    if (stm_get_global_stats("global_nb_commits", &stat) != 0) \
      printf("#commits    : %lu\n", stat);   \
    if (stm_get_global_stats("global_nb_aborts", &stat) != 0) \
      printf("#aborts     : %lu\n", stat)

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
  STM_STARTUP();
  findMinMax(pointsIn, n, &minPoint, &maxPoint);
  STM_STATS();
  stm_exit();

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
 stm_init_thread();
 #pragma omp parallel
 {
	 //index_t thread_num = omp_get_thread_num();
 #pragma omp for schedule(static)	
		 for (index_t i = 0; i < n; i++) {
			//__transaction_atomic {
			STM_START();	
			if (STM_READ_POINT(&minPoint->x) > points[i].x) {
				STM_STORE(&minPoint->x, points[i].x);
			}			
			if (STM_READ_POINT(&minPoint->y) > points[i].y) {
				STM_STORE(&minPoint->y, points[i].y);
			}
			if (STM_READ_POINT(&maxPoint->x) < points[i].x) {
				STM_STORE(&maxPoint->x, points[i].x);
			}
			if (STM_READ_POINT(&maxPoint->y) < points[i].y) {
				STM_STORE(&maxPoint->y, points[i].y);
			}
			stm_commit();
		}
		stm_exit_thread();
  }
printf("maxPoint x:%lf minPoint x:%lf maxPoint y:%lf minPoint y:%lf \n", maxPoint->x, minPoint->x, maxPoint->y, minPoint->y);
}

}


