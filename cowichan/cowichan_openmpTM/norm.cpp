/**
 * \file cowichan_openmp/norm.cpp
 * \brief OpenMP norm implementation.
 * \see CowichanOpenMP::norm
 */

#include "cowichan_openmp.hpp"

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
  findMinMax(pointsIn, n, &minPoint, &maxPoint);

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
	 //index_t thread_num = omp_get_thread_num();
 #pragma omp for schedule(static)	
		 for (index_t i = 0; i < n; i++) {
			__transaction_atomic {	
				if (minPoint->x > points[i].x) {
				  minPoint->x = points[i].x;
				}			
				if (minPoint->y > points[i].y) {
				  minPoint->y = points[i].y;
				}
				if (maxPoint->x < points[i].x) {
				  maxPoint->x = points[i].x;
				}
				if (maxPoint->y < points[i].y) {
				  maxPoint->y = points[i].y;
				}
	  }
	}
  }
//printf("maxPoint x:%lf minPoint x:%lf maxPoint y:%lf minPoint y:%lf \n", maxPoint->x, minPoint->x, maxPoint->y, minPoint->y);
}

}


