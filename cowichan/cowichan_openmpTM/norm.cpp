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
 #pragma omp for schedule(static)	
		 for (index_t i = 0; i < n; i++) {
			__transaction_atomic {	
			if (minPoint->x > points[i].x) {
			  minPoint->x = points[i].x;
			}
			if (minPoint->y > points[i].y) {
			  minPoint->y = points[i].y;
			}
			if (maxPoint->x < points[i].y) {
			  maxPoint->x = points[i].x;
			}
			if (maxPoint->y < points[i].y) {
			  maxPoint->y = points[i].y;
			}
		  }
	  }

}

}
/*
int main (){
	CowichanOpenMP oi;
	Point a(1,2), b(2,3), c(3,4), d(4,5);
	PointVector in,out;
	in[0] = a;
	in[1] = b;
	in[2] = c;
	in[3] = d;
	
	oi.norm(in,out);
	return 0;
}*/

