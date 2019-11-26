/**
 * \file cowichan_openmp/winnow.cpp
 * \brief OpenMP winnow implementation.
 * \see CowichanOpenMP::winnow
 */

#include "../cowichan_openmp/cowichan_openmp.hpp"
#include "sort.hpp"

namespace cowichan_openmp
{

/**
 * Count the number of set cells in each thread bucket.
 * \param mask boolean mask.
 * \param nr number of rows in mask.
 * \param nc number of columns in mask.
 */
index_t mask_count(BoolMatrix mask, index_t nr, index_t nc);

}

/*****************************************************************************/

void CowichanOpenMP::winnow(IntMatrix matrix, BoolMatrix mask,
    PointVector points) {

  index_t r, c;
  index_t len; // number of points
  index_t stride; // selection stride
		
  // count set cell in
  len = mask_count (mask, nr, nc);
  printf("len: %ld \n", len);
  if (len < n) {
    not_enough_points();
  }

  WeightedPointVector weightedPoints = NULL;
  try {
    weightedPoints = NEW_VECTOR_SZ(WeightedPoint, len);
  }
  catch (...) {out_of_memory();}

  // fill temporary vector
  index_t i = 0;
  index_t num_threads = omp_get_max_threads();

  index_t* buckets = NULL;

  try {
    buckets = NEW_VECTOR_SZ(index_t, num_threads);
  }
  catch (...) {out_of_memory();}
  buckets[0] = 0;
  buckets[1] = 84387359;
  buckets[2] = 28128126;
  buckets[3] = 56259144;
#pragma omp parallel private(i)
  {
	index_t thread_num = omp_get_thread_num();
    i = buckets[thread_num];
    printf("i: %ld \n", i);
#pragma omp for schedule(static)
    for (r = 0; r < nr; r++) { 
#pragma omp parallel for schedule(static)
      for (c = 0; c < nc; c++) {
        if (MATRIX_RECT(mask, r, c)) {
          weightedPoints[i++] = WeightedPoint((real)c, (real)r, MATRIX_RECT(matrix, r, c));
        }
	  }
    }
  }
delete [] buckets;

#ifdef SORT_TIME
  INT64 start, end;
  start = get_ticks ();
#endif

  // sort
#if defined(LIN32) || defined(LIN64)
#pragma omp parallel
  {
#pragma omp single
    {
		printf("%s\n", "quick_sort");
      quick_sort(weightedPoints, len);
    }
  }
#else
  printf("%s\n", "histogram_sort");
  histogram_sort(weightedPoints, len);
#endif

#ifdef SORT_TIME
  end = get_ticks ();
#endif

  // copy over points
  stride = len / n;

#pragma omp parallel for schedule(static)
  for (i = n - 1; i >= 0; i--) {
#ifdef WINNOW_OUTPUT
    std::cout << weightedPoints[len - 1 - (n - 1 - i) * stride].weight << "\n";
#endif
    points[i] = weightedPoints[len - 1 - (n - 1 - i) * stride].point;
  }
  
#ifdef SORT_TIME
  std::cout << "winnow sort: ";
  print_elapsed_time(start, end);
  std::cout << std::endl;
#endif

  delete [] weightedPoints;
}

/*****************************************************************************/

namespace cowichan_openmp
{

index_t mask_count(BoolMatrix mask, index_t nr, index_t nc) {

  index_t r, c;
  index_t sum, sum2 = 0;

#pragma omp parallel firstprivate(sum)
  {
	#pragma omp for schedule(static)
			for (r = 0; r < nr; r++) {
				//__transaction_relaxed {
					#pragma omp parallel for schedule(static)
						  for (c = 0; c < nc; c++) {
							if (MATRIX_RECT_NC(mask, r, c, nc)) {
							  sum++; 
							}
						  }
					  //}
		}
		__transaction_atomic {sum2 += sum;}
	}
	printf("sum2: %ld \n", sum2);
	return sum2;
}
}
