/**
 * \file cowichan_openmp/vecdiff.cpp
 * \brief OpenMP vecdiff implementation.
 * \see CowichanOpenMP::vecdiff
 */

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


real CowichanOpenMP::vecdiff (Vector actual, Vector computed)
{
  index_t i;
  real diff;
  real maxDiff;
  maxDiff = (real)fabs((double)(actual[0] - computed[0]));

  STM_STARTUP();

  stm_init_thread();
#pragma omp parallel private(diff)
  {
  	STM_START();
	 //index_t thread_num = omp_get_thread_num();
	#pragma omp for schedule(static)
		for (i = 1; i < n; i++) { 
			diff = (real)fabs((double)(actual[i] - computed[i]));
			if (STM_READ(&maxDiff) < diff) {
				STM_STORE(&maxDiff, diff);
				//maxDiff = diff;
			  }
			
		}
  }
  stm_commit();

  STM_STATS();
  stm_exit();
  printf("maxDiff =%f \n", maxDiff);
  return maxDiff;
}

