/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   tinySTM.h
 * Author: douglas
 *
 * Created on 9 de Dezembro de 2019, 17:20
 */

#ifndef TINYSTM_H
#define TINYSTM_H

#include "tinySTM/include/stm.h"
#include "tinySTM/include/wrappers.h"
#include "tinySTM/include/mod_mem.h"
#include "tinySTM/include/mod_stats.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Initialize and finalize (free) global tiny data structures
 */
#define STM_GLOBAL_INITIALIZE()     \
        stm_init();       \
        mod_mem_init(0);  \
        mod_stats_init() 
    
#define STM_GLOBAL_FINALIZE()      stm_exit();
    
/*
 * Initialize and finalize (free) private thread tiny data structures
 */    
#define STM_INITIALIZE_THREAD()     stm_init_thread();
#define STM_FINALIZE_THREAD()       stm_exit_thread();    
    
/*
 * Start a new transaction
 * Each transactions ends with a commit or an abort
 */
#define STM_START_TRANSACTION()  \
        sigjmp_buf * checkPoint = stm_start((stm_tx_attr_t) {0}); \
        sigsetjmp(*checkPoint, 0); 
    
#define STM_TRY_COMMIT()    stm_commit();    

/*
 * Option: print global statistics, such as number of aborts and commits
 */    
#define STM_PRINT_STATISTICS()     \
    unsigned long stat;     \
    if (stm_get_global_stats("global_nb_commits", &stat) != 0) \
      printf("#commits    : %lu\n", stat);   \
    if (stm_get_global_stats("global_nb_aborts", &stat) != 0) \
      printf("#aborts     : %lu\n", stat)

#ifdef __cplusplus
}
#endif

#endif /* TINYSTM_H */

