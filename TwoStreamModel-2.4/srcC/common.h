/* ---------------------------------------------------
 * File            : common.h                       
 * Author          : Lavergne Thomas                          
 * Creation Date   : 10 August 2004                            
 * Purpose         :                                  
 * 	Constants and macros needed by the whole project                                      
 * ---------------------------------------------------
 */

#ifndef COMMON_H
#define COMMON_H


#define MAIN_VERSION_NUMBER 2
#define SECONDARY_VERSION_NUMBER 4

#define NOVALUE   999   /* used to detect if a physical parameter was not set or set twice. */
#define ISOLIGHT  333   /* used to detect if the source was set to a purely isotropic one (-s keyword) */



#define USE_C99
#ifdef USE_C99
#define is_negative(x)  (signbit(x) != 0)
#define is_positive(x)  (signbit(x) == 0 && x != 0.)
#else /* USE_C99 */
#define is_negative(x)  (x < 0.)
#define is_positive(x)  (x > 0.)
#endif /* USE_C99 */



#define max(a, b)               ((a) > (b) ? (a) : (b))
#define min(a, b)               ((a) < (b) ? (a) : (b))

#endif /* COMMON_H */
