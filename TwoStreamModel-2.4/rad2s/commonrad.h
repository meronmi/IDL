/* ---------------------------------------------------
 * File            : commonrad.h                       
 * Author          : Lavergne Thomas                          
 * Creation Date   : 10 August 2005                            
 * Purpose         :                                  
 * 	Constants and macros needed only by the 
 * 	low-level radiation routines.
 * ---------------------------------------------------
 */

#ifndef COMMONRAD_H
#define COMMONRAD_H

#define PI           3.14159265358979323846
#define deg2rad(x)   (PI * x / 180.)
#define rad2deg(x)   (180. * x / PI)


#define EPSILON 0.00001
#define are_equals(x,y) (fabs(x-y)<=EPSILON*fabs(x))

#endif /* COMMONRAD_H */
