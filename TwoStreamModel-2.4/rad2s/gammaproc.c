/* ----------------------------------------------------------------------
 * File            : gammaproc.c                       
 * Author          : Lavergne Thomas                          
 * Creation Date   : 09 August 2005                            
 * Purpose         :                                  
 *         Isolate the low-level procedures for the gamma coefficients
 *         calculations.
 * -----------------------------------------------------------------------
 */

#include "gammaproc.h"


void gammas(double rl,double tl,double mu0,double *gamma) 
{
   double wd  = rl - tl;
   double w0  = rl + tl;
   double w0half  = w0*0.5;
   double wdsixth = wd*RTSIXTH;
   
   gamma[0] = 2.*(1. - w0half + wdsixth);
   gamma[1] = 2.*(w0half + wdsixth);
   gamma[2] = 2.*(w0half*0.5 + mu0*wdsixth)/w0;
   gamma[3] = 1. - gamma[2];
}

