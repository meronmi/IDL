/* ---------------------------------------------------
 * File            : black_background.c
 * Author          : Lavergne Thomas                          
 * Creation Date   : 10 August 2004                            
 * Purpose         :                                  
 * 	Radiative tranfer module for the BlackSoil 2-streams model.                                      
 * ---------------------------------------------------
 *
 * Note: 
 * This module needs:
 *    *) C Maths library functions <math.h> (cos,sqrt,...)
 *    *) a 'are_equals(f1,f2)' which return 1 if f1 and f2 can be considered equals 
 *              (the threshold is not specified)
 *              
 */

#include <stdio.h>
#include <math.h>
#include "commonrad.h"


#undef STREAMSRT_DEBUG


/*++++++++ PROCEDURE-LIKE INTERFACE ++++++++*/
void dhrT1(const double rl,const double tl,
      const double gamma1,const double gamma2,const double gamma3,const double gamma4,
      const double tta,const double tau,double *AlbBS,double *Tdif,double *AbsVgt,int *ok)
{
  double alpha1,alpha2,ksquare,k;
  double first_term,secnd_term1,secnd_term2,secnd_term3;
  double expktau,Tdir;
  double mu0 = cos(tta);
  double w0 = rl + tl;

  *ok = 0;

  Tdir = exp(-tau/mu0);

  /* There is a difference between conservative and non-conservative scattering conditions */
  if (!are_equals(w0,1.) && (w0 != 0.)) {
     /* NON_CONSERVATIVE SCATTERING */

     /* some additional parameters */
     alpha1  = gamma1*gamma4 + gamma2*gamma3;
     alpha2  = gamma1*gamma3 + gamma2*gamma4;
     ksquare = gamma1*gamma1 - gamma2*gamma2;
     k       = sqrt(ksquare);
     expktau = exp(k*tau);
     
     /* Black Soil Albedo */
     first_term  = ((1.-ksquare*mu0*mu0)*((k+gamma1)*expktau + (k-gamma1)/expktau));
     if (are_equals(first_term,0.)) {
	/* we will be dividing by zero : cannot continue. */
	*ok = 1;
     } else { 
        first_term = 1./first_term;
        secnd_term1 = (1. - k*mu0)*(alpha2 + k*gamma3)*expktau;
        secnd_term2 = (1. + k*mu0)*(alpha2 - k*gamma3)/expktau;
        secnd_term3 = 2. * k * (gamma3 - alpha2*mu0)*Tdir;
        *AlbBS = (w0 * first_term * (secnd_term1 - secnd_term2 - secnd_term3));
     
        /* Transmission */
        if (are_equals(ksquare,0.)) first_term = 1.;
        secnd_term1 = (1.+k*mu0)*(alpha1+k*gamma4)*expktau;
        secnd_term2 = (1.-k*mu0)*(alpha1-k*gamma4)/expktau;
        secnd_term3 = 2. * k * (gamma4 + alpha1*mu0);
        *Tdif = - w0*first_term*(Tdir*(secnd_term1 - secnd_term2) - secnd_term3);
     
        /* Absorption by vegetation */
        *AbsVgt = (1. - (*Tdif+Tdir) - *AlbBS);
     }
  } else if (w0 == 0.) {
    /* BLACK CANOPY: */
     *AlbBS = *Tdif = 0.;
     *AbsVgt = (1. - Tdir);
  } else {
    /* CONSERATIVE SCATTERING */
    *AlbBS =  (1./(1. + gamma1*tau))*(gamma1*tau + (gamma3-gamma1*mu0)*(1.-exp(-tau/mu0)));
    *Tdif   = 1. - *AlbBS - Tdir;
    *AbsVgt = 0.;
  }
}

void bhrT1(const double rl,const double tl,
      const double gamma1, const double gamma2, const double gamma3, const double gamma4,
      const double tau,double *AlbBS,double *Tdif,double *AbsVgt,int *ok)
{
   double a=0.705;
   dhrT1(rl,tl,gamma1,gamma2,gamma3,gamma4,acos(0.5/a),tau,AlbBS,Tdif,AbsVgt,ok);
}

