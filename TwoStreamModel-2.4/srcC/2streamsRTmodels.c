
#include <stdio.h>
#include <math.h>

/* general usage tools and types */
#include "commonrad.h"
#include "common.h"
#include "interf.h"
#include "gamma.h"
#include "2streamsRTmodels.h"

/* radiative transfer formulations */
#include "black_canopy.h"
#include "black_background.h"

/* Generic 2stream model as published in the JGR paper */
static int  T1T2T3model(struct RTContext *,GammaCoeffs_F);

int  Full_model(struct RTContext *ct)
{
   return T1T2T3model(ct,FullGammas);
}
int  Dble_model(struct RTContext *ct)
{
   return T1T2T3model(ct,DblGammas);
}
int  Sgle_model(struct RTContext *ct)
{
   return T1T2T3model(ct,SglGammas);
}
int  Plano_model(struct RTContext *ct)
{
   return T1T2T3model(ct,PlanoGammas);
}
int  Erecto_model(struct RTContext *ct)
{
   return T1T2T3model(ct,ErectoGammas);
}


int T1T2T3model(struct RTContext *ct,GammaCoeffs_F gammas) 
{
   /* local variables */
   int ok;
   
   /* calculated fluxes */
   double Collim_Alb_BB, Collim_Tran_BB, Collim_Abs_BB;
   double Isotrop_Alb_BB,Isotrop_Tran_BB,Isotrop_Abs_BB;
   double Collim_Tran_BC, Isotrop_Tran_BC;
   double Collim_Alb_BC, Isotrop_Alb_BC;
   double Collim_Alb_T3,Collim_Tran_T3,Collim_Abs_T3;
   double Isotrop_Alb_T3,Isotrop_Tran_T3,Isotrop_Abs_T3;
   double Collim_Tran_TotalOneWay,Isotrop_Tran_TotalOneWay;
   double Collim_Rbgd1, Isotrop_Rbgd1;
   double Bellow_reinject_rad;
   
   /* definition of the routines to be used */
   TbarreUncoll_P tbu=TBarreUncoll_exact;

   /* intermediate variables to the problem */
   double fdir;
   double gammaCoeffs[4],gammaCoeffs_star[4];
   double tauprimetilde,tauprimestar;
   
   
   (*gammas)(ct,&tauprimetilde,&tauprimestar,gammaCoeffs,gammaCoeffs_star);
   
   /* zeroing all quantities */
   Collim_Alb_BB = Collim_Tran_BB = Collim_Abs_BB = Collim_Tran_BC = Collim_Tran_TotalOneWay = Collim_Alb_BC = 0. ;
   Isotrop_Alb_BB = Isotrop_Tran_BB = Isotrop_Abs_BB = Isotrop_Tran_BC = Isotrop_Tran_TotalOneWay = Isotrop_Alb_BC = 0.;
   Collim_Alb_T3 = Collim_Tran_T3 = Collim_Abs_T3 = 0.;
   Isotrop_Alb_T3 = Isotrop_Tran_T3 = Isotrop_Abs_T3 = 0.;
   Collim_Rbgd1 = Isotrop_Rbgd1 = 0.;

   /* black background (T1) BHx terms are always nedeed irrespective of the illumination.*/
   bhrT1(ct->rl,ct->tl,
	 gammaCoeffs_star[0],gammaCoeffs_star[1],gammaCoeffs_star[2],gammaCoeffs_star[3],
	 tauprimestar,&Isotrop_Alb_BB,&Isotrop_Tran_BB,&Isotrop_Abs_BB,&ok);
   if (ok == 1) {return 1;}
   
   (*tbu)(tauprimestar,&Isotrop_Tran_BC);

   Bellow_reinject_rad = 1.0 / (1.0 - ct->rg*Isotrop_Alb_BB);
   Isotrop_Tran_TotalOneWay = Isotrop_Tran_BC + Isotrop_Tran_BB ;
   
   /* we perform DHRs only if the illumination is *not* purely isotropic: */
   if (ct->tta != ISOLIGHT) {
      
      /* black background fluxes */
      dhrT1(ct->rl,ct->tl,gammaCoeffs[0],gammaCoeffs[1],gammaCoeffs[2],gammaCoeffs[3],
	    ct->tta_rad,tauprimetilde,&Collim_Alb_BB,&Collim_Tran_BB,&Collim_Abs_BB,&ok);
      if (ok == 1) {return 1;}

      /* black canopy terms */
      TdirCollimated(tauprimetilde,ct->tta_rad,&Collim_Tran_BC);
      Collim_Alb_BC = Collim_Tran_BC * Isotrop_Tran_BC;

      /* total one-way transmisstion */
      Collim_Tran_TotalOneWay  = Collim_Tran_BC  + Collim_Tran_BB ;

      /* coupled terms */
      Collim_Rbgd1   = Collim_Tran_BC*Isotrop_Tran_BB + Collim_Tran_BB*(Isotrop_Tran_TotalOneWay);
      Collim_Alb_T3  = (Collim_Rbgd1 + ct->rg*Collim_Alb_BC*Isotrop_Alb_BB)*Bellow_reinject_rad;
      Collim_Tran_T3 = Isotrop_Alb_BB*Collim_Tran_TotalOneWay*Bellow_reinject_rad;

      Collim_Rbgd1 += Collim_Alb_BC;
   } 
   
   if (ct->f_diff != 0.) {

      Isotrop_Alb_BC  = Isotrop_Tran_BC * Isotrop_Tran_BC;
      
      Isotrop_Rbgd1   = Isotrop_Tran_BC*Isotrop_Tran_BB + Isotrop_Tran_BB*(Isotrop_Tran_TotalOneWay);
      Isotrop_Alb_T3  = (Isotrop_Rbgd1 + ct->rg*Isotrop_Alb_BC*Isotrop_Alb_BB)*Bellow_reinject_rad;
      Isotrop_Tran_T3 = Isotrop_Alb_BB*Isotrop_Tran_TotalOneWay*Bellow_reinject_rad;
      
      Isotrop_Rbgd1 += Isotrop_Alb_BC;
   } 
  
   /* fraction of collimated light */
   fdir = 1. - ct->f_diff;

   /* black background and black canopy terms */
   ct->Alb_t1  = fdir*Collim_Alb_BB         + ct->f_diff*Isotrop_Alb_BB;
   ct->Tran_t1 = fdir*Collim_Tran_BB        + ct->f_diff*Isotrop_Tran_BB;
   ct->Abs_t1  = fdir*Collim_Abs_BB         + ct->f_diff*Isotrop_Abs_BB;
   ct->Alb_t2  = ct->rg*(fdir*Collim_Alb_BC + ct->f_diff*Isotrop_Alb_BC);
   ct->Tran_t2 = fdir*Collim_Tran_BC        + ct->f_diff*Isotrop_Tran_BC;
   
   ct->Rbgd1   = ct->rg*(fdir*Collim_Rbgd1  + ct->f_diff*Isotrop_Rbgd1);
 
   /* coupled term */
   ct->Alb_t3  = ct->rg*(fdir*Collim_Alb_T3 + ct->f_diff*Isotrop_Alb_T3);
   ct->Tran_t3 = ct->rg*(fdir*Collim_Tran_T3 + ct->f_diff*Isotrop_Tran_T3);

   /* total diffuse-sky fluxes */
   ct->Alb  = ct->Alb_t1 + ct->Alb_t2 + ct->Alb_t3;
   ct->Tran = ct->Tran_t1 + ct->Tran_t2 + ct->Tran_t3;
   ct->Abs  = 1.0 + ct->rg*ct->Tran - (ct->Alb + ct->Tran);
  
   ct->Abs_t3 = ct->Abs - ct->Abs_t1;
   
   return 0;
}
