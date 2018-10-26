/* ----------------------------------------------------------------------
 * File            : gamma.h                       
 * Author          : Lavergne Thomas                          
 * Creation Date   : 25 May 2005                            
 * Purpose         :                                  
 * 	Routines and data types for the calculation of appropriate gamma
 * 	coefficient (as in Table 4)
 * -----------------------------------------------------------------------
 */

#ifndef GAMM_H
#define GAMM_H

typedef void (*GammaCoeffs_F)(const struct RTContext *,double *,double *,double *,double *);

extern void FullGammas(const struct RTContext *ct,
      double *tauprimetilde,double *tauprimestar,double *gamma,double *gamma_star);
extern void SglGammas(const struct RTContext *ct,
      double *tauprimetilde,double *tauprimestar,double *gamma,double *gamma_star);
extern void DblGammas(const struct RTContext *ct,
      double *tauprimetilde,double *tauprimestar,double *gamma,double *gamma_star);
extern void PlanoGammas(const struct RTContext *ct,
      double *tauprimetilde,double *tauprimestar,double *gamma,double *gamma_star);
extern void ErectoGammas(const struct RTContext *ct,
      double *tauprimetilde,double *tauprimestar,double *gamma,double *gamma_star);

#endif /* GAMM_H */

