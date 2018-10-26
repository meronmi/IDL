/* ---------------------------------------------------
 * File            : black_canopy.h
 * Author          : Lavergne Thomas                          
 * Creation Date   : 10 August 2004                            
 * Purpose         :                                  
 * 	Radiative tranfer module for the Black Canopy component.                                      
 * ---------------------------------------------------
*/


/* Procedure-like interface */
typedef void (*TbarreUncoll_P)(const double,double *);
extern void TBarreUncoll_exact(const double tau,double *);
extern void TBarreUncoll_approxA(const double tau,double *);
extern void TBarreUncoll_approxATuned(const double tau,double *);
extern void TBarreUncoll_approxB(const double tau,double *);
extern void TBarreUncoll_approxBTuned(const double tau,double *);
extern void TdirCollimated(const double tau,const double tta0,double *);
extern void R_T2Collimated(const double tau, const double tta0, const double taustar, TbarreUncoll_P tbu,double *);
extern void R_T2Isotropic(const double taustar, TbarreUncoll_P tbu,double *);
