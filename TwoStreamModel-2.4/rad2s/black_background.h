/* ---------------------------------------------------
 * File            : black_background.h
 * Author          : Lavergne Thomas                          
 * Creation Date   : 10 August 2004                            
 * Purpose         :                                  
 * 	Radiative tranfer module for the BlackSoil 2-streams model.                                      
 * ---------------------------------------------------
 */

#ifndef BBRT_H
#define BBRT_H

/* definition of the main routine for calculating the solution to the 
 *    BlackBackground 2-streams model.
 *    Explicit variable names are given here for convenience.
 */    

extern void  dhrT1(
      const double Leaf_Reflectance,               
      const double Leaf_Transmitance,
      const double gamma1,               
      const double gamma2,
      const double gamma3,               
      const double gamma4,
      const double Sun_Zenith_Angle_radians,
      const double Optical_Thickness,
      double *BlackSoil_Albedo,
      double *Collided_Transmission_Through_the_Layer,
      double *Fraction_of_Absorption_by_the_Layer,
      int *ok);

extern int  bhrT1(
      const double Leaf_Reflectance,               
      const double Leaf_Transmitance,
      const double gamma1,               
      const double gamma2,
      const double gamma3,               
      const double gamma4,
      const double Optical_Thickness,
      double *BlackSoil_Albedo,
      double *Collided_Transmission_Through_the_Layer,
      double *Fraction_of_Absorption_by_the_Layer,
      int *ok);
#endif /* BBRT_H */
