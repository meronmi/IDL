/* ---------------------------------------------------
 * File            : 2streams.c                       
 * Author          : Lavergne Thomas                          
 * Creation Date   : 02 August 2004                            
 * Purpose         :                                  
 * 	Main file for the computation of a 2-streams solution.                                      
 * ---------------------------------------------------
 */

/*
 * Copyright (C) 2005 by STARS
 *
 * 	Academic users:
 * 	Are authorized to use this code for research and teaching,
 * 	but must acknowledge the use of these routines explicitly
 * 	and refer to the references in any publication or work.
 * 	Original, complete, unmodified versions of these codes may be
 * 	distributed free of charge to colleagues involved in similar
 * 	activities.  Recipients must also agree with and abide by the
 * 	same rules. The code may not be sold, nor distributed to
 * 	commercial parties, under any circumstances.
 *
 * 	Commercial and other users:
 * 	Use of this code in commercial applications is strictly
 * 	forbidden without the written approval of the authors.
 * 	Even with such authorization the code may not be distributed
 * 	or sold to any other commercial or business partners under
 * 	any circumstances.
 * 
 * This software is provided as is without any warranty whatsoever.
 */


/*
 * NOTE ON THE IMPLEMENTATION (ALL VERSIONS):
 * ====================================
 * The implementation tries to be as C99 compliant as possible. 
 * In standard run mode, every warning/error messages are sent to stderr whereas results are always sent to stdout.
 *
 * VERSION 1.1:
 * ============
 * disable the not-yet-wide-spreaded <stdbool.h> header
 *
 * VERSION 1.2:
 * ============
 * the user can choose the output format via the -o option.
 *
 * VERSION 2.0:
 * ============
 * move towards a complete model (not only black-background) to come with the JGR paper.
 * 
 * DESCRIPTION:
 * ===========
 * In the main program, the command line is parsed to get the physical properties specidied by the user. These properties are
 * stored in a structure RTContext and then checked out. Eventually, a 2stream solution is calculated (as in the JGR paper)
 * and results are displayed if everything went smoothly.
 * 
 * THE PROPOSED 2-STREAM MODEL:
 * ===========================
 * The 2-stream approch solves the Radiative Transfer equation *only in fluxes*. It solves the Albedo, the Transmition and
 * hence the Absorption for an arbitraty thick vegetation layer under top-illumination by a collimated and/or 
 * isotropic Sun and above a Lambertian background. Unlike in usual 1D approach, effective variable values are used for the LAI
 * and the leaf scattering properties to account for the heterogeneity (3D properties) of the system under study (e.g. forests).
 * 
 *    
 * INPUT AND OUTPUT VALUES:
 * =======================
 * please refer to the usage message or the USAGE file (once the program is compiled) and to the JGR paper for an extensive discussion
 * on the meaning and valid input parameters value (illumination conditions, LAI, structure factor, background reflectance, leaves 
 * reflectance and transmittance,...). Output values (radiative fluxes) may be accessed for the different contributions (black background/
 * black canopy/coupled term) and for the sum of these (default). 
 */



#include <stdio.h>
#include <stdlib.h>
/* #include <stdbool.h> As of V1.1 this is avoided as it is not implemented in many compilers (e.g. Sun cc) */
#include <math.h>
#include "commonrad.h"   /* defines some macros (e.g. deg2rad(),...) and numerical values (e.g. PI) */
#include "common.h"     
#include "interf.h"

#undef STREAMS_DEBUG

/*
 * MAIN ROUTINE.
 */
int main(int argc,char *argv[])
{

   int exit_code=EXIT_SUCCESS;

   /* The RTContext structure (defined in interf.h) serves to store input and output parameters */
   struct RTContext Context;
   
   /* Fill the Context's fields with default values to later detect unset or multiply set arguments */
   FillUpRTContext(&Context,(float)NOVALUE);

   /* Parse the command line and does a first check on parameter values (positivness, valid ranges,...)
    * A 'usage message' can be displayed (-h) as well as a version number (-v).
    * The goal here is to verbose and exit as soon as an error is found.*/
   if (ParseCommandLine(argc,argv,&Context,NOVALUE)) {
      fprintf(stderr,"Error while parsing the command line arguments.\n");
      goto ErrorFinish;
   }
   
   /* Check the consistency of the physical parameters. At exit, the prerequisites for the 2stream model are granted. */
   if (CheckRTContext(&Context)) {
      fprintf(stderr,"Physical parameters are not consistent with the choosen model.\n");
      goto ErrorFinish;
   }

   
   /* At that point, we are ready to calculate the 2streams solution for the given parameters. */
   if (Context.RTmodel(&Context)) {
      /* No error message is displayed by the routine. ??? consider having different exit codes ??? */
      fprintf(stderr,"The 2streams model implemented here has no consistent solution for these values.\n");
      fprintf(stderr,"(cont.) You can try again with slightly perturbed input values.\n");
      goto ErrorFinish;
   } else {
      /* The output values of the model can be trusted so that we display them. */
      PrintRTContext(&Context);
   }


Finish:
   /* Eventually, we release all allocated memory to the OS */
   if (FreeRTContext(&Context)) {
      fprintf(stderr,"Problem in releasing memory to the OS.\n");
      exit(EXIT_FAILURE);
   }
   return exit_code;

ErrorFinish:
   exit_code=EXIT_FAILURE;
   goto Finish;
}

