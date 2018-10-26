/* ---------------------------------------------------
 * File            : usageMessage.c                       
 * Author          : Lavergne Thomas                          
 * Creation Date   : 09 August 2005                            
 *
 * Purpose         : The display message and the
 *          function to display it.
 * --------------------------------------------------
 */

#include <stdio.h>

#include "common.h"
#include "usageMessage.h"

void DisplayUsageMessage(void)
{
   fprintf(stderr,"Two-stream model for heterogeneous vegetation canopy. Version %d.%d\n",MAIN_VERSION_NUMBER,SECONDARY_VERSION_NUMBER);
   fprintf(stderr,"\n");
   fprintf(stderr,"Reference: \"Symplifying the Interaction of Land Surfaces with Radiation for Relating Remote Sensing Products \n");
   fprintf(stderr,"to Climate Models\", Pinty et al. (2005), Journal of Geophysical Research (JGR), in press.\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"All flags can appear in no specified order on the command line.\n");
   fprintf(stderr,"GENERAL FLAGS\n"); 
   fprintf(stderr,"\t-h\n\t\tDisplays this message and exits\n");
   fprintf(stderr,"\t-v\n\t\tDisplays the software's version number and exits\n");
   fprintf(stderr,"\t-o format_string\n\t\tUse format_string to select the radiative quantities\n"); 
   fprintf(stderr,"\t\tto be displayed as output. This is not compatible with the -A flag.\n");
   fprintf(stderr,"\t-A\n\t\tDisplay the whole set of resulting radiative quantities. This is not compatible with the -o flag.\n"); 
   fprintf(stderr,"\t-O\n\t\t(capital o) Display a line composed of the columns' tags and index.\n");
   fprintf(stderr,"\tFor both -o and -A, refer to section OUTPUT FORMAT.\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"TWO-STREAM MODEL\n");
   fprintf(stderr,"\tThe first three models are intended to calculate the radiative fluxes in heterogeneous vegetation\n");
   fprintf(stderr,"\tcanopies by using effective variable values:\n");
   fprintf(stderr,"\t--full\n\t\tSelects the generic model in [Pinty et al. 2005] with all orders of scattering activated.\n");
   fprintf(stderr,"\t--dble\n\t\tSelects a model with only the first and second orders of vegetation scattering activated.\n");
   fprintf(stderr,"\t--sgle\n\t\tSelects a model with only the first order of vegetation scattering activated (single scattering).\n");
   fprintf(stderr,"\n\tTo choose a model is optional. The default is to use the full model.\n\n");
   fprintf(stderr,"\tThe following three models are intended to calculate the radiative fluxes in homogeneous vegetation\n");
   fprintf(stderr,"\tcanopies but with different leaf normal distributions (LND):\n");
   fprintf(stderr,"\t--spher\n\t\tModels a homogeneous canopy with a spherical LND.\n");
   fprintf(stderr,"\t--plano\n\t\tModels a homogeneous canopy with a planophile LND (mostly horizontal leaves).\n");
   fprintf(stderr,"\t--erecto\n\t\tModels a homogeneous canopy with an erectophile LND (mostly vertical leaves).\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"ILLUMINATION\n");
   fprintf(stderr,"\tThe model calculates the radiative fluxes in two illumination geometry, namely collimated source\n");
   fprintf(stderr,"\tand isotropic source. The fraction of diffuse to total downward flux f_diff can be specified. A weighting\n");
   fprintf(stderr,"\tis applied between the outputs in the two illumination conditions to yields the resulting fluxes:\n");
   fprintf(stderr,"\t\t\tFlux(tot) = (1 - f_diff)*Flux(collimated) + f_diff*Flux(isotropic)\n");
   fprintf(stderr,"\t-s illu   : Describe the illumination geometry. If illu is a numerical value, it is the zenith angle of the\n");
   fprintf(stderr,"\t\tcollimated source. Unit is degrees and it must be in ]-90,90[. However, if illu is the keyword \"iso\"\n");
   fprintf(stderr,"\t\t(quotes removed), it indicates that the source is purely diffuse (f_diff is implied to 1.0)\n");
   fprintf(stderr,"\t-F f_diff   : set the fraction of diffuse to total downward flux. If no -F flag is used, the illumination is\n"); 
   fprintf(stderr,"\t\teither purely collimated or purely isotropic, depending on the -s flag. f_diff is a numerical value that\n");
   fprintf(stderr,"\t\tmust be in ]0,1[. The -F flag can only be used if -s is present and is given a numerical value.\n");
   fprintf(stderr,"\n");  
   fprintf(stderr,"VEGETATION AND BACKGROUND DESCRIPTION PARAMETERS\n");
   fprintf(stderr,"\t-l lai     : Set the True Leaf Area Index of the canopy <LAI>. Must be > 0.\n");
   fprintf(stderr,"\tThe Structure factors zeta and zeta* multiply <LAI> to yield the effective value of LAI.\n");
   fprintf(stderr,"\t-z  zeta      : Structure factor zeta. Must be > 0.\n");
   fprintf(stderr,"\t-z* zeta_star : Mean Structure factor zeta_star. Must be > 0.\n");
   fprintf(stderr,"\t\tIf both are unset, the canopy is homogeneous: zeta=zeta_star=1.\n");
   fprintf(stderr,"\t\tIf only one is set, both zeta and zeta_star are set to the given value.\n");
   fprintf(stderr,"\t-g rg      : Set the background reflectance. Must be in [0,1]. If the flag is omitted, the background\n");
   fprintf(stderr,"\t\treflectance defaults to 0 (black background model).\n");
   fprintf(stderr,"\tThe effective value for the leaf Reflectance and Transmittance can be set using 2 (and no more than 2) of:\n");
   fprintf(stderr,"\t-r rl      : Set the effective leaf Reflectance. Must be in [0,1[\n");
   fprintf(stderr,"\t-t tl      : Set the effective leaf Transmittance. Must be in [0,1[.\n");
   fprintf(stderr,"\t-w wl      : Set the effective leaf single scattering albedo. wl=rl+tl. Must be in [0,1[.\n");
   fprintf(stderr,"\t-d dl      : Set the effective leaf forward-scattering efficiency. dl = rl/tl. Must be > 0.\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"OUTPUT FORMAT (issued on stdout only if the model completes the calculations).\n");
   fprintf(stderr,"\tThe results are all displayed on one line. The first 8 columns are a remainder\n");
   fprintf(stderr,"\tof the input parameters to the program, in the following order.\n");
   fprintf(stderr,"\t (1) sun     : Sun zenith angle in degrees. (if the illumination is purely diffuse: a non-value)\n");
   fprintf(stderr,"\t (2) fdiff   : fraction of diffuse downward radiation.\n");
   fprintf(stderr,"\t (3) <lai>   : True Lai of the canopy.\n");  
   fprintf(stderr,"\t (4) zeta    : Structure factor of the canopy for this Sun angle.\n");
   fprintf(stderr,"\t (5) zeta*   : Mean Structure factor of the canopy.\n");
   fprintf(stderr,"\t (6) rbgd    : Background reflectance.\n");
   fprintf(stderr,"\t (7) rleaf   : Leaf reflectance (effective value).\n");
   fprintf(stderr,"\t (8) tleaf   : Leaf transmittance (effective value).\n");
   fprintf(stderr,"\tThe results of the calculations are then displayed on the same line. Some (and their relative order)\n");
   fprintf(stderr,"\tof these values can be choosen via the -o format_string sequence. format_string is a list of space (or comma)\n");
   fprintf(stderr,"\tseparated tokens, all of which composed of the %% (percent) character immediatly followed by a multi-character\n");
   fprintf(stderr,"\tidentifier (e.g. %%Ttot). These tokens are now listed. If no -o flag is specified, the output is the same\n");
   fprintf(stderr,"\tas if the sequence -o \"%%Alb,%%Abs,%%Tran\" had been specified. Alternatively, the -A flag makes all the\n");
   fprintf(stderr,"\tfollowing radiative fluxes to be displayed, in this order:\n");
   fprintf(stderr,"\t( 9) %%Alb      Total Albedo.\n");
   fprintf(stderr,"\t(10) %%Abs      Total Absorption by the vegetation canopy.\n");
   fprintf(stderr,"\t(11) %%Tran     Total Transmission to the background level.\n");
   fprintf(stderr,"\t(12) %%Alb_t1   Albedo accounting for the Black Background component.\n");
   fprintf(stderr,"\t(13) %%Alb_t2   Albedo accounting for the Black Canopy component.\n");
   fprintf(stderr,"\t(14) %%Alb_t3   Albedo accounting for the coupled canopy-background term.\n");
   fprintf(stderr,"\t(15) %%Abs_t1   Absorption by the vegetation for the Black Background component.\n");
   fprintf(stderr,"\t(16) %%Abs_t3   Absorption by the vegetation for the coupled canopy-background term.\n");
   fprintf(stderr,"\t(17) %%Tran_t1  Transmission to the background level for the Black Background conponent.\n");
   fprintf(stderr,"\t\t\tIt only accounts for the radiation collided by the vegetation.\n"); 
   fprintf(stderr,"\t(18) %%Tran_t2  Transmission to the background level for the Black Canopy conponent.\n");
   fprintf(stderr,"\t\t\tThis is the direct transmission through the layer.\n");
   fprintf(stderr,"\t(19) %%Tran_t3  Transmission to the background level for the coupled canopy-background term.\n");
   fprintf(stderr,"\t(20) %%Rbgd1    Albedo accounting for the radiation having hit only once the background (whether collided\n");
   fprintf(stderr,"\t\t\tor not by the vegetation, whether upward or downward).\n");
   fprintf(stderr,"\n");
   
   fprintf(stderr,"NOTE\n");
   fprintf(stderr,"\tExcept for -z and -z*, only the first letter of the flag is parsed\n\tso that -t -tl -trans -tta are all equivalent.\n");
}
