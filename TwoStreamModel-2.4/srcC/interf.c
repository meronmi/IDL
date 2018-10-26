/* ---------------------------------------------------
 * File            : interf.c                       
 * Author          : Lavergne Thomas                          
 * Creation Date   : 20 August 2004                            
 * History         : major revisions and adds for version 1.2 (user configuration of the output format)
 *                       and version 2.0
 * Purpose         :                                  
 * 	This file only deals with user interfaces, *not* with radiative transfer.
 * 	This file hosts all the necessary functionalities to produce a valid input context
 * 	(parse the command-line, check consistency) and to output the results from the model.
 * 	
 * ---------------------------------------------------
 */

#define string_converted(pend) ((*pend=='\0'?1:0)) /* macro used to determine if a string (command line) was succesfully */
                                                          /* converted to a floating point value. */

#define CheckTrailingArgument    if (argc == 1) { \
					fprintf(stderr,"\tERROR: The last flag on the command line requires an argument.\n");\
					return 1;\
                                 }
				 

#define MAXLAI  15    /* used to issue a warning (*not* an error) if a lai greater than MAXLAI is given. */
#define MAXFMTSIZE 90 /* the -o outputfmt specifier cannot be longer than MAXFMTSIZE */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>        /* used for the parsing of the output format string */

#include "commonrad.h"
#include "common.h"    
#include "usageMessage.h"

#include "interf.h"
#include "gamma.h"
#include "2streamsRTmodels.h"

#undef OUTFMT_DEBUG


/* the program is written so that programmers can add their own models (or their approximations). 
 * For the time being, only the T1T2T3 2streams model (Pinty et al. 2005) is coded, along with
 * its first and first+second orders of scattering approximations.*/
#define NBMODEL 6
static struct model_ { 
   char *tag; 
   RTmodel_F RTmodel;
   } known_models[NBMODEL]={{"--full",Full_model},
   			    {"--dble",Dble_model},
			    {"--sgle",Sgle_model},
                            {"--spher",Full_model},
			    {"--plano",Plano_model},
			    {"--erecto",Erecto_model}};

/* this table stores the valid identifiers that can appear with the -o flag */
#define NBIDENT 12
#define MAXIDENTSIZE 7
static char *valid_ident[NBIDENT]={
   "Alb","Abs","Tran",
   "Alb_t1","Alb_t2","Alb_t3",
   "Abs_t1","Abs_t3",
   "Tran_t1","Tran_t2","Tran_t3",
   "Rbgd1"};

#define ALB_IDENT  0
#define ABS_IDENT  1
#define TRAN_IDENT 2

#define ALBT1_IDENT  3
#define ALBT2_IDENT  4
#define ALBT3_IDENT  5

#define ABST1_IDENT  6
#define ABST3_IDENT  7

#define TRANT1_IDENT 8
#define TRANT2_IDENT 9
#define TRANT3_IDENT 10

#define RBGD1_IDENT 11

#define NBINPUT 8
static char *input_ident[NBINPUT]={
   "sun","<lai>","rleaf","tleaf",
   "rgbd","zeta","zeta*","fdiff"};

#define SZA_IDENT  20
#define LAI_IDENT  21
#define RL_IDENT   22
#define TL_IDENT   23
#define RG_IDENT   24
#define ZETA_IDENT        25
#define ZETA_STAR_IDENT   26
#define FRAC_DIFF_IDENT   27

static char DefaultOutPrecisionString[]="%.6f";
static char DefaultSZAPrecisionString[]="%.2f";
static char DefaultLAIPrecisionString[]="%.3f";
static char DefaultLOPPrecisionString[]="%.4f";

/* some internal routines */
static int       ParseOutputFmt(struct RTContext *,char *);
static OutFmt_t *AddOutFmt_token(OutFmt_t **,OutFmt_t *,unsigned int,struct RTContext *);
static int       InsertStandardInFmt(OutFmt_t **,struct RTContext *);
static int       InsertDefaultOutputFmt(struct RTContext *);
static int       AddAllOutputFmt(struct RTContext *); 
static void      PrintRTContext_tags(struct RTContext *);

#define DEFAULT_LAI 18
#define DEFAULT_TTA 17
#define DEFAULT_RL  32
#define DEFAULT_TL  13

int ParseCommandLine(int argc, char* argv[],struct RTContext *Context_,float startvalue)
{
   int    needed_rt=0;  /* This is a counter for asserting that we get enough information for setting rl and tl */
   float  w0,rat;
   int    i_model;
   int found_a_valid_model=0;

   w0 = rat = startvalue; /* w0 is the single scattering albedo (rl+tl) and rat is rl/tl */

   /* It is now (11th August 2005) allowed to call the executable without any argument: 
    * default values are loaded in that case.*/

   /* Parse the arguments, they should all be introduced by a -flag or --xxx sequence */
   while (--argc) {
      argv++;
      /* test the presence of the '-flag' sequence */
      if (argv[0][0] != '-') {
         fprintf(stderr,"\tERROR: All argument's tag should be preceeded by '-': %s\n",argv[0]);
	 return 1;
      }
      /* set the physical values according to the present flags */
      switch (argv[0][1]) {
	 char *checkconvert;
	 /* The model is choosen via a --xxx where xxx is a model identifier. See what model 
	  * has been asked by the user 
	  */
	 case '-' : /* choose the model to be used */
	    
	    for (i_model=0 ;i_model<NBMODEL; i_model++) {
               if (!strcmp(argv[0],known_models[i_model].tag)) {
		  Context_->RTmodel = known_models[i_model].RTmodel;
		  found_a_valid_model=1;
	       }
	    } 
	    if (found_a_valid_model == 0) {
               fprintf(stderr,"\tERROR: the model identifier is unknown: %s\n",argv[0]);
	       return 1;
	    }
	    break;
         /* ++++++++++++++++++++++++
	  * Leaf Reflectance        
	  * ++++++++++++++++++++++++*/
         case 'r' : 
            CheckTrailingArgument;
	    if (Context_->rl != startvalue) {
               fprintf(stderr,"\tERROR: the Leaf Reflectance is set more than once: %s %s\n",argv[0],argv[1]);
	       return 1;
	    }
	    Context_->rl = strtof(argv[1],&checkconvert);
	    if (!string_converted(checkconvert)) {
               fprintf(stderr,"\tERROR: The value given for the Leaf Reflectance (-r) is not correct: %s\n",argv[1]);
	       return 1;
	    }
            if (is_negative(Context_->rl)) {
               fprintf(stderr,"\tERROR: the Leaf Reflectance should not be a negative value.\n");
	       return 1;
	    }
            if (Context_->rl >= 1.) {
               fprintf(stderr,"\tERROR: the Leaf Reflectance is greater than one.\n");
	       return 1;
	    }

	    needed_rt++;
	    argc--;argv++;
	    break; 
         /* ++++++++++++++++++++++++
	  * Leaf Transmittance
	  * ++++++++++++++++++++++++*/
	 case 't' : 
            CheckTrailingArgument;
            if (Context_->tl != startvalue) {
               fprintf(stderr,"\tERROR: the Leaf Transmittance is set more than once: %s %s\n",argv[0],argv[1]);
	       return 1;
	    }
	    Context_->tl = strtof(argv[1],&checkconvert);
            if (!string_converted(checkconvert)) {
               fprintf(stderr,"\tERROR: The value given for the Leaf Transmittance (-t) is not correct: %s\n",argv[1]);
	       return 1;
	    }
            if (is_negative(Context_->tl)) {
               fprintf(stderr,"\tERROR: the Leaf Transmittance should not be a negative value.\n");
	       return 1;
	    }
            if (Context_->tl >= 1.) {
               fprintf(stderr,"\tERROR: the Leaf Transmittance is greater than one.\n");
	       return 1;
	    }
	    needed_rt++;
	    argc--;argv++;
	    break;
         /* ++++++++++++++++++++++++++++++
	  * Leaf Single Scattering Albedo
	  * ++++++++++++++++++++++++++++++*/
	 case 'w' : 
            CheckTrailingArgument;
	    if (w0 != startvalue) {
               fprintf(stderr,"\tERROR: the Single Scattering albedo is set more than once: %s %s\n",argv[0],argv[1]);
	       return 1;
	    }
	    w0 = strtof(argv[1],&checkconvert);
            if (!string_converted(checkconvert)) {
               fprintf(stderr,"\tERROR: The value given for the Single Scattering Albedo (-w) is not correct: %s\n",argv[1]);
	       return 1;
	    }
            if (is_negative(w0)) {
               fprintf(stderr,"\tERROR: the Single Scattering Albedo should not be a negative value.\n");
	       return 1;
	    }
	    if (w0 > 1.) {
               fprintf(stderr,"\tERROR: the single scattering albedo is greater than one.\n");
	       return 1;
	    }
	    needed_rt++;
            argc--;argv++;
	    break;
         /* ++++++++++++++++++++++++++++++
	  * Leaf Forward efficiency ratio
	  * ++++++++++++++++++++++++++++++*/
	 case 'd' : 
            CheckTrailingArgument;
            if (rat != startvalue) {
               fprintf(stderr,"\tERROR: the ratio rl/tl is set more than once: %s %s\n",argv[0],argv[1]);
	       return 1;
	    }
	    rat = strtof(argv[1],&checkconvert);
            if (!(string_converted(checkconvert) && is_positive(rat))) {
               fprintf(stderr,"\tERROR: The value given for the ratio rl/tl (-d) is not correct: %s\n",argv[1]);
	       return 1;
	    }
	    needed_rt++;
            argc--;argv++;
	    break;
         /* +++++++++++++++++++++++++
	  * Background Reflectance
	  * +++++++++++++++++++++++++*/
	 case 'g' : 
            CheckTrailingArgument;
            if (Context_->rg != startvalue) {
               fprintf(stderr,"\tERROR: the Background reflectance is set more than once: %s %s\n",argv[0],argv[1]);
	       return 1;
	    }
	    Context_->rg = strtof(argv[1],&checkconvert);
	    if (!string_converted(checkconvert)) {
               fprintf(stderr,"\tERROR: The value given for the Background reflectance (-g) is not correct: %s\n",argv[1]);
	       return 1;
	    }
            if (is_negative(Context_->rg)) {
               fprintf(stderr,"\tERROR: the Background reflectance should not be a negative value.\n");
	       return 1;
	    }
            if (Context_->rg > 1.) {
               fprintf(stderr,"\tERROR: the Background reflectance is greater than one.\n");
	       return 1;
	    }
	    argc--;argv++;
	    break;
         /* +++++++++++++++++++++++++
	  * Illumination geometry
	  * +++++++++++++++++++++++++*/
	 case 's' : 
            CheckTrailingArgument;
	    if (Context_->tta != startvalue) {
               fprintf(stderr,"\tERROR: the Illumination is set more than once: %s %s\n",argv[0],argv[1]);
	       return 1;
	    }
	    Context_->tta = strtof(argv[1],&checkconvert);
            if (!(string_converted(checkconvert))) {
	       /* We know it is not a numerical value, check if it is the iso keyword */
               if (!strcmp(argv[1],"iso")) {
		  /* The illumination is purely isotropic */
                  Context_->tta = ISOLIGHT;   
	       } else {
		  /* wrong token */
                  fprintf(stderr,"\tERROR: The value given for the Illumination (-s) is not correct: %s\n",argv[1]);
	          return 1;
	       }
	    }
	    argc--;argv++;
	    break;
         /* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	  * Illumination's fraction of diffuse to total downward flux 
	  * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
	 case 'F' :
            CheckTrailingArgument;
            if (Context_->f_diff != startvalue) {
               fprintf(stderr,"\tERROR: the Fraction of Diffuse light is set more than once: %s %s\n",argv[0],argv[1]);
	       return 1;
	    }
	    Context_->f_diff = strtof(argv[1],&checkconvert);
            if (!(string_converted(checkconvert) && is_positive(Context_->tl))) {
               fprintf(stderr,"\tERROR: The value given for the Fraction of Diffuse light (-F) is not correct: %s\n",argv[1]);
	       return 1;
	    }
            if (Context_->f_diff > 1.) {
               fprintf(stderr,"\tERROR: the Fraction of Diffuse light is greater than one.\n");
	       return 1;
	    }
	    argc--;argv++;
	    break;
         /* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	  * True (Domain Averaged) Leaf Area Index
	  * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
	 case 'l' : 
            CheckTrailingArgument;
            if (Context_->lai != startvalue) {
               fprintf(stderr,"\tERROR: the Leaf Area is set more than once: %s %s\n",argv[0],argv[1]);
	       return 1;
	    }
	    Context_->lai = strtof(argv[1],&checkconvert);
            if (!(string_converted(checkconvert) && is_positive(Context_->lai))) {
               fprintf(stderr,"\tERROR: The value given for the Leaf Area Index (-l) is not correct: %s\n",argv[1]);
	       return 1;
	    }
	    argc--;argv++;
	    break;
         /* ++++++++++++++++++++++++++++++++++++++
	  * Structure factors zeta and zeta_star
	  * ++++++++++++++++++++++++++++++++++++++*/
	 case 'z' : 
            CheckTrailingArgument;
	    
	    /* make the difference between zeta (-z) and zeta_star (-z*) flags */
	    if (strlen(argv[0]) == 2) {
	       /* only one letter (first is always '-' and we know the second is a z) */
	       
               if (Context_->zeta != startvalue) {
                  fprintf(stderr,"\tERROR: the Structure Factor zeta is set more than once: %s %s\n",argv[0],argv[1]);
	          return 1;
	       }
               Context_->zeta = strtof(argv[1],&checkconvert);
               if (!(string_converted(checkconvert) && is_positive(Context_->zeta))) {
                  fprintf(stderr,"\tERROR: The value given for the Structure Factor zeta (-z) is not correct: %s\n",argv[1]);
	          return 1;
	       }
	    } else if ((argv[0][2]=='*') && (strlen(argv[0]) == 3)) {
	       /* this flag defines the zeta star */
               if (Context_->zeta_star != startvalue) {
                  fprintf(stderr,"\tERROR: the Structure Factor zeta* is set more than once: %s %s\n",argv[0],argv[1]);
	          return 1;
	       }
	       Context_->zeta_star = strtof(argv[1],&checkconvert);
               if (!(string_converted(checkconvert) && is_positive(Context_->zeta_star))) {
                  fprintf(stderr,"\tERROR: The value given for the Structure Factor zeta* (-z*) is not correct: %s\n",argv[1]);
	          return 1;
	       }
	    } else {
	       /* neither '-z' nor '-z*'. this is not allowed */
               fprintf(stderr,"\tERROR: Unknown argument tag: %s. Try -h for help.\n",argv[0]);
	       return 1;
	    }
	    argc--;argv++;
	    break;
         /* ++++++++++++++++++++
	  * Usage/Help message
	  * ++++++++++++++++++++*/
	 case 'h' : 
	    DisplayUsageMessage();
	    exit(EXIT_SUCCESS);
	    break;
         /* ++++++++++++++++++++
	  * Version number
	  * ++++++++++++++++++++*/
         case 'v' : 
	    fprintf(stdout,"Version %d.%d\n",MAIN_VERSION_NUMBER,SECONDARY_VERSION_NUMBER);
	    exit(EXIT_SUCCESS);
	    break;
         /* ++++++++++++++++++++
	  * display (on stderr) the output columns' tags.
	  * ++++++++++++++++++++*/
         case 'O' : 
            if (Context_->display_ident_strings != 0) {
	       fprintf(stderr,"\tWARNING: the -O flag is used more than once.\n");
	    }
	    Context_->display_ident_strings = 1;
	    break;
         /* ++++++++++++++++++++++++++++++++++++++++
	  * Output format for the resulting fluxes
	  * ++++++++++++++++++++++++++++++++++++++++*/
	 case 'o' : 
	    if (Context_->outfmt) {
               /* the option has already been specified. It is not possible */
               fprintf(stderr,"\tERROR: The output format has been given twice. (beware that -A and -o are incompatible).\n");
	       return 1;
	    }
            CheckTrailingArgument;
	    /* Call an internal routine */
	    if (ParseOutputFmt(Context_,argv[1])) {
	       fprintf(stderr,"\tERROR: The output format given (-o) is not valid.\n");
	       return 1;
	    }
            argc--;argv++;
	    break;
         /* +++++++++++++++++++++++++++++++++++++++++++++++
	  * Output format: shortcut to get all the fluxes
	  * +++++++++++++++++++++++++++++++++++++++++++++++*/
	 case 'A' : 
	    if (Context_->outfmt) {
               /* the option has already been specified. It is not possible */
               fprintf(stderr,"\tERROR: The output format has been given twice (beware that -A and -o are incomopatible).\n");
	       return 1;
	    }
	    /* Call an internal routine */
	    if(AddAllOutputFmt(Context_)) {
	       fprintf(stderr,"\tERROR: The installation of one of the output formats (-A) raised an error.\n");
	       return 1;
	    }
	    break;
         /* ++++++++++++++++
	  * Non valid flag
	  * ++++++++++++++++*/
	 default :
            fprintf(stderr,"\tERROR: Unknown argument tag: %s. Try -h for help.\n",argv[0]);
	    return 1;
      }
   }
   
   
   /* first test for rl and tl: if the user did not specify anything, we use the 
    * default values. */
   if (needed_rt == 0) {
      Context_->rl = DEFAULT_RL/100.;
      Context_->tl = DEFAULT_TL/100.;
      fprintf(stderr,"\tWARNING: The leaf reflectance and transmittance (-r/-t/-d/-w) were not set.\n");
      fprintf(stderr,"\t\t(cont.) Default values are used: rleaf=%.3f and tleaf=%.3f\n",Context_->rl,Context_->tl);
   } else {
      /* We now need to derive rl and tl from the 2 arguments: pair of rl,tl,w0,rat */
      if (needed_rt < 2) {
         fprintf(stderr,"\tERROR: Not enough information to retrieve the Leaf Reflectance and Transmittance.\n");
         return 1;
      } else if (needed_rt > 2) {
         fprintf(stderr,"\tERROR: Too many information have been given to retrieve the Leaf Reflectance and Transmittance.\n");
         fprintf(stderr,"\tAs they might be inconsistent, stop here.\n");
         return 1;
      }

      /* From here, we know that exactly 2 of the 4 arguments have been set. */
      if (Context_->rl == startvalue || Context_->tl == startvalue) {
         if (w0 != startvalue) {
            if (Context_->rl != startvalue) {
	       /* rl and w0 */
	       Context_->tl = w0 - Context_->rl;
	       if (is_negative(Context_->tl)) {
                  fprintf(stderr,"\tERROR: The given parameters imply a negative value for the Leaf Transmitance.\n");
	          return 1;
	       }
	    } else if (Context_->tl != startvalue) {
	       /* tl and w0 */
	       Context_->rl = w0 - Context_->tl;
               if (is_negative(Context_->rl)) {
                  fprintf(stderr,"\tERROR: The given parameters imply a negative value for the Leaf Reflectance.\n");
	          return 1;
	       }
	    } else {
	       /* w0 and rat */
	       Context_->tl = w0 / (rat + 1.);
	       Context_->rl = Context_->tl * rat;
	    }
         } else if (rat != startvalue) {
            if (Context_->rl != startvalue) {
	       /* rl and rat */
	       Context_->tl = Context_->rl / rat;
	    } else if (Context_->tl != startvalue) {
	       /* tl and rat */
	       Context_->rl = Context_->tl * rat;
	    } else {
	       /* w0 and rat */
	       Context_->tl = w0 / (rat + 1.);
	       Context_->rl = Context_->tl * rat;
	    }
         }
      }
      /* Last possibility is rl and tl already set: nothing to do.*/
   }
   /* Rl and Tl are set (either by default values) of by the flags.*/


   /* If the RT model has not been choosen by the user, the default will be the general 2-streams model */
   if (Context_->RTmodel == NULL) {
      Context_->RTmodel = Full_model;
   }

   /* If the output format has not been specified, use the default "%Alb %Tran %Abs" */
   if (Context_->outfmt == NULL) {
      if (InsertDefaultOutputFmt(Context_)) {
         fprintf(stderr,"\tERROR: Could not create the default output format.\n");
      }
   }
   return 0;
}

void VerboseRTContext(struct RTContext *Context_)
{
   fprintf(stderr,"\tTTA = %.4f\n",Context_->tta);
   fprintf(stderr,"\tLAI = %.4f\n",Context_->lai);
   fprintf(stderr,"\tZETA = %.4f\n",Context_->zeta);
   fprintf(stderr,"\tZETA_STAR  = %.4f\n",Context_->zeta_star);
   fprintf(stderr,"\tRG  = %.4f\n",Context_->rg);
   fprintf(stderr,"\tRL  = %.4f\n",Context_->rl);
   fprintf(stderr,"\tTL  = %.4f\n",Context_->tl);
   fprintf(stderr,"\tTotal Albedo = %.4f\n",Context_->Alb);
   fprintf(stderr,"\tTotal Transmission = %.4f\n",Context_->Tran);
   fprintf(stderr,"\tTotal Absorption by vegetation = %.4f\n",Context_->Abs);
}

int ParseOutputFmt(struct RTContext *Context_,char *str)
{
   char fmt[MAXFMTSIZE];
   char ident[MAXIDENTSIZE];
   char *fmt_tok; 
   char c1,c2;
   int i,j,jm,ii,tt,cnt;
   int token = 0;
   int retcode = 0;

   OutFmt_t *head,*curr,*oldcurr;
   
   /* Handle the case were the given string is empty */
   if (strlen(str) == 0) 
      return 1;

   /* First thing, we copy the string in a buffer of maximum MAXFMTSIZE */
   /* so we have to check for the size */
   strncpy(fmt,str,(size_t)MAXFMTSIZE);
   if (fmt[MAXFMTSIZE-1]!='\0') {
      /* the MAXFMTSIZE characters were not enough to handle the given output format */
      fprintf(stderr,"\t\tERROR: The output format given is too long. At most %d characters are allowed.\n",MAXFMTSIZE);
      return 1;
   }
   /* Now parse the string. */
   oldcurr = curr = head = NULL;
   i = 0;
   while ((c1=fmt[i++]) != '\0') {
      j = i;
#ifdef OUTFMT_DEBUG
      printf("parse <%c> ; i=%d ; token=%d\n",c1,i,token);
#endif
      if (c1=='%' && token==0) {
#ifdef OUTFMT_DEBUG
         printf("\tinitiate a token search.\n");
#endif
	 token = 1;
         while ((fmt[j] != ' ') && (fmt[j] != '\0') && (fmt[j] != ',')) {
#ifdef OUTFMT_DEBUG
	    printf("\t<%c> is not a token completion ; j=%d\n",fmt[j],j); 
#endif
	    j++;
	 }
#ifdef OUTFMT_DEBUG
	 printf("\t<%c> indicates a token completion ; j=%d\n",fmt[j],j); 
#endif
	 
#ifdef OUTFMT_DEBUG
         printf("\t\tTOKEN: <"); 
	 for (ii=i;ii<j;ii++) 
	    printf("%c",fmt[ii]);
         printf(">;\n");
#endif
	 
	
	    cnt = 0; jm = (((j-i+1)<=MAXIDENTSIZE)? j : (i+MAXIDENTSIZE));
	    for (ii=i;ii<jm;ii++) {
#ifdef OUTFMT_DEBUG
               printf("Fill ident[%d] with %c.\n",cnt,fmt[ii]);
#endif
	       ident[cnt++]=fmt[ii];
	    }
	    ident[cnt]='\0';

#ifdef OUTFMT_DEBUG
	    printf("\t\tTOKEN as string: <%s>;\n",ident);
#endif
           oldcurr = curr;    
           if (jm != j) {
#ifdef OUTFMT_DEBUG
	       printf("\tSize is too long : it cannot be a valid token.\n"); 
#endif
	   } else {
	   
	    for (tt=0;tt<NBIDENT;tt++) {
#ifdef OUTFMT_DEBUG
	       printf("\t\t\t\t(%d) compare with: <%s>\n",tt,valid_ident[tt]);
#endif 
	       if (!strcmp(ident,valid_ident[tt])) {
		  curr = AddOutFmt_token(&head,curr,tt,Context_);
#ifdef OUTFMT_DEBUG
		  printf("\t\t\t<%s> is valid. ident=%d head=%p curr=%p\n",ident,tt,head,curr);
#endif
		  /* halt the search for tokens as soon as we found a valid one in the list.*/
		  break;
	       }
	    }
         }
	 if (curr == oldcurr) {
	    /* The token was not a valid one */
	    fprintf(stderr,"\t\tERROR: The token <%s> (< and > removed) is not a valid token for the output format.\n",ident);
	    retcode = 1;
	 }
      } else if ((c1 != ' ') && (c1 != ',')) {
         fprintf(stderr,"\t\tERROR: The character <%c> (< and > removed) is not a valid delimiter in the output format.\n",c1);
	 retcode = 1;
      }
	 
      token = 0;
      i = j;
   }
	 
   if (InsertStandardInFmt(&head,Context_)) {
      fprintf(stderr,"\t\tERROR: Could not insert the standard input conditions.\n");
      retcode = 1;
   }

   /* attach the output format created to the Context pointer. */
   Context_->outfmt = head; 
   
   return retcode;
}

void PrintRTContext_tags(struct RTContext *Context_)
{
   int i=1;
   OutFmt_t *curr = Context_->outfmt;
   
   while (curr != NULL) {
      if ( curr->index < SZA_IDENT ) {
	 fprintf(stderr,"%s",valid_ident[curr->index]);
      } else {
	 fprintf(stderr,"%s",input_ident[curr->index-SZA_IDENT]);
      }
      fprintf(stderr,"{%d}",i);
      fprintf(stderr,"  ");
      curr = curr->next;
      i++;
   }
   fprintf(stderr,"\n");   
}

void PrintRTContext(struct RTContext *Context_)
{
   OutFmt_t *curr = Context_->outfmt;
   
   if (Context_->display_ident_strings == 1) 
   	PrintRTContext_tags(Context_);

   while (curr != NULL) {
      printf(curr->fmt,*(curr->val));
      printf(" ");
      curr = curr->next;
   }
   printf("\n");
}


void FillUpRTContext(struct RTContext *Context_,float val)
{
   Context_->tta = Context_->lai = Context_->rl = Context_->tl = val;
   Context_->rg  = val;Context_->zeta = val;Context_->zeta_star = val;
   Context_->f_diff = val;
   Context_->RTmodel = NULL;
   Context_->display_ident_strings = 0;
   Context_->outfmt = NULL;
}

int CheckRTContext(struct RTContext *Context_)
{
   double zetaval;
   int  nbarg = 0;
   int  ok = 0;

   /* Rl and Tl are set, let's check their value.                           */
   /* the single scattering albedo must be less than 1, but cannot be zero. */
   if (Context_->rl + Context_->tl - EPSILON > 1.) {
      fprintf(stderr,"\tERROR: the single scattering albedo is more than one.\n");
      fprintf(stderr,"\t\t(cont.) rleaf=%.7f and tleaf=%.7f\n",Context_->rl,Context_->tl);
      ok = 1; 
   }
   
   /* Check the true LAI: <LAI> */   
   if (Context_->lai == NOVALUE) {
      Context_->lai = (double)DEFAULT_LAI/10;
      fprintf(stderr,"\tWARNING: The Leaf Area Index <LAI> (-l) is not set. Use default value <LAI>=%.1f\n",Context_->lai); 
      nbarg++;
   } else {
      nbarg++;
      /* issue a warning is the LAI is more than MAXLAI */
      if (Context_->lai > (float)MAXLAI) 
	 fprintf(stderr,"\tWARNING: The Leaf Area Index is more than %.1f\n",(float)MAXLAI);
   }

   /* Check the structure factors: zeta and zeta_star */
   if (Context_->zeta == NOVALUE && Context_->zeta_star == NOVALUE) 
      /* If none was set, we have a homogeneous canopy zeta=zeta_star=1 */
      Context_->zeta = Context_->zeta_star = 1.;
   else if ((Context_->zeta == NOVALUE && (zetaval = Context_->zeta_star) != NOVALUE)
	 || ((zetaval = Context_->zeta) != NOVALUE && Context_->zeta_star == NOVALUE)) 
      /* zeta or zeta_star was set. Consider a canopy with a structure factor non depending on the 
       * Sun angle. */
      Context_->zeta = Context_->zeta_star = zetaval;

   /* now deal with the illumination geometry */
   if (Context_->tta == NOVALUE) {
      /* we need to activate the default illumination ; but -F is set: error.*/
      if (Context_->f_diff != NOVALUE) {
	 fprintf(stderr,"\tERROR: The -F flag cannot appear alone: it should be used in conjonction with (-s).\n");
	 fprintf(stderr,"\t\t(cont.) Note: purely isotropic illumination is set with -s iso\n");
	 ok=1;
      } else {
         Context_->tta = (double)DEFAULT_TTA;
         Context_->f_diff = 0.000;
         Context_->tta_rad = deg2rad(Context_->tta);
         fprintf(stderr,"\tWARNING: The Sun Zenith Angle (-s) is not set. Use default collimated source at %.1f degrees.\n",
	       Context_->tta);
         nbarg++;
      }
   } else if (Context_->tta == ISOLIGHT) {
      /* check that no -F flag was used */
      if (!(Context_->f_diff == NOVALUE)) {
	 fprintf(stderr,"\tERROR: When -s iso is used, the -F flag cannot appear.\n");
	 ok=1;
      } else {
	 /* in this situation we have a valid illumination */
	 Context_->f_diff = 1.;
	 nbarg++;
      }
   } else {
      /* a valid numerical value was provided for the Sun zenith angle. Check this. */
      /* We rely on the parsing stage to be sure that f_diff is in ]0,1[ */
      nbarg++;  
      /* the given Sun zenith angle is taken to be positive and must be between 0 and 90. */
      /* It is converted to radians.                                                      */
      Context_->tta=fabs(Context_->tta);
      if (Context_->tta >= (90.-EPSILON)) {
	 fprintf(stderr,"\tERROR: the Sun Zenith angle has to be in ] -90 ; +90 [\n");
         ok=1;
      }
      /* if no -F flag was used, the source is collimated, hence f_diff = 0.*/
      if (Context_->f_diff == NOVALUE) 
	 Context_->f_diff = 0.;
      
      Context_->tta_rad = deg2rad(Context_->tta);
   }

   /* Check the background reflectance: */
   if (Context_->rg == NOVALUE) {
      /* the background reflectance was not set. It defaults to zero (black background model) */
      fprintf(stderr,"\tWARNING: The Background reflectange (-g) is not set. Use default black surface (Rgbd = 0)\n");
      Context_->rg = 0.;
   }
   
   /* Were all the needed arguments given? */
   return ((ok == 1) || (nbarg != 2));

}


OutFmt_t *CreateOutFmt_token(unsigned int token_index,char *fmt,struct RTContext *Context_)
{
   OutFmt_t *tmp;
   
   /* first allocate the token handler */
   if ((tmp=malloc(sizeof *tmp))==NULL) {
      fprintf(stderr,"\t\t\tERROR: Memory error in trying to allocate %u bytes.\n",(unsigned int)sizeof(*tmp));
      return NULL;
   }
   /* immediatly set the next to NULL */
   tmp->next = NULL;

   /* associate the token to a value in the RTContext */
   /* {"Alb","Abs","Ttot","Tdif"} */
   switch (token_index) {
      case ALB_IDENT  : tmp->val = &(Context_->Alb) ;break;
      case ABS_IDENT  : tmp->val = &(Context_->Abs);break;
      case TRAN_IDENT : tmp->val = &(Context_->Tran)  ;break;
      case ALBT1_IDENT  : tmp->val = &(Context_->Alb_t1) ;break;
      case ABST1_IDENT  : tmp->val = &(Context_->Abs_t1);break;
      case TRANT1_IDENT : tmp->val = &(Context_->Tran_t1)  ;break;
      case ALBT2_IDENT  : tmp->val = &(Context_->Alb_t2) ;break;
      case TRANT2_IDENT : tmp->val = &(Context_->Tran_t2)  ;break;
      case ALBT3_IDENT  : tmp->val = &(Context_->Alb_t3) ;break;
      case ABST3_IDENT  : tmp->val = &(Context_->Abs_t3);break;
      case TRANT3_IDENT : tmp->val = &(Context_->Tran_t3)  ;break;
      case RBGD1_IDENT : tmp->val = &(Context_->Rbgd1)  ;break;
      case SZA_IDENT  : tmp->val = &(Context_->tta)   ;break;
      case LAI_IDENT  : tmp->val = &(Context_->lai)   ;break;
      case RL_IDENT   : tmp->val = &(Context_->rl)    ;break;
      case TL_IDENT   : tmp->val = &(Context_->tl)    ;break;
      case RG_IDENT   : tmp->val = &(Context_->rg)    ;break;
      case ZETA_IDENT : tmp->val = &(Context_->zeta)  ;break;
      case ZETA_STAR_IDENT : tmp->val = &(Context_->zeta_star)  ;break;
      case FRAC_DIFF_IDENT : tmp->val = &(Context_->f_diff)  ;break;  
      default: tmp->val = NULL;
	       fprintf(stderr,"ERROR: Invalid token index: %u\n",token_index);
	       free(tmp);
	       return NULL;
   }
   /* associate the output precision */
   tmp->fmt = fmt;

   /* keep track of the given index */
   tmp->index = token_index;
   
   return tmp;
}

OutFmt_t *AddOutFmt_token(OutFmt_t **head,OutFmt_t *current,unsigned int token_index,struct RTContext *Context_)
{
   OutFmt_t *tmp;
   
   if (*head == NULL) {
      /* This is the first token to be added to the list */
      *head = CreateOutFmt_token(token_index,DefaultOutPrecisionString,Context_);
      if (*head == NULL)
	 fprintf(stderr,"\t\tERROR: Cannot create Output Format from token.\n");
      
      return *head;
   } else {
      /* This is not the first token */
      tmp = CreateOutFmt_token(token_index,DefaultOutPrecisionString,Context_);
      current->next = tmp;
      return tmp;
   }
}

int InsertStandardInFmt(OutFmt_t **head,struct RTContext *Context_)
{
   OutFmt_t *out_sza,*out_lai,*out_rl,*out_tl,*out_rg;
   OutFmt_t *out_zeta,*out_zeta_star,*out_fdiff;
   
   if (!(out_sza = CreateOutFmt_token(SZA_IDENT,DefaultSZAPrecisionString,Context_))) {
      fprintf(stderr,"\t\tERROR: Cannot create Output Format for SZA.\n");
      return 1;
   }
   if (!(out_fdiff  = CreateOutFmt_token(FRAC_DIFF_IDENT,DefaultLAIPrecisionString,Context_))) {
      fprintf(stderr,"\t\tERROR: Cannot create Output Format for F_DIFF.\n");
      return 1;
   }
   if (!(out_lai = CreateOutFmt_token(LAI_IDENT,DefaultLAIPrecisionString,Context_))) {
       fprintf(stderr,"\t\tERROR: Cannot create Output Format for LAI.\n");
      return 1;
   }     
   if (!(out_rl  = CreateOutFmt_token(RL_IDENT,DefaultLOPPrecisionString,Context_))) {
      fprintf(stderr,"\t\tERROR: Cannot create Output Format for RL.\n");
      return 1;
   }
   if (!(out_tl  = CreateOutFmt_token(TL_IDENT,DefaultLOPPrecisionString,Context_))) {
      fprintf(stderr,"\t\tERROR: Cannot create Output Format for TL.\n");
      return 1;
   }
   if (!(out_rg  = CreateOutFmt_token(RG_IDENT,DefaultLOPPrecisionString,Context_))) {
      fprintf(stderr,"\t\tERROR: Cannot create Output Format for RG.\n");
      return 1;
   }
   if (!(out_zeta  = CreateOutFmt_token(ZETA_IDENT,DefaultLAIPrecisionString,Context_))) {
      fprintf(stderr,"\t\tERROR: Cannot create Output Format for ZETA.\n");
      return 1;
   }
   if (!(out_zeta_star  = CreateOutFmt_token(ZETA_STAR_IDENT,DefaultLAIPrecisionString,Context_))) {
      fprintf(stderr,"\t\tERROR: Cannot create Output Format for ZETA_STAR.\n");
      return 1;
   }
   
   /* inserting them before head */
   out_tl->next         = *head;
   out_rl->next         = out_tl;
   out_rg->next         = out_rl;
   out_zeta_star->next  = out_rg;
   out_zeta->next       = out_zeta_star;
   out_lai->next        = out_zeta;
   out_fdiff->next      = out_lai;
   out_sza->next        = out_fdiff;
   *head                = out_sza; 
   
   return 0;
}

int InsertDefaultOutputFmt(struct RTContext *Context_)
{
   OutFmt_t *out_alb,*out_ttot,*out_abs;

   /* Create the 3 standard output (albedo,total transmission,absorption in the layer)*/
   if (!(out_alb = CreateOutFmt_token(ALB_IDENT,DefaultOutPrecisionString,Context_))) {
      fprintf(stderr,"\t\tERROR: Cannot create Output Format for the Albedo.\n");
      return 1;
   }
   if (!(out_ttot = CreateOutFmt_token(TRAN_IDENT,DefaultOutPrecisionString,Context_))) {
       fprintf(stderr,"\t\tERROR: Cannot create Output Format for the Total Transmission.\n");
      return 1;
   }     
   if (!(out_abs  = CreateOutFmt_token(ABS_IDENT,DefaultOutPrecisionString,Context_))) {
      fprintf(stderr,"\t\tERROR: Cannot create Output Format for the Absorption.\n");
      return 1;
   }

   /* inserting them before head */
   out_ttot->next    = NULL;
   out_abs->next   = out_ttot;
   out_alb->next    = out_abs;
   Context_->outfmt = out_alb;
   
   if (InsertStandardInFmt(&(Context_->outfmt),Context_)) {
      fprintf(stderr,"\t\tERROR: Cannot insert the standard input format in the output string.\n");
      return 1;
   }
   
   return 0;
}

int AddAllOutputFmt(struct RTContext *Context_)
{
   OutFmt_t *out_fmt,*curr;
   unsigned int t;
   
   for (t=0 ; t<NBIDENT ; t++) {
      if (!(out_fmt = CreateOutFmt_token(t,DefaultOutPrecisionString,Context_))) {
         fprintf(stderr,"\t\tERROR: Cannot create Output Format for the variable %u.\n",t);
         return 1;
      }
      
      if (t == 0)
	 Context_->outfmt = out_fmt;
      else 
	 curr->next = out_fmt;
      
      curr=out_fmt;

   }

   if (InsertStandardInFmt(&(Context_->outfmt),Context_)) {
      fprintf(stderr,"\t\tERROR: Cannot insert the standard input format in the output string.\n");
      return 1;
   }
   
   return 0;
}

int FreeRTContext(struct RTContext *Context_)
{

   OutFmt_t *curr;
   
   while (Context_->outfmt != NULL) {
      curr = (Context_->outfmt)->next;
      free(Context_->outfmt);
      Context_->outfmt = curr;
   }
   return 0;
}
