
typedef struct OutFmt_t {
   double *           val;
   char              *fmt;
   int                index;
   struct OutFmt_t   *next;
} OutFmt_t;



/* RT Context struct type. It holds all parameters (input and output) to be handled in this code. */
struct RTContext {
   /* ILLUMINATION */
   double f_diff;       /* fraction of diffuse to total downward flux */
   double tta;          /* The redundancy tta (degrees) and tta_rad (radians)                */
   double tta_rad;      /* avoids to deprecate the value through various rounding operation. */

   /* VEGETATION AND BACKGROUND PROPERTIES */
   double lai;          /* Note: this is the true LAI of the canopy <LAI>. 
			   The LAI entering the 2stream model (LAI_tilde and LAI_tilde_star) 
			   are computed with zeta and zeta_star */
   double zeta;
   double zeta_star;
   double rl;
   double tl;
   double rg;

   /* RESULTS (RADIATIVE FLUXES) */
   double Alb;
   double Tran;
   double Abs;
   double Alb_t1;
   double Tran_t1;
   double Abs_t1;
   double Alb_t2;
   double Tran_t2;
   double Alb_t3;
   double Tran_t3;
   double Abs_t3;
   double Rbgd1;

   /* OUPUT FORMAT */
   OutFmt_t *outfmt;

   /* OTHER VARIABLES */
   int display_ident_strings;

   /* MODEL TO BE USED */
   int (*RTmodel)(struct RTContext *);
};

typedef int (*RTmodel_F)(struct RTContext *);

/* Prototypes to local procedures. */ 
extern int   ParseCommandLine(int, char **,struct RTContext *,float); 
extern void  VerboseRTContext(struct RTContext *);
extern void  PrintRTContext(struct RTContext *);
extern int   CheckRTContext(struct RTContext *);
extern int   FreeRTContext(struct RTContext *);
extern void  FillUpRTContext(struct RTContext *,float);
