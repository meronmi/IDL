/* ---------------------------------------------------
 * File            : 2streamsRTmodels.h                       
 * Author          : Lavergne Thomas                          
 * Creation Date   : 16 Septembre 2004                            
 * Purpose         :                                  
 * 	Prototypes for the various 2stream models implemented.                                      
 * ---------------------------------------------------
 */

#ifndef STREAMSMODELS_H
#define STREAMSMODELS_H


/* Apply T1T2T3model with all orders of scattering */
extern int  Full_model(struct RTContext *ct);
/* Apply T1T2T3model with first + second orders of scattering in the layer*/
extern int  Dble_model(struct RTContext *ct);
/* Apply T1T2T3model with first order of scattering (single collided) in the layer*/
extern int  Sgle_model(struct RTContext *ct);
/* Apply T1T2T3model with a turbid model and with a planophile LND */
extern int  Plano_model(struct RTContext *ct);
/* Apply T1T2T3model with a turbid model and with an erectophile LND */
extern int  Erecto_model(struct RTContext *ct);

#endif /* 2STREAMSMODEL_H */
