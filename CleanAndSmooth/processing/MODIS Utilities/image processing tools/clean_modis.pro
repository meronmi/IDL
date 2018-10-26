Pro clean_MODIS
STARTTIME = SYSTIME(1)
;****************************************************
;MODIS terra
;path='K:\ILRI\MODIS_C5\bil_files'
;ns = 3808
;nl =  4658
;nb = 301
;fn_index_image = path+'\'+'MODIS_C5_Terra_NDVI_bil'     ;fn stands for filename
;fn_qc_image =    path+'\'+'MODIS_C5_Terra_QC_byte_bil'
;fn_ui_image =    path+'\'+'MODIS_C5_Terra_UI_byte_bil'           

;MODIS aqua
path='K:\ILRI\MODIS_C5_aqua\bil_files'
ns = 3808
nl =  4658
nb = 242
fn_index_image = path+'\'+'MODIS_C5_Aqua_NDVI_bil'     ;fn stands for filename
fn_qc_image =    path+'\'+'MODIS_C5_Aqua_QC_byte_bil'
fn_ui_image =    path+'\'+'MODIS_C5_Aqua_UI_byte_bil'

;****************************************************
;build the cloud mask (it's actually a boolean mask used to keep QC<=1 and UI<=5 
;and the fapar file cleaned
fn_index_image_clean=fn_index_image+'_cl'


;do not scale, keep integer with x10000 
res=MODISmask_and_scale(fn_index_image, fn_qc_image, fn_ui_image, fn_index_image_clean, $
                    fn_cloudmask, ns, nl, nb, 1, 5)

;smooth it with Chen 2004 RSE

fn_index_image_clean_sm=fn_index_image_clean+'_Sm_Ch'

res=sg_smooth(fn_index_image_clean, fn_index_image_clean_sm, ns, nl, nb)

; Evaluation of processing time
ELAPSED_TIME = FIX(SYSTIME(1) - STARTTIME)
MINUTES = ELAPSED_TIME / 60
SECS=ELAPSED_TIME MOD 60
PRINT, 'prepare_data PROCESSING TOOK :'+STRCOMPRESS(MINUTES)+' MINUTES AND'+STRCOMPRESS(SECS)+' SECONDS'

End