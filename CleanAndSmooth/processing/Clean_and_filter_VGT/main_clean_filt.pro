;Preparation of data
;Objective: to prepare a BIL layer stack of fAPAR and blu band decadal data (blu is for the cleaning procedure).
;- With spirit (Basic Tool – Ancillary – Create VAR/MTA) create the ENVI metafile of fAPAR (*a) and blu band (*1)
;- With ENVI save a BIL (an intermediate bsq can be saved) of the region of interest (can be very big, then a mask is used to process selected countries)
;Cleaning / Smoothing of data
;- Run scale_bil (main_clean_filt)with setting relevant info (‘use’ refers to the type of data, fAPAR, blu band, etc; ‘run’ refers to directory settings and file dimensions) twice, for blu band and then for fapar.
;- Run clean_filt, only filenames have to be edited in the code




Pro scale_bil
STARTTIME = SYSTIME(1)
use='ltdr';'spiritsHisProb';'bokuzOF';ltdr';'emodis';'spi';'emodis';'spi';'mars_ndvi_vgt_global';'mars_ndvi_vgt_global';'mars_ndvi_vgt_global';'mars_fapar_vgt_global';'mars_fapar_modis_europe';'mars_blu_vgt_global';'mars_fapar_vgt_global'
run='ltdr';'spiritsHisProb';'globoku';'som';'africa_ltdr';'niger';'lta' ; 'iga_mod';'Tunisia2014single';'wa';'Tunisia';'igad'; 'tunisiaSWETS';'nigerSWETS';'tunisiaSWETS';'new_niger';'g2' ;'niger'
;!!!!!!!!!!!!!!!!BE CARE ABOUT THE FOLLOWING:
useBPinverseScaling = 0 ; set to 1 to make physical val = (DN-OFF)/SCAL
                        ; set it to 0 to make physical val = DN*SCAL+OFF
;**********************************************************
;DEFINE GAIN AND OFFSET FOR SCALING
dt = 1
case use of
  'boku': begin
    gain=0.0048  & offset=-0.2 & maxval=250 & minval=0
  end
  'spiritsHisProb': begin
    gain=0.5  & offset=-5  & maxval=250 & minval=0
  end
  'bokuzOF': begin
    gain=0.02  & offset=-2.5  & maxval=250 & minval=0
   end 
  'ltdr': begin
    gain=1.0/10000.0  & offset=0  & maxval=10000 & minval=0
   end
  'zfa': begin
    gain=0.02  & offset=-2.5  & maxval=250 & minval=0
  end
  'spi': begin                      
    gain=0.001  & offset=0.0  & maxval=10000 & minval=-10000
  end
  'emodis': begin                      ;##fapar vgt
    gain=1/100.0  & offset=-1.0  & maxval=200 & minval=101
  end
  'probav_ndvi': begin                      ;##fapar vgt
    gain=0.005  & offset=0.0  & maxval=250
  end
  'mars_fapar_vgt_global': begin                      ;##fapar vgt
    gain=0.005  & offset=0.0  & maxval=250
  end
  'mars_ndvi_vgt_global': begin                       ;##ndvi
    gain=0.004  & offset=-0.08 & maxval=250 & minval=0
  end
  'mars_blu_vgt_global': begin                        ;##blu in ref units
    gain=(0.25)/100.0 & offset=0.0 & maxval=250
  end
  'mars_fapar_modis_europe': begin                    ;##fapar modis
    gain=0.005  & offset=0.0  & maxval=200
  end
  'clement': begin
    gain=1/250.0  & offset=0.0  & maxval=250
  end
  'g2_fapar': begin                                   ;##fapar BioPar
    gain=1/250.0  & offset=0.0 & maxval=235
  end
  'g2_ndvi': begin                                   ;##fapar BioPar
    gain=250.0  & offset=25.0 & maxval=250
  end
endcase
;**********************************************************
;DEFINE path, filename and file dimensions
case run of
  'boku2':begin
    path='\\ies\d5\asap\boku\V1\pheno\LTA2002-2016'
    image_file='AAA_OF_LTA2002_2016_bil'
    ns=40320 & nl=14673 & nb=36 & dt=1       ; number of samples, lines, bands
  end
  'boku1':begin
    path='\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA'
    image_file='OF2003-2016_bil'
    ns=1920 & nl=698 & nb=504 & dt=1       ; number of samples, lines, bands
  end
  'lta_boku':begin
    path='Y:\remote_sensing\boku\Test Michele\LTAs'
    image_file='LTA_BOKU_OF_bil'
    ns=40320 & nl=14673 & nb=36 & dt=1       ; number of samples, lines, bands
  end
  'lta_vgt':begin
    path='Y:\remote_sensing\boku\Test Michele\LTAs'
    image_file='LTA_VGT_smooth_bil'
    ns=40320 & nl=14673 & nb=36 & dt=1       ; number of samples, lines, bands
  end
  'lta_metop':begin
    path='Y:\remote_sensing\boku\Test Michele\LTAs'
    image_file='LTA_Metop_smooth_bil'
    ns=40320 & nl=14673 & nb=36 & dt=1       ; number of samples, lines, bands
  end
  'spiritsHisProb':begin
    path='\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\NEP of SPIRITS'
    image_file='AAA_nepNfLf_of_spirits_bil'
    ns=1920 & nl=698 & nb=504 & dt=1       ; number of samples, lines, bands
  end
  'globoku':begin
    path='\\ies\d5\asap\TEST_BOKU_CONSOLIDATION_STAGE\THINNED_DATA\Z OF by SPIRITS'
    image_file='AAA_zNfLf_spirits2003-2016_bil'
    ns=1920 & nl=698 & nb=504 & dt=1       ; number of samples, lines, bands
  end
  'wa_tamsat_res2': begin
    path='E:\WA\EWP\FAPAR_Z-score_at_Tamsat_res'
    image_file='AAA_zFAPAR_at_TAMSAT_res_bil'
    ns=1867 & nl=348 & nb=540 & dt=1       ; number of samples, lines, bands
  end
  'wa_tamsat_res3': begin
    path='E:\WA\EWP\EWP_images\standardize with spirits\StandardSpiritsCWP'
    image_file='BBB_SpiritsCWP2000_bil'
    ns=7200 & nl=348 & nb=540 & dt=2       ; number of samples, lines, bands
  end
  'ltdr' : begin
    path='D:\LTDR'
    image_file='LTDR10day_MVC_NDVIstack_V5screened_v3'
    ns=7200 & nl=2780 & nb=1334 & dt=2       ; number of samples, lines, bands
  end
  'africa_ltdr': begin
    path='Y:\users\meronmi\EL NINO Anton\LTDR\Screened data\Africa_subset'
    image_file=0
    ns=1572 & nl=1697 & nb=1246 & dt=2       ; number of samples, lines, bands
  end
  'wa_tamsat_res': begin
    path='E:\WA\EWP\SPI1 from Original Tamsat'
    image_file='AAA_spi9_9835-1234_bil'
    ns=1867 & nl=348 & nb=540 & dt=2       ; number of samples, lines, bands
  end
 'lta': begin
    path='X:\pheno_spirits_test_over_europe'
    image_file='lta_NDVIs_99-13_bil'
    ns=40320 & nl=14673 & nb=36        ; number of samples, lines, bands
  end
  'niger': begin
    path='S:\Actions\FOODSEC\projects\Biomass Sahel\Biomass_Data&Analysis_Niger\Niger_eMODIS'
    image_file='sNE_eM_010101-151221_BIL_160301'
    ns=6673 & nl=3896 & nb=540        ; number of samples, lines, bands
  end
  'wa_anne': begin
    path='S:\Actions\FOODSEC\temporary_share\4 Anne\SG_GGW\ACT_M10'
    image_file='eMODIS_niger_010101_151121_bil'
    ns=2363 & nl=1079 & nb=504        ; number of samples, lines, bands
  end
  'iga_mod': begin
    path='E:\GRACE\emodis\bil1km'
    image_file='eMODIS_NDVI_1km_0101_15_01_eastAfr'
    ns=3473 & nl=3977 & nb=505        ; number of samples, lines, bands
  end
  'kenya': begin
    path='E:\ILRI\pheno'
    image_file='eMODIS_ndvi_01_13'
    ns=3350 & nl=4360 & nb=468        ; number of samples, lines, bands
  end
  'north_afr': begin
    path='S:\Actions\FOODSEC\base_data\remote_sensing\Rabat_joint_experiment_data\NDVI\ENVI FORMAT\ROI North Africa\OVERALP in physical units\VGT'
    image_file='vt_overlap_bil'
    ns=3417 & nl=2185 & nb=22        ; number of samples, lines, bands
  end
  'exHoA': begin
    path='E:\Extended_HoA\NDVI4prob\NDVIs'
    image_file='up1413vgt_ndvi_s_bil'
    ns=4538 & nl=4124 & nb=580        ; number of samples, lines, bands
  end
  'AfrVGT': begin
    path='X:\Active Projects\MARSOP and transition\Proba V 1 km'
    image_file='vgt_1333_1413i'
    ns=7841 & nl=8289 & nb=17        ; number of samples, lines, bands
  end
  'AfrPV': begin
    path='X:\Active Projects\MARSOP and transition\Proba V 1 km'
    image_file='PV_1333_1413i'
    ns=7841 & nl=8289 & nb=17        ; number of samples, lines, bands
  end
  'tunisiaSWETS': begin
    path='K:\Tunisia\VGT_data\bil'
    image_file='Tunisia_VGTas';'Tunisia_VGTas'
    ns=494 & nl=842 & nb=487        ; number of samples, lines, bands
  end
  'Tunisia': begin
    path='K:\Tunisia\VGT_data\bil'
    image_file='Tunisia_VGTa';'Tunisia_VGTas'
    ns=494 & nl=842 & nb=487        ; number of samples, lines, bands
  end
  'Tunisia2': begin
    path='K:\Tunisia\TEST_NDVI\NDVI2012'
    image_file='vt1211i';'Tunisia_VGTas'
    ns=494 & nl=842 & nb=1        ; number of samples, lines, bands
  end
  'TUNBP': begin
    path='K:\Tunisia\BP_data\BIL'
    image_file='TUN_BP_NDVI_9836_1128';'Tunisia_VGTas'
    ns=494 & nl=842 & nb=460        ; number of samples, lines, bands
  end
  'Tunisia2013': begin
    path='K:\Tunisia\VGT_data\2013 extraction'
    image_file='vt1311a.img';'Tunisia_VGTas'
    ns=494 & nl=842 & nb=1        ; number of samples, lines, bands
  end
  'Tunisia2014': begin
    path='E:\Tunisia\VGT_data\bil'
    image_file='Tunisia_VGTi_up1336';'Tunisia_VGTas'
    ns=494 & nl=842 & nb=567        ; number of samples, lines, bands
  end
  'Tunisia2014single': begin
    path='E:\Tunisia\VGT_data\NDVI 2014'
    image_file='vt1413i.img';'Tunisia_VGTas'
    ns=494 & nl=842 & nb=1        ; number of samples, lines, bands
  end
  'nigerSWETS': begin
    path='E:\Tunisia\VGT_data\NDVI 2014'
    image_file='NIGER_VGTaSWETS_bil'
    ns=1600 & nl=900 & nb=1        ; number of samples, lines, bands
  end
  'igad': begin
    ;path='K:\HoA\VGT data SWETS\bil'
    path='K:\HoA\VGT data\raw\bil'
    ;image_file='bil_IGAD_VGT_as'
    image_file='bil_IGAD_VGT_20111010_a'
    ns=3586 & nl=3810 & nb=489        ; number of samples, lines, bands
  end
    'igad2': begin
    path='Q:\HoA\VGT data\raw\bil'
    image_file='bil_IGAD_VGT_20120401_a'
    ns=3586 & nl=3810 & nb=505        ; number of samples, lines, bands
  end
  'ExtHoa': begin
    path='Q:\Extended_HoA'
    image_file='up1224_bil_ExtHoA'
    ns=4538 & nl=4124 & nb=519        ; number of samples, lines, bands
  end
    'igad3': begin
    path='Q:\HoA\SPIRITS_HOA\MTA and bil files'
    image_file='bil_MIN'
    ns=3586 & nl=3810 & nb=36        ; number of samples, lines, bands
  end
  'new_wa': begin
    path='Q:\WA\new_window'
    image_file='bilWA_FAPAR'
    ns=4717 & nl=2690 & nb=510        ; number of samples, lines, bands
  end
  'wa': begin
    path='Q:\WA\raw\bil'
    image_file='bil_WA_VGT_up2012_32_a'
    ns=4716 & nl=1233 & nb=527        ; number of samples, lines, bands
  end
    'sahel': begin
     path='S:\Actions\FOODSEC\projects\Biomass Sahel\NDVI_Data'
    image_file='Vgt_NDVI_smoothed_BIL'
    ns=7841 & nl=1458 & nb=580        ; number of samples, lines, bands
  end
    'hoa2': begin
    path='X:\MODIS WORK'
    image_file='VGT1116_1302bil_a'
    ns=3809 & nl=3809 & nb=59        ; number of samples, lines, bands
  end
    'hoa3': begin
    path='X:\MODIS WORK'
    image_file='MODIS1116_1302bil_a'
    ns=15236 & nl=15236 & nb=59        ; number of samples, lines, bands
  end
  'eu': begin
    path='K:\Europe'
    image_file='EU_VGTa'
    ns=5407 & nl=4650 & nb=497        ; number of samples, lines, bands
  end
    'france': begin
    path='X:\G2\fAPAR\France\FSdata'
    image_file='bil_FS_france0303-0505';'Tunisia_VGTas'
    ns=1120 & nl=1120 & nb=75        ; number of samples, lines, bands
  end
  'syria': begin
    path='X:\SYRIA\vgt'
    image_file='upto09_2014NDVI_syria_bil';
    ns=871 & nl=698 & nb=549        ; number of samples, lines, bands
  end
endcase
;**********************************************************
save, ns, nl, nb, filename=path+'\dims.sav'
;load, scale, flag -> NaN, and save the original bil fAPAR file
filein=path+'\'+image_file
fileout=filein+'_sc'
CASE use OF
  'emodis': BEGIN
     res=load_scale_flag_emodis(filein, fileout, ns, nl, nb , gain, offset, maxval, minval, useBPinverseScaling)
   END
   'spi': BEGIN
      res=load_scale_flag_spi(filein, fileout, ns, nl, nb, dt, gain, offset, maxval, minval, useBPinverseScaling)
   END
   'ltdr': BEGIN
      res=load_scale_flag_ltdr(filein, fileout, ns, nl, nb, dt, gain, offset, maxval, minval, useBPinverseScaling)
   END
   ELSE: BEGIN
    res=load_scale_flag(filein, fileout, ns, nl, nb , gain, offset, maxval, useBPinverseScaling)
   END
ENDCASE

; Evaluation of processing time
ELAPSED_TIME = FIX(SYSTIME(1) - STARTTIME)
MINUTES = ELAPSED_TIME / 60
SECS=ELAPSED_TIME MOD 60
PRINT, 'scale_bil PROCESSING TOOK :'+STRCOMPRESS(MINUTES)+' MINUTES AND'+STRCOMPRESS(SECS)+' SECONDS'
End

Pro clean_filt
STARTTIME = SYSTIME(1)
path='X:\IGAD\VGT data\bil'
restore, path+'\dims.sav' 
fn_index_image=path+'\'+'bil_IGAD_VGTa_98-10_11-14_sc'     ;fn stands for filename 
fn_bluband_image=path+'\'+'bil_IGAD_VGT1_98-10_11-14_sc'
;build the cloud mask 
;fn_cloudmask=path+'\'+image_file+'_cloudmask'
fn_cloudmask=fn_index_image+'_cm'
;and the fapar file cleaned
;fn_fapar_clean=fn_fapar_sc+'_clean2'
fn_index_image_clean=fn_index_image+'_cl'
;fn_bluband_image=path+'\'+fn_bluband_image


          ;res=build_cloudmask(fn_fapar_sc, blu_band_file, fn_fapar_clean, fn_cloudmask, ns, nl, nb, 85, 5, -0.15)
          ;res=build_cloudmask(fn_fapar_sc, blu_band_file, fn_fapar_clean, fn_cloudmask, ns, nl, nb, 90, 5, 0.2)
res=build_cloudmask(fn_index_image, fn_bluband_image, fn_index_image_clean, $
                    fn_cloudmask, ns, nl, nb, 90, 5, 0.2)
;smooth it with Chen 2004 RSE
;fn_fapar_sm=fn_fapar_clean+'_sm_chen2'
fn_index_image_clean_sm=fn_index_image_clean+'_Sm_Ch'

res=sg_smooth(fn_index_image_clean, fn_index_image_clean_sm, ns, nl, nb)

; Evaluation of processing time
ELAPSED_TIME = FIX(SYSTIME(1) - STARTTIME)
MINUTES = ELAPSED_TIME / 60
SECS=ELAPSED_TIME MOD 60
PRINT, 'prepare_data PROCESSING TOOK :'+STRCOMPRESS(MINUTES)+' MINUTES AND'+STRCOMPRESS(SECS)+' SECONDS'

End



;if (run eq 'tunisiaa') then begin
;  path='X:\Tunisia\VGT data\bil'
;  blu_band_file='TUNISIA_VGT1_bil_sc';'bil_niger_1_blue_396d_sc'
;  image_file='TUNISIA_VGTa_bil';'bil_sub_west_sheva_eth_fapar'
;  ns=494                   ; number of samples
;  nl=842                   ; number of lines
;  nb=473                   ; number of bands
;endif
;
;
;if (run eq 'niger') then begin
;  path='X:\Niger'
;  ;image_file='bil_niger_a_fapar_396d'
;  blu_band_file='bil_sub_west_sheva_eth_blu_sc';'bil_niger_1_blue_396d_sc'
;    image_file='bil_niger_clem_fapar_upperenv';'bil_sub_west_sheva_eth_fapar'
;  ns=2018;3586;2018;;101;401;2018                   ; number of samples
;  nl=1038;3810;1038;;601;401;1038                   ; number of lines
;  nb= 396                  ; number of bands
;endif
;
;if (run eq 'blu') then begin  ;just scale the blu
;  path='X:\Niger\VGT data\bil'
;  ;blu band has to be already scaled
;  image_file='NIGER_VGT1_bil'
;  ns=1600                   ; number of samples
;  nl=900                   ; number of lines
;  nb=442                   ; number of bands
;endif
;
;if (run eq 'new_niger') then begin
;  path='X:\Niger\VGT data\bil'
;  ;blu band has to be already scaled
;  blu_band_file='NIGER_VGT1_bil_sc'
;  image_file='NIGER_VGTa_bil'
;  ns=1600                   ; number of samples
;  nl=900                   ; number of lines
;  nb=442                   ; number of bands
;endif
;
;If (run eq 'g2') then begin
;  PATH='X:\G2\fAPAR\France'
;  image_file='bil_BioPar_FR_FAPAR'
;  ns=1120;        ; number of samples
;  nl=1120;        ; number of lines
;  nb= 75                  ; number of bands
;endif
;
;if (run eq 'ferdi') then begin
;  path='X:\ferdinando\fapar'
;  image_file='bil_modis250_fapar_gobron_file'
;  ;image_file='subset_bil_vgt_a_fAPAR_file_NIGER'
;  ;image_file='bil_vgt_a_fAPAR_file_NIGER'
;  ns=5681;401;2018                   ; number of samples
;  nl=10441;401;1038                   ; number of lines
;  nb= 267                  ; number of bands
;end
