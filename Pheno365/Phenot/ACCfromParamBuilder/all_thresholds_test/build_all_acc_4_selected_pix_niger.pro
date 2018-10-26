PRO build_all_acc_4_selected_pix_niger
; build the images (only values for 3x3 box around the field sampling sites in Niger are actually
; claculate) of acc with the following features (associated with naming convention 
; for the output files):
; NAME: Ba{0,1}Gr{0,1}SxEy
; - Ba: baseline subtraction {1} or not {0}
; - Gr: use of gloabl radiatio {1} or not {0}
; - S: x is the treshold use to define the start in the growing phase  
; - E: y is the treshold use to define the start in the decay phase, if y = 200, end is taken from field meas.

;*****************************************************************************
;user settings (here you decide what of the above features has actually to be computed
;base line sub
b_option = [0,1]
;use glob rad
g_option = [0,1]
;percentages use to define SOS
s_option  = [1,5,10,20,30,50,70,90,99]; [1,(INDGEN(19)+1)*5, 99]   ;21 steps: 1,5,10,15, .., 90, 95, 99]
;percentages use to define EOS + fixed doy from files
e_option = [1,5,10,20,30,50,70,90,99,200];[1,(INDGEN(19)+1)*5, 99, 200] ;22 steps, as above plus 200 that is "take doy from measurements"
;*****************************************************************************
;
;filename used to retrieve the map info 
fn_example_for_coord_ret = 'E:\WA\all sahel\data\DIR_RECOMPOSED_UppEnv\REALIGN_ON_sos\A1sos-1997_acc1'
;Info for adjust for the time in dekads instead of JD (the new version of pheno correctly store JD so
;this part should be removed)
JDacq_fn='E:\WA\all sahel\data\UpTo1301g_bil'
nbAcq = 532
globrad_fname ='E:\WA\all sahel\data\globrad\UpTo1301gr_bil'
nbGlobrad = 532   ;be consistent with other files
;Model parameters file 
pheno_path = 'E:\WA\all sahel\data\DIR_RECOMPOSED_UppEnv\REALIGN_ON_sos'
output_path = 'E:\WA\all sahel\data\DIR_RECOMPOSED_UppEnv\REALIGN_ON_sos\all_acc_results'
base_par_fn = ['p0','p1','p2','p3','p4','p5','p6','t0','t1']
base_par_fn = 'A1sos-1997_'+base_par_fn
;Growing season to be computed
gs=1
base_par_fn = base_par_fn + STRTRIM(gs,2)
;basic file info
ns = 7841                      ; number of samples
nl = 1458                      ; number of lines
nb = 16                        ; number of years  from 1997-1998
frstyear = 1997
;here load the databse of measurements
ground_data_fn = 'S:\Actions\FOODSEC\projects\Biomass Sahel\Biomass_Afr\NIGER_IDL_FORMAT_2014-03-05.csv'


;Read the table of ground data
tmp =  READ_CSV(ground_data_fn, $
  HEADER=hdr, MISSING_VALUE=-9999)
PRINT, hdr
;load data into a strcture with the following fields:
;ID, SiteID, Location, Longitude, Latitude, Biomass, Year, Month, Day
data = rename_tags(tmp, ['FIELD1','FIELD2','FIELD3','FIELD4','FIELD5','FIELD6','FIELD7','FIELD8','FIELD9'], $ ; if you have more than 9 fields, name as follows: FIELD01, FIELD02, ...
  ['ID','SiteID','Location','Longitude','Latitude','Biomass','Year','Month','Day'])
; replace -9999 as missing value by NaN for biomass column
FOR i=3,5 DO BEGIN
  ind = WHERE(data.(i) EQ -9999, count)
  IF (count GT 0) THEN data.(i)[ind] = !VALUES.F_NAN
ENDFOR
;add a field containing sample and lines
data = CREATE_STRUCT(data, 'CoordSL', FLTARR(N_ELEMENTS(data.ID),3,3,2))
data.CoordSL[*,*,*] = !VALUES.F_NAN
;Make sure that ground data are sorted by ID
indSorted = FSORT(data.ID)
FOR i = 0, N_TAGS(tmp)-1 DO data.(i) = data.(i)[indSorted]
FOR i = 0, N_ELEMENTS(data.ID)-1 DO BEGIN
  ;associate smaple and line to each record
  data.coordSL[i,*,*,*] = SampleLineFromLonLat(data.Longitude(i), data.Latitude(i), fn_example_for_coord_ret, 3)
ENDFOR



FOR s = 0, N_ELEMENTS(s_option)-1 DO BEGIN
  FOR e = 0, N_ELEMENTS(e_option)-1 DO BEGIN
    res = compute_single_image(pheno_path, output_path, base_par_fn, ns, nl, nb, frstyear, JDacq_fn, nbAcq, globrad_fname, nbGlobrad, data, s_option[s], e_option[e])
  ENDFOR ;FOR e_option
ENDFOR ;FOR s_option





END