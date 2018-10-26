FUNCTION rs_yield_regress

;  Purpose:
;     To perform the linear regression analysis between yield data and a set of remote
;     sensing indicators. Yield data are provided at department level. Rs indicators
;     can be optionally filtered by a crop mask and by a threshold set on the maximum fAPAR (or NDVI).
;     Then they are averaged (weighted average based on the crop fraction) at the department level.
;     The linear regression model is tuned both at national (all data pooled togheter) and
;     at department level (one gression for each department).
;
;  Outcome:
;     Three report files:
;     - 2 showing the statistical analysis for yield and production
;     - 1 formatted to be used afterwards in R
;     If required, all the plots of regression models
;
;  Restriction:
;     all the images are coregistered and at the same VGT resolution

;  Usage:
;     rc = rs_yield_regress()

;  Input parameters: 
;     None. Relevant parameters are set in common blocks defined by the
;     job handler.

;  Output parameters: None.

;  Return values:
;     0: Normal completion.
;
;  Author:
;     Michele Meroni, michele.meroni@jrc.ec.europa.eu
;     
;  History:
;     Version 01:  Beta version, May 2011
;     Version 0.1: Beta version with comments added for CNCT, November 2011

;  References:
;     none


;COMMON exclude, excludeDepts, excludeYears
;COMMON param, MinPixPerDept, MinCropFract, UseMaxFaparThreshold, $
;              MaxFaparThreshold, UseCropMask, ComputeSurface
;COMMON settings, pheno_products, dept_ids, frst_RS_yr, doplot, verbose
;Examples
;regress_handler, 'K:\Tunisia\IO_info_marsFAPAR.txt', 'K:\Tunisia\OPTIONS_info.txt'


@cb_io.comm
@cb_options.comm 

dlmtr=','   ;for csv output
;output files
CALDAT,SYSTIME(/JULIAN),mm, dd, yy  ;yy introduced on 17 Jun 2014
strdata=strtrim(mm,2)+'-'+strtrim(dd,2)+'-'+strtrim(yy,2)
report_yield_fn = 'rep_' + varY + '_' + crop_base_name + '_' + strdata +'.csv'
report_production_fn = 'rep_production' + '_' + crop_base_name + '_' +strdata + '.csv'
report_processing_fn = 'rep_processing' + '_' + crop_base_name + '_' +strdata +'.txt'
report_table_fn = 'report_table'           + '_' + crop_base_name + '_' +strdata +'.csv'
;FILE_MKDIR, out_path

IF (UseMaxFaparThreshold EQ 0) THEN BEGIN
  PRINT, 'Warning: Max fAPAR threshold not used, MaxFaparThreshold set to 0'
  MaxFaparThreshold=0.0
ENDIF
IF (UseMaxFaparThreshold EQ 1) AND (MaxFaparThreshold EQ 0.0) THEN BEGIN
  PRINT, 'Error: requested use of avg_max_fapar threshold and threshold set to 0'
  STOP
ENDIF

;Remote sensing years
rs_yrs=yield_Year_4_frst_RS_band+indgen(nb)


; *****************************************************************************************
; 1 Retrieve data
; Open the fractional crop map
fcm=BYTARR(ns,nl)                  ;for storing fractional crop mask (originally 0-200 = 0-100%)
IF (UseCropMask EQ 1) THEN BEGIN
  OPENR, R1, cm_fn, /GET_LUN        
  READU, R1, fcm & CLOSE, R1
  fcm=FLOAT(fcm)/200.0                             ; now 0-1
ENDIF ELSE BEGIN                                   ; do not use crop mask
  PRINT, 'Warning: crop mask will not be used as requested, MinCropFract set to 0'
  MinCropFract=0
  fcm=fcm*0+1.0
ENDELSE

dept=BYTARR(ns,nl)                               ;for storing the department map     
; Open the  department map
OPENR, R1, dept_fn, /GET_LUN  ;read the file
READU, R1, dept & CLOSE, R1

;read the yield data 
;yield_data(3, n), columns: department, year, yield (negative -999 values are transformed into NaN
yield_data=read_yield(yield_fn,',')
IF (ComputeSurface EQ 0) THEN surface_data=read_yield(surface_fn,',') 
n_record=N_ELEMENTS(yield_data[0,*])  ;may be all or not all present in RS data

;read the names of the departments connected to id
tmp_struct = READ_CSV(dept_names_fn)
ids = REFORM(tmp_struct.field2)
dep_names = REFORM(tmp_struct.field1)
dep_names = dep_names[SORT(ids)]

;define db
db = db_structure_define(n_record, pheno_products)

;FILL THE DB
;Labels
db.pheno_wavg.lbl=pheno_products+'_wavg'
db.ncm_AFT_ACMT_FCY.lbl=pheno_products+'_ncm_AFT_ACMT_FCY'
db.totalw.lbl=pheno_products+'_totalw'
;Data
FOR i = 0, n_record-1 DO BEGIN
    db.depts.data[i]=yield_data[0,i] & db.years.data[i]=yield_data[1,i] & db.yield.data[i]=yield_data[2,i]
    IF (ComputeSurface EQ 0) THEN BEGIN
      ;check consistency with yield
      IF (yield_data[0,i] NE surface_data[0,i]) THEN STOP
      IF (yield_data[1,i] NE surface_data[1,i]) THEN STOP
      ;fill
      db.surf.data[i] = surface_data[2,i]
    ENDIF
ENDFOR

;Plot statistics
inds = SORT(db.years.data) 
tmp = db.years.data[inds]
year_list = tmp[UNIQ(tmp)]
inds = SORT(db.depts.data)
tmp =  db.depts.data[inds]
dep_list = tmp[UNIQ(tmp)]
;by dep stat
IF (doplot EQ 1) THEN BEGIN
  WINDOW, /FREE, TITLE='Yield by dep'
  xr=[MIN(rs_yrs), MAX(year_list)+3]
  yr=[MIN(db.yield.data, /NAN), MAX(db.yield.data, /NAN)]
  PLOT, [0,1], [0,1], XRANGE=xr, $
        YRANGE=yr, BACKGROUND = 16777215, COLOR = 0, /NODATA, XSTYLE=1
  DEVICE, DECOMPOSED=0 
  LOADCT, 38, /SILENT
  xp1=max(xr)-(max(xr)-min(xr))/10.0
  FOR d = 0, N_ELEMENTS(dep_list)-1 DO BEGIN
    yieldd = year_list * !VALUES.F_NAN
    FOR y = 0, N_ELEMENTS(year_list)-1 DO BEGIN  
      inddy = WHERE((db.depts.data EQ dep_list[d]) AND (db.years.data EQ year_list[y]), countdy)
      yieldd[y] = db.yield.data[inddy] ;MEAN(db.yield.data[inddy], /DOUBLE,/NAN)
    ENDFOR
    clr=FIX((d+1)/double(N_ELEMENTS(dep_list))*253)
    OPLOT, year_list, yieldd, COLOR=clr
    XYOUTS, xp1, yr[0]+((yr[1]-yr[0])/double(N_ELEMENTS(dep_list)+1))*(d+1), 'Dep ' + STRTRIM(FIX(dep_list[d]),2), COLOR=clr, CHARSIZE=1.0 
  ENDFOR 
  DEVICE, DECOMPOSED=1
  SAVEIMAGE, out_path+'\Yield_by_dep.tiff', /tiff, /QUIET
ENDIF
;exclude selected departments form the list of depts to be anlayzed (if required)
IF excludeDepts[0] NE 0 THEN BEGIN
  FOR i=0, N_ELEMENTS(excludeDepts)-1 DO BEGIN
    ind=WHERE(db.depts.data EQ excludeDepts[i], count)
    IF count GT 0 THEN BEGIN 
      db.depts.excluded[ind]=1
    ENDIF ELSE BEGIN
      IF (verbose EQ 1) THEN PRINT, 'WARNING: Department '+strtrim(excludeDepts[i],2)+' cannot be excluded because not present in the yield data'
    ENDELSE
  ENDFOR 
ENDIF 
;exclude selected years if required
PRINT, 'EXCLUDED YEARS because user request:'
IF excludeYears[0] NE 0 THEN BEGIN
  FOR i=0, N_ELEMENTS(excludeYears)-1 DO BEGIN
    ind=WHERE(db.years.data EQ excludeYears[i], count)
    IF count GT 0 THEN BEGIN 
      db.years.excluded[ind]=1
    ENDIF ELSE BEGIN
      IF (verbose EQ 1) THEN PRINT, 'WARNING: Year '+strtrim(excludeYears[i],2)+' cannot be excluded because not present in the yield data'
    ENDELSE
    tmp = db.years.data[ind]
    PRINT, tmp[UNIQ(tmp)]  
  ENDFOR
ENDIF 

;Flag the years where RS data are not present and flag them as excluded
;compute the array of RS years
;flag as excluded all the yield years below and above such years
ind = WHERE((db.years.data LT MIN(rs_yrs) OR (db.years.data GT MAX(rs_yrs))), count)
IF (count NE 0) THEN BEGIN
  db.years.excluded[ind] = 1
  PRINT, 'EXCLUDED YEARS because not present in the RS time series:'
  tmp = db.years.data[ind]
  PRINT, tmp[UNIQ(tmp)] 
ENDIF
 
;OPEN AND WRITE PROCESSING REPORT FILE
OPENW, urepproc, out_path+'\'+report_processing_fn, /GET_LUN
PRINTF, urepproc, SYSTIME()
PRINTF, urepproc, 'MinPixPerDept='+strtrim(MinPixPerDept,2)
PRINTF, urepproc, 'UseMaxFaparThreshold='+strtrim(UseMaxFaparThreshold,2)
PRINTF, urepproc, 'MaxFaparThreshold='+strtrim(MaxFaparThreshold,2)
PRINTF, urepproc, 'UseCropMask='+strtrim(UseCropMask,2)
PRINTF, urepproc, 'MinCropFract='+strtrim(MinCropFract,2)
FREE_LUN, urepproc  
; *****************************************************************************************
; 2 extract agri-weighted pheno indicators per department

;build avg_max_fapar based on maxFapar if required
IF UseMaxFaparThreshold EQ 1 THEN avg_max_fapar = get_avg_max_fapar(maxFapar_fn) 

;IF UseMaxFaparThreshold EQ 1 THEN BEGIN
;  ;open the pheno product (it is a bil file) and load it all in the pheno matrix
;  IF FILE_TEST(maxFapar_fn+'_avg') EQ 0 THEN BEGIN
;    avg_max_fapar=avg3Dimage(maxFapar_fn, ns, nl, nb)
;    IF (N_ELEMENTS(avg_max_fapar) EQ 1) THEN STOP
;    OPENW, W1, maxFapar_fn+'_avg', /GET_LUN 
;    WRITEU, W1, avg_max_fapar & FREE_LUN, W1 ;hdr is not written
;  ENDIF ELSE BEGIN
;    PRINT, 'Mask already present, use the existing one'
;    avg_max_fapar=fltarr(ns,nl)
;    OPENR, R1, maxFapar_fn+'_avg', /GET_LUN & READU, R1, avg_max_fapar & FREE_LUN, R1
;  ENDELSE
;  ;remove NaN and put -999 to avoid error using it as logical avg_max_fapar
;  indNaN=WHERE(FINITE(avg_max_fapar) NE 1, countNaN)
;  IF (countNaN NE 0) then avg_max_fapar(indNaN)=-999
;ENDIF

;Open reports file
;yiled report
OPENW, urepy, out_path+'\'+report_yield_fn, /GET_LUN           
PRINTF, urepy, 'RS_product' + dlmtr + $
                   strtrim(db.depts.lbl,2) + dlmtr + $
                   'R2' + dlmtr + $
                   'R2cv_overall' + dlmtr + $
                   'R2cv_within' + dlmtr + $
                   'RMSEcv' + dlmtr + $
                   'mod_eff_cv' + dlmtr + $
                   'RMSE' + dlmtr + $
                   'RRMSE' + dlmtr + $
                   'BIAS' + dlmtr + $
                   'b0' + dlmtr + $
                   'b1' + dlmtr + $
                   strtrim(db.n.lbl,2) + dlmtr + $
                   strtrim(db.ncm.lbl,2) + dlmtr + $
                   strtrim(db.ncm_AFT.lbl,2) + dlmtr + $
                   strtrim(db.ncm_AFT_ACMT.lbl,2) + dlmtr + $
                   'ncm_AFT_ACMT_FCY' + dlmtr + $
                   'totalw'
;production report
OPENW, urepp, out_path+'\'+report_production_fn, /GET_LUN
PRINTF, urepp, 'Model,RS_product,R2 Obs Vs. Mod,b0,b1,n_years'
;table for Eduardo
OPENW, ureptable, out_path+'\'+report_table_fn, /GET_LUN
tmp0=''

;Find the list of unexcluded records 
ind=WHERE((db.years.excluded EQ 0) AND (db.depts.excluded EQ 0), count)
dlist = uniqlist(db.depts.data[ind])
ndepts = N_ELEMENTS(dlist)
;find the list of years (that of depts was already performed)
ylist = uniqlist(db.years.data[ind])
nyears = N_ELEMENTS(ylist)

;variable for storing the r2cv for department and rs indicator
cvr2_dep_pp=DBLARR(ndepts,N_ELEMENTS(pheno_products))

;START THE ANALYS BY LOOPING ON EACH PHENO PRODUCR
FOR pp=0, N_ELEMENTS(pheno_products)-1 DO BEGIN
  PRINT, 'START ANALYSIS OF INDICATOR ' + pheno_products[pp]
  ;open the pheno product (it is a bil file) and load it all in the pheno matrix
  pheno=FLTARR(ns,nl,nb)
  OPENR, R1, pheno_path+'\'+pheno_products[pp], /GET_LUN
  ;if pp eq 2 then stop
  line_ass_data = ASSOC(R1, FLTARR(ns,nb))
  FOR line=0, nl-1, 1L DO pheno[*, line, *]=float(line_ass_data[line])
  FREE_LUN, R1
  ;treat -9999 data, set them to NaN or 0
  ind999 = WHERE(pheno EQ -999, count999)
  IF (count999 GT 0) THEN BEGIN
    
    pheno[ind999]=pheno_val999[pp] ;0.0 ; !VALUES.F_NAN
    PRINT, '-999s found and replaced with '+STRTRIM(pheno_val999[pp],2)+' in file: '+  pheno_products[pp]
  ENDIF
  ;for every record of db where dept and years are not excluded 
  ;compute weighted department avg and geo data (number of pixels etc)
  FOR r=0, n_record-1 DO BEGIN
    ;process the non excluded data
    IF ((db.years.excluded[r] EQ 0) AND (db.depts.excluded[r] EQ 0)) THEN BEGIN
      ;extract the pheno layer for the Current Year (phenoCY)
      bandCY=WHERE(rs_yrs EQ db.years.data[r], count) & IF count NE 1 THEN STOP
      phenoCY=REFORM(pheno[*,*,bandCY])
      ;PRINT, bandCY
      ;print, bandCY
      out = extract_RS_avg_val(phenoCY, dept, db.depts.data[r], $
                             UseMaxFaparThreshold, avg_max_fapar, MaxFaparThreshold, $
                             UseCropMask, fcm, MinCropFract, $
                             MinPixPerDept)
      IF (size(out, /DIMENSIONS) EQ 0) THEN IF (out EQ 10) THEN STOP  ;no finite values available
      db.n.data[r] = out.n
      db.ncm.data[r] = out.nCm
      db.ncm_AFT.data[r] = out.nAmt
      db.ncm_AFT_ACMT.data[r] = out.nAat
      db.ncm_AFT_ACMT_FCY.data[pp,r] = out.nFrs
      db.totalw.data[pp,r] = out.totalw
      db.pheno_wavg.data[pp,r] = out.wRSindex
    ENDIF ;processing of non excluded db record
  ENDFOR  ;r

  ;B) Global Model (Overall analysis, all depts pooled together)
  ;B1) collect unexcluded data 
  ind=WHERE((db.years.excluded EQ 0) AND (db.depts.excluded EQ 0), count)
  used_years = uniqlist(db.years.data[ind])   
  
  x=REFORM(db.pheno_wavg.data[pp,ind])  ;pheno/RS data
  y=REFORM(db.yield.data[ind])          ;yield data
  ids=REFORM(db.depts.data[ind])        ;department ids
  yrs=REFORM(db.years.data[ind])        ;year
  ;Check for missing data
  ind_finite = ind_of_XYfinite(x, y, 0, pheno_products[pp], verbose)

  x=x[ind_finite] & y=y[ind_finite] & n=N_ELEMENTS(y)
  ids=ids[ind_finite] & yrs=yrs[ind_finite]
  
  
  ;Plot by department and store global coeff of the linear regression
  wt='DEPT Yield Vs. ' + pheno_products[pp]+ ' (Pooled-OLS)'
  pt='Pooled-OLS'
  IF (UseCropMask) THEN pt = pt +', MinCropFract='+string(MinCropFract, FORMAT='(f4.2)')
  IF (UseMaxFaparThreshold EQ 1) THEN pt = pt +', MaxFAPARthreshold='+string(MaxFaparThreshold, FORMAT='(f4.2)')
  c=XYplotByZ(x, y, ids, wt, pt, pheno_products[pp], 'Yield', doplot, dep_names)
  gb0=c[0]  & gb1=c[1] & gcorr=c[2]
  IF (doplot EQ 1) THEN BEGIN
    SAVEIMAGE, out_path+'\'+pheno_products[pp]+'_'+yield_base_name+'_Pooled-OLS_by_DEPT.tiff', /tiff, /QUIET
  ENDIF
  ;B2) Plot by year and store global coeff of the linear regression
  wt='YEARS Yield Vs. ' + pheno_products[pp] + ' (Pooled-OLS)'
  pt='Pooled-OLS'
  IF (UseCropMask) THEN pt = pt +', MinCropFract='+STRING(MinCropFract, FORMAT='(f4.2)')
  IF (UseMaxFaparThreshold EQ 1) THEN pt= pt +', MaxFAPARthreshold='+STRTRIM(MaxFaparThreshold,2)
  null_legend = STRARR(N_ELEMENTS(dep_names)) 
  c = XYplotByZ(x, y, yrs, wt, pt, pheno_products[pp], 'Yield', doplot, null_legend)
  ;compute cross validated stats
  ;tmp = cv_stats_lo_year_o(x, y, yrs)
  tmp = cv_stats_lo_year_o_regCoeff_for_global_model(db, dlist, ylist, pp, pheno_products, verbose)
  cvr2_overall = tmp[0] & cvr2_within = tmp[1]  & rmsecv = tmp[2] & mod_eff_cv = tmp[3]
  ;cvr2 = tmp[0] & rmsecv = tmp[1] & mod_eff_cv = tmp[2]
  ;cvr2 = r2cvLoyo(x, y, yrs)
  ;compute other statistics 
  yobs = y
  ymod = gb0 + gb1 * x
  ;RMSE
  rmse = computeRMSE(yobs, ymod)
  ;RRMSE
  rrmse = computeRRMSE(yobs, ymod)
  ;BIAS
  bias = computeBIAS(yobs, ymod)
  IF (tmp[0] LE -10) THEN STOP
  IF (doplot EQ 1) THEN BEGIN
    SAVEIMAGE, out_path+'\'+pheno_products[pp]+'_'+yield_base_name+'_Pooled-OLS_by_YEAR.tiff', /tiff, /QUIET
  ENDIF 
    
  PRINTF, urepy, pheno_products[pp]+dlmtr+'RSI-glob_Coeff-glob'+dlmtr+STRTRIM(gcorr^2, 2)+ $
                 dlmtr+STRTRIM(cvr2_overall,2)+dlmtr+STRTRIM(cvr2_within,2)+dlmtr+STRTRIM(rmsecv,2)+dlmtr+STRTRIM(mod_eff_cv,2)+$
                 dlmtr+STRTRIM(rmse,2)+dlmtr+STRTRIM(rrmse,2)+dlmtr+STRTRIM(bias,2)+ $
                 dlmtr+STRTRIM(gb0,2)+$
                 dlmtr+STRTRIM(gb1,2)+dlmtr+STRTRIM(N_ELEMENTS(x),2)
  
  ;BB) Fixed effect model  
;  tmp=fix_eff_model(x, y, ids, yrs)
  ;C) Regional models (Department level analysis)
;  ;Find the list of unexcluded records 
;  ind=WHERE((db.years.excluded EQ 0) AND (db.depts.excluded EQ 0), count)
;  dlist = uniqlist(db.depts.data[ind])
;  ndepts = N_ELEMENTS(dlist)
;  ;find the list of years (that of depts was already performed)
;  ylist = uniqlist(db.years.data[ind])
;  nyears = N_ELEMENTS(ylist)
  ; make arrays for storing the modeled and observed yield for all depth
  ; array to store the results of regional models
  y_m = FLTARR(ndepts, nyears)* !VALUES.F_NAN
  y_o = y_m & x_o = y_m
   
  offset = FLTARR(N_ELEMENTS(dlist)) * !VALUES.F_NAN
  gain = offset
  FOR d=0,  N_ELEMENTS(dlist)-1 DO BEGIN
    ;C1) collect unexcluded data 
    ind=WHERE((db.years.excluded EQ 0) $
               AND (db.depts.excluded EQ 0 $
               AND db.depts.data EQ dlist[d]), count)
    x=REFORM(db.pheno_wavg.data[pp,ind])  ;pheno/RS data
    y=REFORM(db.yield.data[ind])          ;yield data
    years = db.years.data[ind]
    ;Check for missing data
    ind_finite = ind_of_XYfinite(x, y, db.depts.data[d], pheno_products[pp], verbose)
    x=x[ind_finite] & y=y[ind_finite] & n=N_ELEMENTS(y)
    years = years[ind_finite]
    IF (N_ELEMENTS(ind_finite) GT N_ELEMENTS(used_years)) THEN STOP
    ;C2) regress
    coeff = REGRESS(x, y, CORRELATION=corr, CONST=b0, /DOUBLE)
    tmp = cv_stats_lo_year_o(x, y, years)
    cvr2 = tmp[0] & rmsecv = tmp[1] & mod_eff_cv = tmp[2]
    ;cvr2 = r2cv(x, y)
    cvr2_dep_pp[d,pp] = cvr2
    ;IF (cvr2 LE -10) THEN STOP 
    gain[d] = coeff[0]
    offset[d] = b0
    ;compute other statistics 
    yobs = y
    ymod = b0 + coeff[0] * x
    ;RMSE
    rmse = computeRMSE(yobs, ymod)
    ;RRMSE
    rrmse = computeRRMSE(yobs, ymod)
    ;BIAS
    bias = computeBIAS(yobs, ymod)
    PRINTF, urepy, pheno_products[pp]+dlmtr+STRTRIM(dlist[d],2)+dlmtr+$
                   STRTRIM(corr^2, 2)+dlmtr+STRTRIM(cvr2,2)+dlmtr+STRTRIM('-',2)+dlmtr+$
                   STRTRIM(rmsecv,2)+dlmtr+STRTRIM(mod_eff_cv,2)+dlmtr+$
                   STRTRIM(rmse, 2)+dlmtr+STRTRIM(rrmse,2)+dlmtr+STRTRIM(bias,2)+dlmtr+$
                   STRTRIM(offset[d],2)+dlmtr+STRTRIM(gain[d],2)+ dlmtr +$
                   STRTRIM(MEAN(db.n.data[ind], /DOUBLE),2) + dlmtr + $
                   STRTRIM(MEAN(db.ncm.data[ind], /DOUBLE),2) + dlmtr + $
                   STRTRIM(MEAN(db.ncm_AFT.data[ind], /DOUBLE),2) + dlmtr + $
                   STRTRIM(MEAN(db.ncm_AFT_ACMT.data[ind], /DOUBLE),2) + dlmtr + $
                   ;strtrim(MEAN(db.pheno_wavg.data[pp,ind]),2) + dlmtr + $
                   STRTRIM(MEAN(db.ncm_AFT_ACMT_FCY.data[pp,ind], /DOUBLE),2) + dlmtr + $
                   STRTRIM(MEAN(db.totalw.data[pp,ind], /DOUBLE),2)
     x_o[d,ind_finite] = x
     y_o[d,ind_finite] = y
     y_m[d, ind_finite] = offset[d]+gain[d]*x              
  ENDFOR ;depart analyisis
  ;C3) Make actual vs predicted plot using regional models
  y_o_array = FLTARR(N_ELEMENTS(y_o))
  y_m_array = y_o_array & ids_array = y_o_array
  FOR dd=0, ndepts-1 DO BEGIN
    y_o_array[dd*(nyears):(dd+1)*(nyears)-1] = y_o[dd,*]   
    y_m_array[dd*(nyears):(dd+1)*(nyears)-1] = y_m[dd,*]
    ids_array[dd*(nyears):(dd+1)*(nyears)-1] = dlist[dd]
  ENDFOR
  wt='Yield, Predicted vs Measured ' + pheno_products[pp] + ' (Dept-OLS)'
  pt='Dept-OLS'
  IF (UseCropMask) THEN pt = pt +', MinCropFract='+string(MinCropFract, FORMAT='(f4.2)')
  IF (UseMaxFaparThreshold EQ 1) THEN pt= pt +', MaxFAPARthreshold='+strtrim(MaxFaparThreshold,2) 
  c=XYplotByZmodobs(y_m_array, y_o_array, ids_array, wt, pt, 'Predicted Yield', 'Measured Yield', doplot, dep_names)
  gb0R=c[0]  & gb1R=c[1] & gcorrR=c[2]
  IF (doplot EQ 1) THEN BEGIN
    SAVEIMAGE, out_path+'\'+pheno_products[pp]+'_'+yield_base_name+'_Dept-OLS_MODvsOBS.tiff', /tiff, /QUIET
  ENDIF 
  ;Compute cross validate R2
  
  ;oldcvr2 = r2cv_dep(x_o, y_o) ;è una vera cross val questa! infatti è lo stesso che:
  
  tmp = cv_stats_lo_year_o_regCoeff_at_dep(db, dlist, ylist, pp, pheno_products, verbose)
  cvr2_overall = tmp[0] & cvr2_within = tmp[1]  & rmsecv = tmp[2] & mod_eff_cv = tmp[3]
  ;cvr2 = tmp[0] & rmsecv = tmp[1] & mod_eff_cv = tmp[2]
  ;cvr2 = r2cv_dep_tuned(db, dlist, ylist, pp, pheno_products, verbose)
;  ;now compute the cvr2 of the moodel using the best indicator in each dept (in the cvR2 sense)
;  cvr2best = best_indicator_dep_tuned_r2cv(db, dlist, ylist, pp, pheno_products, verbose, cvr2_dep_pp)
;  PRINTF, urepy, 'bestRSeachDep'+dlmtr+'All_regional_mods'+dlmtr+'-'+dlmtr+$
;                 STRTRIM(cvr2best,2)+dlmtr+'-'+dlmtr+'-'+dlmtr+'-'+dlmtr+$
;                 '-'+dlmtr+'-'+dlmtr+'-'
  ;compute other statistics 
  yobs = y_o_array
  ymod = y_m_array
  ;RMSE
  rmse = computeRMSE(yobs, ymod)
  ;RRMSE
  rrmse = computeRRMSE(yobs, ymod)
  ;BIAS
  bias = computeBIAS(yobs, ymod)
  PRINTF, urepy, pheno_products[pp]+dlmtr+'RSI-glob_Coeff-dep'+dlmtr+strtrim(gcorrR^2, 2)+dlmtr+$
                 STRTRIM(cvr2_overall,2)+dlmtr+STRTRIM(cvr2_within,2)+dlmtr+STRTRIM(rmsecv,2)+dlmtr+STRTRIM(mod_eff_cv,2)+dlmtr+$
                 STRTRIM(rmse, 2)+dlmtr+STRTRIM(rrmse,2)+dlmtr+STRTRIM(bias,2)+dlmtr+$
                 strtrim(gb0R,2)+$
                 dlmtr+strtrim(gb1R,2)+dlmtr+strtrim(N_ELEMENTS(ymod),2)
  
  ;D) Production analyis
  ;D1) With the model tuned at national level
  ;D1.1) collect unexcluded data 
  ind=WHERE((db.years.excluded EQ 0) $
             AND (db.depts.excluded EQ 0), count)
  ;D1.2) compute observed and modeled production for each year each dep
  ;              Y     *   S              = P
  ;file:         Q/ha  *   1000ha          
  ;mask:         Q/ha  *   Km2
  units='Ktons'            
  IF (ComputeSurface EQ 1) THEN BEGIN
     ;in Ktons 
    surf = db.totalw.data[pp,ind]/100.0
    pt = 'Surface from mask'
  ENDIF ELSE BEGIN
     ;in Ktons 
    surf = db.surf.data[ind]/10.0
    pt = 'Surface from external data'
  ENDELSE
  obs_pro0=db.yield.data[ind]*surf
  mod_pro0=(gb0+gb1*db.pheno_wavg.data[pp,ind])*surf
  yrs0=db.years.data[ind]
  ;D1.3) Now sum all the deprtment for every year
  ;    be care, missing data (NaN) can be present in the yield data
  ;D1.3.1) Find the list of unexcluded years
  z=yrs0 
  ylist = uniqlist(z)
  obs_pro=fltarr(N_ELEMENTS(ylist))
  mod_pro=fltarr(N_ELEMENTS(ylist))
  FOR y=0, N_ELEMENTS(ylist)-1 DO BEGIN
    ind=WHERE(yrs0 EQ ylist[y], count)
    ; be care, missing data (NaN) can be present in the yield data
    ; Check for missing data
    obs_tmp=obs_pro0[ind]
    mod_tmp=mod_pro0[ind]
    ind_finite = ind_of_XYfinite(obs_tmp, mod_tmp, 0, 0, 0)
;    ind_obs_finite=WHERE(FINITE(obs_tmp) EQ 1, count_obs_finite)
;    ind_mod_finite=WHERE(FINITE(mod_tmp) EQ 1, count_mod_finite)
;    ind_finite=SetIntersection(ind_obs_finite, ind_mod_finite)
    obs_pro[y]=TOTAL(obs_tmp[ind_finite], /DOUBLE)
    mod_pro[y]=TOTAL(mod_tmp[ind_finite], /DOUBLE)
  ENDFOR
  
  ;D4) PLOT PRODUCTION
  ;D4.1) TREND with model tuned at National level
  x=REFORM(obs_pro)  & y=REFORM(mod_pro) 
  rmse=SQRT(TOTAL(((x-y)^2)/N_ELEMENTS(x), /DOUBLE))
  coeff = REGRESS(x, y, CORRELATION=corr, CONST=b0)
  IF (doplot EQ 1) THEN BEGIN
    WINDOW, /FREE, TITLE='PRODUCTION TREND, '+ pheno_products[pp] + ' (Pooled-OLS)'   
    range=max([x, y])-min([x, y])
    range=[min([x, y])-range/10.0,max([x, y])+range/10.0]
    PLOT, ylist, x, TITLE=pt+', R2 = '+STRTRIM(corr^2,2)+ ' RMSE = '+STRTRIM(rmse,2), $
          YRANGE=range, BACKGROUND = 16777215, COLOR = 0, $
          XTITLE='Years', YTITLE='Production ('+units+')' 
    OPLOT, ylist, x, PSYM=1, COLOR=0
    OPLOT, ylist, y, COLOR=3000
    OPLOT, ylist, y, COLOR=3000, PSYM=1
    XYOUTS, max(ylist)-(max(ylist)-min(ylist))/10.0, $
            min(range)+(max(range)-min(range))/10.0, 'mod', COLOR=3000
    XYOUTS, max(ylist)-(max(ylist)-min(ylist))/10.0, $
            min(range)+1.5*(max(range)-min(range))/10.0, 'obs', COLOR=0
    
    PRINTF, urepp, 'RSI-glob_Coeff-glob'+dlmtr+pheno_products[pp]+dlmtr+strtrim(corr^2, 2)+dlmtr+strtrim(b0,2)+ $
                   dlmtr+strtrim(coeff[0],2)+dlmtr+strtrim(N_ELEMENTS(x),2)
    SAVEIMAGE, out_path+'\'+pheno_products[pp]+'_'+yield_base_name+'_production_Global.tiff', /tiff, /QUIET
 
    ;SCATTERPLOT with model tuned at Natioanl level
    WINDOW, /FREE, TITLE='OBS Vs. MOD, '+ pheno_products[pp]+ ' (RSI-glob_Coeff-glob)'
     
    PLOT, obs_pro, mod_pro, TITLE='Production, R2 = '+STRTRIM(corr^2,2), XTITLE='obs', $
                   YTITLE='mod', psym=1, XRANGE=range, YRANGE=range, BACKGROUND = 16777215, COLOR = 0 
    SAVEIMAGE, out_path+'\'+pheno_products[pp]+'_'+yield_base_name+'_production_scatter_Global.tiff', /tiff, /QUIET
  ENDIF


  ;D2) With the model tuned at regional levels
  ;D2.2) compute observed and modeled production for each year each dep
  obs_pro_r = FLTARR(N_ELEMENTS(ylist),N_ELEMENTS(dlist))
  mod_pro_r = obs_pro_r
  mod_pro_r2 = obs_pro_r
  obs_pro = FLTARR(N_ELEMENTS(ylist))
  mod_pro = FLTARR(N_ELEMENTS(ylist))
  mod_pro2 = FLTARR(N_ELEMENTS(ylist))  ;using avg surface for prediction
  units='Ktons'
  IF (ComputeSurface EQ 1) THEN pt = 'Surface from mask'ELSE pt = 'Surface from external data'
  FOR y = 0, N_ELEMENTS(ylist)-1 DO BEGIN
    FOR d = 0, N_ELEMENTS(dlist)-1 DO BEGIN
      ind = WHERE((db.years.data EQ ylist[y]) $
                   AND (db.depts.data EQ dlist[d]), count)
      ;for mean surface for dep
      indall=WHERE((db.years.excluded NE 1) $
                   AND (db.depts.data EQ dlist[d]), count)
      IF (ComputeSurface EQ 1) THEN BEGIN
        surf = db.totalw.data[pp,ind]/100.0
      ENDIF ELSE BEGIN
        surf = db.surf.data[ind]/10.0
      ENDELSE
      mod_pro_r[y,d] = (offset[d]+gain[d]*db.pheno_wavg.data[pp,ind]) * surf
      mod_pro_r2[y,d] = (offset[d]+gain[d]*db.pheno_wavg.data[pp,ind]) * MEAN(db.surf.data[indall]/10.0, /NAN, /DOUBLE)
      obs_pro_r[y,d] = db.yield.data[ind] * surf
    ENDFOR ;d
    obs_tmp = obs_pro_r[y,*]
    mod_tmp = mod_pro_r[y,*]
    mod_tmp2 = mod_pro_r2[y,*]
    ind_finite = ind_of_XYfinite(obs_tmp, mod_tmp, 0, 0, 0)
;    ind_obs_finite = WHERE(FINITE(obs_tmp) EQ 1, count_obs_finite)
;    ind_mod_finite = WHERE(FINITE(mod_tmp) EQ 1, count_mod_finite)
;    ind_finite = SetIntersection(ind_obs_finite, ind_mod_finite)
    obs_pro[y] = TOTAL(obs_tmp[ind_finite], /DOUBLE)
    mod_pro[y] = TOTAL(mod_tmp[ind_finite], /DOUBLE)
    mod_pro2[y] = TOTAL(mod_tmp2[ind_finite], /DOUBLE)
  ENDFOR  ;y
  

  ;D5) PLOT PRODUCTION
  ;D5.1) TREND with model tuned at Regional level
  x=REFORM(obs_pro)  & y=REFORM(mod_pro) & y2=REFORM(mod_pro2)
  rmse=SQRT(TOTAL(((x-y)^2)/N_ELEMENTS(x), /DOUBLE))
  coeff = REGRESS(x, y, CORRELATION=corr, CONST=b0)
  IF (doplot EQ 1) THEN BEGIN
    WINDOW, /FREE, TITLE='PRODUCTION TREND, '+ pheno_products[pp] + ' (Department-OLS)'   
    range=max([x, y])-min([x, y])
    range=[min([x, y])-range/10.0,max([x, y])+range/10.0]
    PLOT, ylist, x, TITLE=pt+', R2 = '+STRTRIM(corr^2,2) + ' RMSE = '+STRTRIM(rmse,2), $
          YRANGE=range, BACKGROUND = 16777215, COLOR = 0, $
          XTITLE='Years', YTITLE='Production ('+units+')' 
    OPLOT, ylist, x, PSYM=1, COLOR=0
    OPLOT, ylist, y, COLOR=3000
    OPLOT, ylist, y, COLOR=3000, PSYM=1
    OPLOT, ylist, y2, COLOR=60000
    OPLOT, ylist, y2, COLOR=60000, PSYM=1
    XYOUTS, max(ylist)-(max(ylist)-min(ylist))/6.0, $
            min(range)+(max(range)-min(range))/10.0, 'mod_true_surface', COLOR=3000
    XYOUTS, max(ylist)-(max(ylist)-min(ylist))/6.0, $
            min(range)+0.5*(max(range)-min(range))/10.0, 'mod_avg_surface', COLOR=60000
    XYOUTS, max(ylist)-(max(ylist)-min(ylist))/6.0, $
            min(range)+1.5*(max(range)-min(range))/10.0, 'obs', COLOR=0
    
    PRINTF, urepp, 'RSI-glob_Coeff-dep'+dlmtr+pheno_products[pp]+dlmtr+strtrim(corr^2, 2)+dlmtr+strtrim(b0,2)+ $
                   dlmtr+strtrim(coeff[0],2)+dlmtr+strtrim(N_ELEMENTS(x),2)
    SAVEIMAGE, out_path+'\'+pheno_products[pp]+'_'+yield_base_name+'_production_Regional.tiff', /tiff, /QUIET
 
    ;SCATTERPLOT with model tuned at Natioanl level
    WINDOW, /FREE, TITLE='OBS Vs. MOD, '+ pheno_products[pp]+ ' (RSI-glob_Coeff-dep)'
     
    PLOT, obs_pro, mod_pro, TITLE='Production, R2 = '+STRTRIM(corr^2,2), XTITLE='obs', $
                   YTITLE='mod', psym=1, XRANGE=range, YRANGE=range, BACKGROUND = 16777215, COLOR = 0 
    SAVEIMAGE, out_path+'\'+pheno_products[pp]+'_'+yield_base_name+'_production_scatter_Regional.tiff', /tiff, /QUIET
  ENDIF
  PRINT, '#################Indicator '+pheno_products[pp]+' completed'
  ;free the windows
  IF (deleteWindows EQ 1) THEN BEGIN
  wid=!d.window
  WHILE (wid NE -1) DO BEGIN
    wdelete, wid
    wid = !d.window
  ENDWHILE
  ENDIF
ENDFOR ;pp

;E) analysis of best RS indicator at each dep
;now compute the cvr2 of the moodel using the best indicator in each dept (in the cvR2 sense)
tmp = cv_stats_lo_year_o_RS_at_dep(db, dlist, ylist, 0, pheno_products, verbose, cvr2_dep_pp, out_path)
cvr2_overall = tmp[0] & cvr2_within = tmp[1]  & rmsecv = tmp[2] & mod_eff_cv = tmp[3]
;cvr2best = r2cv_best_indicator_dep_tuned(db, dlist, ylist, 0, pheno_products, verbose, cvr2_dep_pp, out_path)
PRINTF, urepy, 'RSI-dep_Coeff-dep'+dlmtr+'All_different_regional_mods'+dlmtr+'-'+dlmtr+$
                 STRTRIM(cvr2_overall,2)+dlmtr+STRTRIM(cvr2_within,2)+dlmtr+STRTRIM(rmsecv,2)+dlmtr+STRTRIM(mod_eff_cv,2)+dlmtr+$
                 '-'+dlmtr+'-'+dlmtr+'-'+dlmtr+$
               '-'+dlmtr+'-'+dlmtr+'-'


;F) print the big_table
; print hdr line
;tmp=''
;FOR pp=0, N_ELEMENTS(pheno_products)-1 DO BEGIN
;  tmp=tmp+strtrim(db.pheno_wavg.lbl[pp],2) + dlmtr + $
;          strtrim(db.ncm_AFT_ACMT_FCY.lbl[pp],2) + dlmtr + $
;          strtrim(db.totalw.lbl[pp],2) + dlmtr
;ENDFOR
tmp = strtrim(db.ncm_AFT_ACMT_FCY.lbl[0],2) + dlmtr + $
      strtrim(db.totalw.lbl[0],2) + dlmtr
FOR pp=0, N_ELEMENTS(pheno_products)-1 DO BEGIN
  tmp=tmp+strtrim(db.pheno_wavg.lbl[pp],2) + dlmtr
ENDFOR
PRINTF, ureptable, strtrim(db.depts.lbl,2) + dlmtr + $
                   strtrim(db.years.lbl,2) + dlmtr + $
                   strtrim(db.yield.lbl,2) + dlmtr + $
                   strtrim(db.n.lbl,2) + dlmtr + $
                   strtrim(db.ncm.lbl,2) + dlmtr + $
                   strtrim(db.ncm_AFT.lbl,2) + dlmtr + $
                   strtrim(db.ncm_AFT_ACMT.lbl,2) + dlmtr + $
                   tmp
; print records 
FOR r=0, n_record-1 DO BEGIN
  ;process the non excluded data
  IF ((db.years.excluded[r] EQ 0) AND (db.depts.excluded[r] EQ 0)) THEN BEGIN
    tmp = strtrim(db.ncm_AFT_ACMT_FCY.data[0,r],2) + dlmtr + $
          strtrim(db.totalw.data[0,r],2) + dlmtr
    FOR pp=0, N_ELEMENTS(pheno_products)-1 DO BEGIN
      tmp=tmp+strtrim(db.pheno_wavg.data[pp,r],2) + dlmtr
    ENDFOR
;    tmp=''
;    FOR pp=0, N_ELEMENTS(pheno_products)-1 DO BEGIN
;      tmp=tmp+strtrim(db.pheno_wavg.data[pp,r],2) + dlmtr + $
;              strtrim(db.ncm_AFT_ACMT_FCY.data[pp,r],2) + dlmtr + $
;              strtrim(db.totalw.data[pp,r],2) + dlmtr
;    ENDFOR
    ;pheno_product, db.ncm_AFT_ACMT_FCY, db.totalw
    PRINTF, ureptable, strtrim(db.depts.data[r],2) + dlmtr + $
                       strtrim(db.years.data[r],2) + dlmtr + $
                       strtrim(db.yield.data[r],2) + dlmtr + $
                       strtrim(db.n.data[r],2) + dlmtr + $
                       strtrim(db.ncm.data[r],2) + dlmtr + $
                       strtrim(db.ncm_AFT.data[r],2) + dlmtr + $
                       strtrim(db.ncm_AFT_ACMT.data[r],2) + dlmtr + $
                       tmp
  ENDIF
ENDFOR


;close report file

FREE_LUN, urepy
FREE_LUN, urepp
CLOSE, /ALL
RETURN, 0  

END
