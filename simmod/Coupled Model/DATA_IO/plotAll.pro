FUNCTION plotAll, simGPP, EcGpp, ModisGPP, SiteCode, SiteId, SiteIgbp, overall_run_reportFn, out_dir

SAVE, /ALL, FILENAME =  out_dir + '\overall_results.sav'
;RESTORE,  'D:\SimMod_data\RUNS\non_reloc\newRUN1_Validation_sites_excl_avi_new_graphs6_clump1.00\overall_results.sav'
countfin_sim_cro=0 & r_sim_cro=0 & RMSE_sim_cro=0 & a_sim_cro=0 & b_sim_cro = 0
countfin_sim_gra=0 & r_sim_gra=0 & RMSE_sim_gra=0 & a_sim_gra=0 & b_sim_gra = 0
countfin_mod_cro=0 & r_mod_cro=0 & RMSE_mod_cro=0 & a_mod_cro=0 & b_mod_cro = 0
countfin_mod_gra=0 & r_mod_gra=0 & RMSE_mod_gra=0 & a_mod_gra=0 & b_mod_gra = 0
  
IF (N_ELEMENTS(simGPP) NE N_ELEMENTS(EcGpp) OR N_ELEMENTS(simGPP) NE N_ELEMENTS(ModisGPP)) THEN STOP

fs = 8 ;font size
lp = [1.2, 1] ;legend position
dm = [900,600]  ;dimensions of the window
xminor = 2        ;monor ticks between majors
nmajor = 4
ticlensec = 0.03
xmin = 0.05
ymin = 0.54 ;(0.605)
graph_dimx = 0.25
graph_dimy = 0.4
;scatter plots
;SIM on the first line (CRO, GRA and SAV, and all)
pos0 = [xmin, ymin, xmin + graph_dimx, ymin + graph_dimy]
pos = pos0 
yrange=[MIN([EcGpp, simGPP, ModisGPP], /NAN)-0.2, MAX([EcGpp, simGPP, ModisGPP], /NAN)+0.2]
;yrange=[-0.5, MAX([EcGpp, simGPP], /NAN)+0.2] 

;all
h_scatter_sim = PLOT(EcGpp, simGPP, SYMBOL='+', LINESTYLE='none', XRANGE=yrange, YRANGE=yrange, FONT_SIZE = fs, POSITION=pos, DIMENSIONS=dm, TITLE = 'All', XTITLE='EC GPP', YTITLE='Sim model GPP (0 offseason)')
gh_11 = PLOT(yrange, yrange, OVERPLOT=1, COLOR='green', NAME='1:1 line', LINESTYLE='--')
indfin = WHERE(FINITE(EcGpp), countfin_sim)
b_sim = REGRESS(EcGpp[indfin], simGPP[indfin], CONST = a_sim, CORRELATION = r_sim)
RMSE_sim = SQRT(TOTAL((EcGpp[indfin]-simGPP[indfin])^2)/FLOAT(countfin_sim))
MBE_sim = TOTAL(simGPP[indfin]-EcGpp[indfin]) / FLOAT(countfin_sim) 
;trick to avoid plotting outside
x0=(0.0-a_sim)/b_sim[0]
x1=(yrange[1]-a_sim)/b_sim[0]
IF (x0 LT 0) THEN x0=0.0
yreg = a_sim+b_sim[0]*[x0,x1]
gh_reg = PLOT([x0,x1], yreg, OVERPLOT=1, COLOR='blue', NAME='Reg, R2='+STRTRIM(r_sim^2,2), YRANGE=yrange)
th1 = TEXT(pos[2]-0.21, pos[1]+0.35, '$R^2$='+STRING(r_sim^2, FORMAT='(F4.2)', /PRINT)+', RMSE=' + STRING(RMSE_sim, FORMAT='(F4.2)', /PRINT), COLOR='blue')
th2 = TEXT(pos[2]-0.21, pos[1]+0.32, 'MBE='+STRING(MBE_sim, FORMAT='(F6.2)', /PRINT)+', n=' + STRTRIM(countfin_sim,2), COLOR='blue')

;density
ngrid = 30
nlevel = 50
dolog = 1
u = EcGpp
v = simGPP
range = [FLOOR(MIN([u, v],/NAN)),CEIL(MAX([u, v],/NAN))]
ngrid = range[1]-range[0]
DensityAndFit_log_scale, u, v, 'EC GPP (gC $m^{-2}$ $d^{-1}$)', 'Sim model GPP (gC $m^{-2}$ $d^{-1}$)', out_dir, range, range, ngrid, nlevel, TITLE='All', $
  PLOT1TO1 = 1, DOFIT=1, SIMPLE =1, DOLOG= dolog, FULL_FILENAME_IMPOSED = 'AAA_densScatter_ECvsSIM_all'


;CRO
pos[[0,2]]= pos[[0,2]] + 0.33
indCRO = WHERE(SiteIgbp EQ 'CRO', count_cro)
IF (count_cro GT 0) THEN BEGIN
  tmpEcGPP =   EcGpp[indCRO]
  tmpSimGPP = simGPP[indCRO]
  h_scatter_sim = PLOT(tmpEcGPP, tmpSimGPP, SYMBOL='+', LINESTYLE='none', XRANGE=yrange, YRANGE=yrange, FONT_SIZE = fs, POSITION=pos, /CURRENT, DIMENSIONS=dm, TITLE = 'CRO', XTITLE='EC GPP', YTITLE='Sim model GPP (0 offseason)')
  gh_11 = PLOT(yrange, yrange, OVERPLOT=1, COLOR='green', NAME='1:1 line', LINESTYLE='--')
  indfin = WHERE(FINITE(tmpEcGPP), countfin_sim_cro)
  b_sim_cro = REGRESS(tmpEcGPP[indfin], tmpSimGPP[indfin], CONST = a_sim_cro, CORRELATION = r_sim_cro)
  RMSE_sim_cro = SQRT(TOTAL((tmpEcGPP[indfin]-tmpSimGPP[indfin])^2)/FLOAT(countfin_sim_cro))
  MBE_sim_cro = TOTAL(tmpSimGPP[indfin]-tmpEcGPP[indfin]) / FLOAT(countfin_sim_cro) 
  ;trick to avoid plotting outside
  x0=(0.0-a_sim_cro)/b_sim_cro[0]
  x1=(yrange[1]-a_sim_cro)/b_sim_cro[0]
  IF (x0 LT 0) THEN x0=0.0
  yreg = a_sim_cro + b_sim_cro[0]*[x0,x1]
  gh_reg = PLOT([x0,x1], yreg, OVERPLOT=1, COLOR='blue', NAME='Reg, R2='+STRTRIM(r_sim_cro^2,2), YRANGE=yrange)
  th1 = TEXT(pos[2]-0.21, pos[1]+0.35, '$R^2$='+STRING(r_sim_cro^2, FORMAT='(F4.2)', /PRINT)+', RMSE=' + STRING(RMSE_sim_cro, FORMAT='(F4.2)', /PRINT), COLOR='blue')
  th2 = TEXT(pos[2]-0.21, pos[1]+0.32, 'MBE='+STRING(MBE_sim_cro, FORMAT='(F6.2)', /PRINT)+', n=' + STRTRIM(countfin_sim_cro,2), COLOR='blue')
  u = tmpEcGPP
  v = tmpSimGPP
  range = [MIN([u, v],/NAN),MAX([u, v],/NAN)]
  range = [FLOOR(MIN([u, v],/NAN)),CEIL(MAX([u, v],/NAN))]
  ngrid = range[1]-range[0]
  DensityAndFit_log_scale, u, v, 'EC GPP (gC $m^{-2}$ $d^{-1}$)', 'Sim model GPP (gC $m^{-2}$ $d^{-1}$)', out_dir, range, range, ngrid, nlevel, TITLE='Crop', $
    PLOT1TO1 = 1, DOFIT=1, SIMPLE =1, DOLOG= dolog, FULL_FILENAME_IMPOSED = 'AAA_densScatter_ECvsSIM_crop'
ENDIF


;GRA
pos[[0,2]]= pos[[0,2]] + 0.33
indGRA = WHERE((SiteIgbp EQ 'GRA') OR (SiteIgbp EQ 'SAV'), count_gra)
IF (count_gra GT 0) THEN BEGIN
  tmpEcGPP =   EcGpp[indGRA]
  tmpSimGPP = simGPP[indGRA]
  h_scatter_sim = PLOT(tmpEcGPP, tmpSimGPP, SYMBOL='+', LINESTYLE='none', XRANGE=yrange, YRANGE=yrange, FONT_SIZE = fs, POSITION=pos, /CURRENT, DIMENSIONS=dm, TITLE = 'GRA, SAV', XTITLE='EC GPP', YTITLE='Sim model GPP (0 offseason)')
  gh_11 = PLOT(yrange, yrange, OVERPLOT=1, COLOR='green', NAME='1:1 line', LINESTYLE='--')
  indfin = WHERE(FINITE(tmpEcGPP), countfin_sim_gra)
  b_sim_gra = REGRESS(tmpEcGPP[indfin], tmpSimGPP[indfin], CONST = a_sim_gra, CORRELATION = r_sim_gra)
  RMSE_sim_gra = SQRT(TOTAL((tmpEcGPP[indfin]-tmpSimGPP[indfin])^2)/FLOAT(countfin_sim_gra))
  MBE_sim_gra = TOTAL(tmpSimGPP[indfin]-tmpEcGPP[indfin]) / FLOAT(countfin_sim_gra) 
  ;trick to avoid plotting outside
  x0=(0.0-a_sim_gra)/b_sim_gra[0]
  x1=(yrange[1]-a_sim_gra)/b_sim_gra[0]
  IF (x0 LT 0) THEN x0=0.0
  yreg = a_sim_gra + b_sim_gra[0]*[x0,x1]
  gh_reg = PLOT([x0,x1], yreg, OVERPLOT=1, COLOR='blue', NAME='Reg, R2='+STRTRIM(r_sim_gra^2,2), YRANGE=yrange)
  th1 = TEXT(pos[2]-0.21, pos[1]+0.35, '$R^2$='+STRING(r_sim_gra^2, FORMAT='(F4.2)', /PRINT)+', RMSE=' + STRING(RMSE_sim_gra, FORMAT='(F4.2)', /PRINT), COLOR='blue')
  th2 = TEXT(pos[2]-0.21, pos[1]+0.32, 'MBE='+STRING(MBE_sim_gra, FORMAT='(F6.2)', /PRINT)+', n=' + STRTRIM(countfin_sim_gra,2), COLOR='blue')
  u = tmpEcGPP
  v = tmpSimGPP
  range = [MIN([u, v],/NAN),MAX([u, v],/NAN)]
  range = [FLOOR(MIN([u, v],/NAN)),CEIL(MAX([u, v],/NAN))]
  ngrid = range[1]-range[0]
  DensityAndFit_log_scale, u, v, 'EC GPP (gC $m^{-2}$ $d^{-1}$)', 'Sim model GPP (gC $m^{-2}$ $d^{-1}$)', out_dir, range, range, ngrid, nlevel, TITLE='Grass', $
    PLOT1TO1 = 1, DOFIT=1, SIMPLE =1, DOLOG= dolog, FULL_FILENAME_IMPOSED = 'AAA_densScatter_ECvsSIM_grass'
ENDIF

;Modis
pos = pos0
pos[[1,3]]= pos[[1,3]] - 0.49
;yrange=[-0.5, MAX([EcGpp, ModisGPP], /NAN)+0.2]
;ALL
h_scatter_mod = PLOT(EcGpp, ModisGPP, SYMBOL='+', LINESTYLE='none', XRANGE=yrange, YRANGE=yrange, color='green', FONT_SIZE = fs, POSITION=pos, /CURRENT, TITLE='All',XTITLE='EC GPP', YTITLE='Modis GPP')
gh_11 = PLOT(yrange, yrange, OVERPLOT=1, COLOR='green', NAME='1:1 line', LINESTYLE='--')
indfin = WHERE(FINITE(EcGpp) AND FINITE(ModisGPP), countfin_mod)
b_mod = REGRESS(EcGpp[indfin], ModisGPP[indfin], CONST = a_mod, CORRELATION = r_mod)
RMSE_mod = SQRT(TOTAL((EcGpp[indfin]-ModisGPP[indfin])^2)/FLOAT(countfin_mod))
MBE_mod = TOTAL(ModisGPP[indfin]-EcGpp[indfin]) / FLOAT(countfin_mod) 
;trick to avoid plotting outside
x0=(0.0-a_mod)/b_mod[0]
x1=(yrange[1]-a_mod)/b_mod[0]
IF (x0 LT 0) THEN x0=0.0
yreg = a_mod+b_mod[0]*[x0,x1]
gh_reg = PLOT([x0,x1], yreg, OVERPLOT=1, COLOR='blue', NAME='Reg, R2='+STRTRIM(r_mod^2,2), YRANGE=yrange)
th1 = TEXT(pos[2]-0.21, pos[1]+0.35, '$R^2$='+STRING(r_mod^2, FORMAT='(F4.2)', /PRINT)+', RMSE=' + STRING(RMSE_mod, FORMAT='(F4.2)', /PRINT), COLOR='blue')
th2 = TEXT(pos[2]-0.21, pos[1]+0.32, 'MBE='+STRING(MBE_mod, FORMAT='(F6.2)', /PRINT)+', n=' + STRTRIM(countfin_mod,2), COLOR='blue')
u = EcGpp
v = ModisGPP
range = [MIN([u, v],/NAN),MAX([u, v],/NAN)]
range = [FLOOR(MIN([u, v],/NAN)),CEIL(MAX([u, v],/NAN))]
ngrid = range[1]-range[0]
DensityAndFit_log_scale, u, v, 'EC GPP (gC $m^{-2}$ $d^{-1}$)', 'MOD17 GPP (gC $m^{-2}$ $d^{-1}$)', out_dir, range, range, ngrid, nlevel, TITLE='All', $
  PLOT1TO1 = 1, DOFIT=1, SIMPLE =1, DOLOG= dolog, FULL_FILENAME_IMPOSED = 'AAA_densScatter_ECvsMOD_all'

;CRO
pos[[0,2]]= pos[[0,2]] + 0.33
IF (count_cro GT 0) THEN BEGIN
  tmpEcGPP = EcGpp[indCRO]
  tmpSimGPP = ModisGPP[indCRO]
  h_scatter_mod = PLOT(tmpEcGPP, tmpSimGPP, SYMBOL='+', LINESTYLE='none', XRANGE=yrange, YRANGE=yrange, color='green',  FONT_SIZE = fs, POSITION=pos, /CURRENT, TITLE='CRO',XTITLE='EC GPP', YTITLE='Modis GPP')
  gh_11 = PLOT(yrange, yrange, OVERPLOT=1, COLOR='green', NAME='1:1 line', LINESTYLE='--')
  indfin = WHERE(FINITE(tmpEcGPP) AND FINITE(tmpSimGPP), countfin_mod_cro); indfin = WHERE(FINITE(tmpEcGPP), countfin_mod_cro)
  b_mod_cro = REGRESS(tmpEcGPP[indfin], tmpSimGPP[indfin], CONST = a_mod_cro, CORRELATION = r_mod_cro)
  RMSE_mod_cro = SQRT(TOTAL((tmpEcGPP[indfin]-tmpSimGPP[indfin])^2)/FLOAT(countfin_mod_cro))
  MBE_mod_cro = TOTAL(tmpSimGPP[indfin]-tmpEcGPP[indfin]) / FLOAT(countfin_mod_cro) 
  ;trick to avoid plotting outside
  x0=(0.0-a_mod_cro)/b_mod_cro[0]
  x1=(yrange[1]-a_mod_cro)/b_mod_cro[0]
  IF (x0 LT 0) THEN x0=0.0
  yreg = a_mod_cro+b_mod_cro[0]*[x0,x1]
  gh_reg = PLOT([x0,x1], yreg, OVERPLOT=1, COLOR='blue', NAME='Reg, R2='+STRTRIM(r_mod_cro^2,2), YRANGE=yrange)
  th1 = TEXT(pos[2]-0.21, pos[1]+0.35, '$R^2$='+STRING(r_mod_cro^2, FORMAT='(F4.2)', /PRINT)+', RMSE=' + STRING(RMSE_mod_cro, FORMAT='(F4.2)', /PRINT), COLOR='blue')
  th2 = TEXT(pos[2]-0.21, pos[1]+0.32, 'MBE='+STRING(MBE_mod_cro, FORMAT='(F6.2)', /PRINT)+', n=' + STRTRIM(countfin_mod_cro,2), COLOR='blue')
  u = tmpEcGPP
  v = tmpSimGPP
  range = [MIN([u, v],/NAN),MAX([u, v],/NAN)]
  range = [FLOOR(MIN([u, v],/NAN)),CEIL(MAX([u, v],/NAN))]
  ngrid = range[1]-range[0]
  DensityAndFit_log_scale, u, v, 'EC GPP (gC $m^{-2}$ $d^{-1}$)', 'MOD17 GPP (gC $m^{-2}$ $d^{-1}$)', out_dir, range, range, ngrid, nlevel, TITLE='Crop', $
    PLOT1TO1 = 1, DOFIT=1, SIMPLE =1, DOLOG= dolog, FULL_FILENAME_IMPOSED = 'AAA_densScatter_ECvsMOD_crop'
ENDIF
;GRA
pos[[0,2]]= pos[[0,2]] + 0.33
IF (count_gra GT 0) THEN BEGIN
  tmpEcGPP = EcGpp[indGRA]
  tmpSimGPP = ModisGPP[indGRA]
  h_scatter_mod = PLOT(tmpEcGPP, tmpSimGPP, SYMBOL='+', LINESTYLE='none', XRANGE=yrange, YRANGE=yrange, color='green',  FONT_SIZE = fs, POSITION=pos, /CURRENT, TITLE='GRA, SAV',XTITLE='EC GPP', YTITLE='Modis GPP')
  gh_11 = PLOT(yrange, yrange, OVERPLOT=1, COLOR='green', NAME='1:1 line', LINESTYLE='--')
  indfin = WHERE(FINITE(tmpEcGPP) AND FINITE(tmpSimGPP), countfin_mod_gra);indfin = WHERE(FINITE(tmpEcGPP), countfin_mod_gra)
  b_mod_gra = REGRESS(tmpEcGPP[indfin], tmpSimGPP[indfin], CONST = a_mod_gra, CORRELATION = r_mod_gra)
  RMSE_mod_gra = SQRT(TOTAL((tmpEcGPP[indfin]-tmpSimGPP[indfin])^2)/FLOAT(countfin_mod_gra))
  MBE_mod_gra = TOTAL(tmpSimGPP[indfin]-tmpEcGPP[indfin]) / FLOAT(countfin_mod_gra) 
  ;trick to avoid plotting outside
  x0=(0.0-a_mod_gra)/b_mod_gra[0]
  x1=(yrange[1]-a_mod_gra)/b_mod_gra[0]
  IF (x0 LT 0) THEN x0=0.0
  yreg = a_mod_gra+b_mod_gra[0]*[x0,x1]
  gh_reg = PLOT([x0,x1], yreg, OVERPLOT=1, COLOR='blue', NAME='Reg, R2='+STRTRIM(r_mod_gra^2,2), YRANGE=yrange)
  th1 = TEXT(pos[2]-0.21, pos[1]+0.35, '$R^2$='+STRING(r_mod_gra^2, FORMAT='(F4.2)', /PRINT)+', RMSE=' + STRING(RMSE_mod_gra, FORMAT='(F4.2)', /PRINT), COLOR='blue')
  th2 = TEXT(pos[2]-0.21, pos[1]+0.32,  'MBE='+STRING(MBE_mod_gra, FORMAT='(F6.2)', /PRINT)+', n=' + STRTRIM(countfin_mod_gra,2), COLOR='blue')
  u = tmpEcGPP
  v = tmpSimGPP
  range = [MIN([u, v],/NAN),MAX([u, v],/NAN)]
  range = [FLOOR(MIN([u, v],/NAN)),CEIL(MAX([u, v],/NAN))]
  ngrid = range[1]-range[0]
  DensityAndFit_log_scale, u, v, 'EC GPP (gC $m^{-2}$ $d^{-1}$)', 'MOD17 GPP (gC $m^{-2}$ $d^{-1}$)', out_dir, range, range, ngrid, nlevel, TITLE='Grass', $
    PLOT1TO1 = 1, DOFIT=1, SIMPLE =1, DOLOG= dolog,  FULL_FILENAME_IMPOSED = 'AAA_densScatter_ECvsMOD_grass'
ENDIF


h_scatter_sim.Save, out_dir + '\' + 'AAA_Overall_scatterplot.png', BORDER=10, RESOLUTION=300

dlmtr=','
OPENW, lun, overall_run_reportFn, /GET_LUN
PRINTF, lun, 'GPP EC vs SIM,,,,,,,,,,,,,GPP EC vs Mod'
PRINTF, lun, 'CRO,,,,,,GRA,,,,,,All,,,,,,CRO,,,,,,GRA,,,,,,All'
PRINTF, lun, 'n_8day_comp,r2,RMSE,MBE,offset,gain,n_8day_comp,r2,RMSE,MBE,offset,gain,n_8day_comp,r2,RMSE,MBE,offset,gain,n_8day_comp,r2,RMSE,MBE,offset,gain,n_8day_comp,r2,RMSE,MBE,offset,gain,n_8day_comp,r2,RMSE,MBE,offset,gain'
PRINTF, lun, STRJOIN([STRING(countfin_sim_cro), STRING(r_sim_cro^2), STRING(RMSE_sim_cro), STRING(MBE_sim_cro), STRING(a_sim_cro),STRING(b_sim_cro[0]), $
                      STRING(countfin_sim_gra), STRING(r_sim_gra^2), STRING(RMSE_sim_gra), STRING(MBE_sim_gra), STRING(a_sim_gra),STRING(b_sim_gra[0]), $
                      STRING(countfin_sim), STRING(r_sim^2), STRING(RMSE_sim), STRING(MBE_sim), STRING(a_sim),STRING(b_sim[0]), $
                      STRING(countfin_mod_cro), STRING(r_mod_cro^2), STRING(RMSE_mod_cro), STRING(MBE_mod_cro), STRING(a_mod_cro),STRING(b_mod_cro[0]), $
                      STRING(countfin_mod_gra), STRING(r_mod_gra^2), STRING(RMSE_mod_gra), STRING(MBE_mod_gra), STRING(a_mod_gra),STRING(b_mod_gra[0]), $
                      STRING(countfin_mod), STRING(r_mod^2), STRING(RMSE_mod), STRING(MBE_mod), STRING(a_mod),STRING(b_mod[0])] + dlmtr)
FREE_LUN, lun
win = GetWindows(NAMES=winNames)
FOR i = 0, N_ELEMENTS(win)-1 DO win[i].close
RETURN, 0
END