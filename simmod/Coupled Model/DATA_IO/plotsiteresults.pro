FUNCTION plotSiteResults, lombratio, siteEc_JD, siteSimNDVIJD, NDVIobsJD, siteEc_gpp, siteResmplSimGpp_offSeason, siteModisGpp, siteModisNDVI,  siteSimNDVI, igbp, code, site_run_reportFn, out_dir

fs = 8 ;font size
lp = [1.0, 1] ;legend position
dm = [900,900]  ;dimensions of the window
xminor = 2        ;monor ticks between majors
nmajor = 4
ticlensec = 0.03
xmin = 0.05
ymin = 0.725 ;(0.605)
graph_dim = 0.25
pos = [xmin, ymin, xmin + graph_dim*3.75, ymin+graph_dim] ;0.24 in width and height
dummy = LABEL_DATE(DATE_FORMAT=['%D/%N','%Y'])
xrange = [MIN(siteEc_JD), MAX(siteEc_JD)]
yrange = [-5, 25.0]
;yrange = [MIN([siteEc_gpp, siteResmplSimGpp_offSeason, siteModisGpp], /NAN),MAX([siteEc_gpp, siteResmplSimGpp_offSeason, siteModisGpp], /NAN)]

gh_ec = PLOT(siteEc_JD, siteEc_gpp, YTITLE='Daily GPP (gC m-2 d-1)', COLOR = 'red', XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XMINOR=xminor, $
  FONT_SIZE = fs, POSITION=pos, DIMENSIONS=dm, $
  XRANGE=xrange, Name='Measured', YRANGE=yrange, WINDOW_TITLE = igbp + '_' + code)
strlabel = STRTRIM(ROUND(jd2doy([xrange[0], xrange[0]+(xrange[1]-xrange[0])/(nmajor-1), xrange[0]+2*(xrange[1]-xrange[0])/(nmajor-1), xrange[1]])),2)
loc = yrange[0]-(yrange[1]-yrange[0])/10.0
a_x = AXIS('X', TARGET = gh_ec, LOCATION=loc, MAJOR = nmajor, MINOR=xminor, COORD_TRANSFORM=[-siteEc_JD[0]+JD2DOY(siteEc_JD[0]), 1], TICKFONT_SIZE = fs, TITLE='Time', TICKLEN=ticlensec, TICKNAME=strlabel)
gh_sim = PLOT (siteEc_JD, siteResmplSimGpp_offSeason, COLOR = 'black', OVERPLOT = 1, Name='Sim',  YRANGE=yrange)
gh_Modis = PLOT (siteEc_JD, siteModisGpp, COLOR = 'green', OVERPLOT = 1, Name='MOD17',  YRANGE=yrange)
!null = LEGEND(target=[gh_ec, gh_sim, gh_Modis], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
  SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.1, TRANSPARENCY=100)

;NDVI
;yrangend = [MIN([siteModisNDVI,siteSimNDVI], /NAN),MAX([siteModisNDVI,siteSimNDVI], /NAN)]
yrangend = [MIN([0.0, MIN([siteModisNDVI,siteSimNDVI])]),1.0]
pos = [xmin, ymin-0.34, xmin + graph_dim*3.75, ymin+graph_dim-0.34]
gh_nd = PLOT(NDVIobsJD, siteModisNDVI, YTITLE='NDVI (-)', COLOR = 'red', XTICKFORMAT='(C(CDI,1x,CMoA,1x,CYI2))', XMINOR=xminor, $
  FONT_SIZE = fs, POSITION=pos, DIMENSIONS=dm, /CURRENT, XRANGE=xrange, Name='MODIS', YRANGE=yrangend, LINESTYLE='none', SYMBOL='+')
strlabel = STRTRIM(ROUND(jd2doy([xrange[0], xrange[0]+(xrange[1]-xrange[0])/(nmajor-1), xrange[0]+2*(xrange[1]-xrange[0])/(nmajor-1), xrange[1]])),2)
loc = yrangend[0]-(yrangend[1]-yrangend[0])/10.0
a_x = AXIS('X', TARGET = gh_nd, LOCATION=loc, MAJOR = nmajor, MINOR=xminor, COORD_TRANSFORM=[-siteEc_JD[0]+JD2DOY(siteEc_JD[0]), 1], TICKFONT_SIZE = fs, TITLE='Time', TICKLEN=ticlensec, TICKNAME=strlabel)
indf = WHERE(FINITE(siteSimNDVI))
gh_sim = PLOT (siteSimNDVIJD[indf], siteSimNDVI[indf], COLOR = 'black', OVERPLOT = 1, Name='Sim',  YRANGE=yrangend, LINESTYLE='none', SYMBOL='o', SYM_SIZE=0.5)
!null = LEGEND(target=[gh_nd, gh_sim], /AUTO_TEXT_COLOR, FONT_SIZE = fs-1, POSITION=lp, /RELATIVE, $
  SHADOW=0, LINESTYLE=6, SAMPLE_WIDTH=0.1, TRANSPARENCY=100)

;scatter plots
;SIM
pos = [xmin, ymin-0.68, xmin + graph_dim, ymin + graph_dim-0.68] 

yrange=[MIN([siteEc_gpp, siteResmplSimGpp_offSeason, siteModisGpp], /NAN)-0.2, MAX([siteEc_gpp, siteResmplSimGpp_offSeason, siteModisGpp], /NAN)+0.2]
;yrange=[-0.5, MAX([siteEc_gpp, siteResmplSimGpp_offSeason], /NAN)+0.2]
h_scatter = PLOT(siteEc_gpp, siteResmplSimGpp_offSeason, SYMBOL='+', LINESTYLE='none', XRANGE=yrange, YRANGE=yrange, $
  FONT_SIZE = fs, POSITION=pos, /CURRENT, XTITLE='Measured GPP (gC m-2 d-1)', YTITLE='sim GPP (gC m-2 d-1)')
gh_11 = PLOT(yrange, yrange, OVERPLOT=1, COLOR='green', NAME='1:1 line', LINESTYLE='--')
indfin = WHERE(FINITE(siteEc_gpp), countfin_sim)
b_sim = REGRESS(siteEc_gpp[indfin], siteResmplSimGpp_offSeason[indfin], CONST = a_sim, CORRELATION = r_sim)
rmse_sim = SQRT(TOTAL((siteEc_gpp[indfin]-siteResmplSimGpp_offSeason[indfin])^2)/FLOAT(countfin_sim))
MBE_sim = TOTAL(siteEc_gpp[indfin]-siteResmplSimGpp_offSeason[indfin]) / FLOAT(countfin_sim) 
;trick to avoid plotting outside
x0=(0.0-a_sim)/b_sim[0]
x1=(yrange[1]-a_sim)/b_sim[0]
IF (x0 LT 0) THEN x0=0.0
yreg = a_sim+b_sim[0]*[x0,x1]
gh_reg = PLOT([x0,x1], yreg, OVERPLOT=1, COLOR='blue', NAME='Reg, R2='+STRTRIM(r_sim^2,2), YRANGE=yrange)
th1 = TEXT(pos[2]-0.2, pos[1]+graph_dim-graph_dim/10.0, '$R^2$='+STRING(r_sim^2, FORMAT='(F4.2)', /PRINT)+', RMSE=' + STRING(RMSE_sim, FORMAT='(F4.2)', /PRINT), COLOR='blue')
;Modis
pos[[0,2]]= pos[[0,2]] + 0.30
h_scatter = PLOT(siteEc_gpp, siteModisGpp, SYMBOL='+', LINESTYLE='none', XRANGE=yrange, YRANGE=yrange, color='green', $
  FONT_SIZE = fs, POSITION=pos, /CURRENT, XTITLE='Measured GPP (gC m-2 d-1)', YTITLE='MOD17 GPP (gC m-2 d-1)')
gh_11 = PLOT(yrange, yrange, OVERPLOT=1, COLOR='green', NAME='1:1 line', LINESTYLE='--')
indfin = WHERE(FINITE(siteEc_gpp) AND FINITE(siteModisGpp), countfin_mod)
b_mod = REGRESS(siteEc_gpp[indfin], siteModisGpp[indfin], CONST = a_mod, CORRELATION = r_mod)
rmse_mod = SQRT(TOTAL((siteEc_gpp[indfin]-siteModisGpp[indfin])^2)/FLOAT(countfin_mod))
MBE_mod =  TOTAL(siteEc_gpp[indfin]-siteModisGpp[indfin])/FLOAT(countfin_mod)
;trick to avoid plotting outside
x0=(0.0-a_mod)/b_mod[0]
x1=(yrange[1]-a_mod)/b_mod[0]
IF (x0 LT 0) THEN x0=0.0
yreg = a_mod+b_mod[0]*[x0,x1]
gh_reg = PLOT([x0,x1], yreg, OVERPLOT=1, COLOR='blue', NAME='Reg, R2='+STRTRIM(r_mod^2,2), YRANGE=yrange)
th1 = TEXT(pos[2]-0.2, pos[1]+graph_dim-graph_dim/10.0, '$R^2$='+STRING(r_mod^2, FORMAT='(F4.2)', /PRINT)+', RMSE=' + STRING(RMSE_mod, FORMAT='(F4.2)', /PRINT), COLOR='blue')

gh_ec.Save, out_dir + '\' + 'Site_' + igbp + '_' + code + '.png', BORDER=10, RESOLUTION=300

dlmtr=','
res = FILE_SEARCH(site_run_reportFn)
IF (res EQ '') THEN BEGIN
  ;it does not exists, open it and write hdr
  OPENW, lun, site_run_reportFn, /GET_LUN
  PRINTF, lun, 'Site, IGBP, Lomb-ratio,GPP EC vs SIM,,,,,GPP EC vs Mod'
  PRINTF, lun, ',,,n_8day_comp,r2,RMSE,MBE,offset,gain,n_8day_comp,r2,RMSE,MBE,offset,gain  
ENDIF ELSE BEGIN
  ;it exists, no hdr needed, apppend results
  OPENW, lun, site_run_reportFn, /GET_LUN, /APPEND
ENDELSE
;write results
PRINTF, lun, STRJOIN([code, igbp, STRING(lombratio), STRING(countfin_sim), STRING(r_sim^2),STRING(RMSE_sim),STRING(MBE_sim), STRING(a_sim),STRING(b_sim[0]),$
                                  STRING(countfin_mod), STRING(r_mod^2),STRING(RMSE_mod),STRING(MBE_mod),STRING(a_mod),STRING(b_mod[0])] + dlmtr)
;close and exit
FREE_LUN, lun
win = GetWindows(NAMES=winNames)
FOR i = 0, N_ELEMENTS(win)-1 DO win[i].close
RETURN, 0
END