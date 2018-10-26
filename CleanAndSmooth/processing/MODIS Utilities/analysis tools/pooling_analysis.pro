PRO pooling_analysis
;to analyse and plot the rsults at division level
path = 'K:\ILRI\Comparison results\aggregate files\Results'
file = path + '\' + 'cumNDVI_aggregated_GIMMS.csv_DIVcomparision02_07.csv'
target = 'MODIS_C5_AQUA';'MODIS_C5_TERRA';'MODIS_C5_AQUA'
data = READ_CSV(file, HEADER=hdr)
archive = data.(0)
;extract only those with average
ind=WHERE(STRMATCH(archive, 'average*', /FOLD_CASE) EQ 1)
;extract only those with desirearchive
ind2=WHERE(STRMATCH(archive[ind], '*'+target+'*', /FOLD_CASE) EQ 1)

archive = archive[ind[ind2]]
divId = data.(1)[ind[ind2]]

SP_R2cv = data.(3)[ind[ind2]]
DSP_R2cv = data.(7)[ind[ind2]]


NP_L_R2cv = data.(15)[ind[ind2]]
SP_L_R2cv = data.(16)[ind[ind2]]


NP_S_R2cv = data.(26)[ind[ind2]]
SP_S_R2cv = data.(27)[ind[ind2]]

  
data = DSP_R2cv-SP_R2cv
bs=0.1
mmin = DOUBLE(FLOOR(MIN(data)))
mmax = DOUBLE(CEIL(MAX(data)))
hist = FLOAT(HISTOGRAM(data, BINSIZE=bs, MIN=mmin, MAX=mmax, LOCATIONS=bins))/N_ELEMENTS(data)
tmp = MIN(ABS(bins), indBin0)
indbins = INDGEN(N_ELEMENTS(bins)) 
indneg = WHERE(indbins LT indBin0)
indpos = WHERE(indbins GE indBin0)
p1 = BARPLOT(bins,hist,  XTITLE='DSP R!U2!Dcv!N - SP R!U2!Dcv', YTITLE='Frequency', TITLE='MODIS!DA-NASA', YRANGE=[0,0.4], /NODATA, XSTYLE=1)
p2 = BARPLOT(bins[indneg],hist[indneg], FILL_COLOR='red', /OVERPLOT)
p3 = BARPLOT(bins[indpos],hist[indpos], FILL_COLOR='blue', /OVERPLOT)
neg = WHERE(data LT 0.0, count_neg)
PRINT, count_neg/FLOAT(N_ELEMENTS(data))*100.0

data = SP_L_R2cv-NP_L_R2cv
bs=0.05
mmin = DOUBLE(FLOOR(MIN(data)))
mmax = DOUBLE(CEIL(MAX(data)))
mmax=0.6; 0.75
mmin=-0.6 ;-0.75
hist = FLOAT(HISTOGRAM(data, BINSIZE=bs, MIN=mmin, MAX=mmax, LOCATIONS=bins))/N_ELEMENTS(data)
tmp = MIN(ABS(bins), indBin0)
indbins = INDGEN(N_ELEMENTS(bins)) 
indneg = WHERE(indbins LT indBin0)
indpos = WHERE(indbins GE indBin0)
p1 = BARPLOT(bins,hist,  XTITLE='SP R!U2!Dcv!N - NP R!U2!Dcv!N, LRLD', YTITLE='Frequency', YRANGE=[0,0.5], TITLE='LRLD, MODIS!DA-NASA',/NODATA)
p2 = BARPLOT(bins[indneg],hist[indneg], FILL_COLOR='red', /OVERPLOT)
p3 = BARPLOT(bins[indpos],hist[indpos], FILL_COLOR='blue', /OVERPLOT)
neg = WHERE(data LT 0.0, count_neg)
PRINT, count_neg/FLOAT(N_ELEMENTS(data))*100.0

data = SP_S_R2cv-NP_S_R2cv
bs=0.05
mmin = DOUBLE(FLOOR(MIN(data)))
mmax = DOUBLE(CEIL(MAX(data)))
mmin = -0.6 
mmax = 0.6
hist = FLOAT(HISTOGRAM(data, BINSIZE=bs, MIN=mmin, MAX=mmax, LOCATIONS=bins))/N_ELEMENTS(data)
tmp = MIN(ABS(bins), indBin0)
indbins = INDGEN(N_ELEMENTS(bins)) 
indneg = WHERE(indbins LT indBin0)
indpos = WHERE(indbins GE indBin0)
p1 = BARPLOT(bins,hist,  XTITLE='SP R!U2!Dcv!N - NP R!U2!Dcv!N, SRSD', YTITLE='Frequency', YRANGE=[0,0.5], TITLE='SRSD, MODIS!DA-NASA', /NODATA)
p2 = BARPLOT(bins[indneg],hist[indneg], FILL_COLOR='red', /OVERPLOT)
p3 = BARPLOT(bins[indpos],hist[indpos], FILL_COLOR='blue', /OVERPLOT)
neg = WHERE(data LT 0.0, count_neg)
PRINT, count_neg/FLOAT(N_ELEMENTS(data))*100.0

print, 'Finished' 
END
