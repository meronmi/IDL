FUNCTION Sentinel_pheno_plot_by_grid, dir, var_set
;plot a map, per crop or crop set, of a pheno variable 
;(as derived from S2 and S1) and of the correlation and RMSE, ME, MAE, of the same variable as obtained from S2 and S1 
;dir =  'D:\RAPHAEL_pheno_test\Results_2019_6_17'
csv_file = dir + '\pheno_results.csv'
dir_out = dir + '\Pheno_grid_images'
FILE_MKDIR, dir_out
plevel = 0.05
minN4regression = 10
grid_size_deg = 1;2;1     ;grid_size_degree
;crop types to analyze together
crop_set = raphael_crop_select()
crop_set = crop_set[0]
;if I want to get one by one
;crop_code_set= crop_code_set[i]

;var to analyze
basic_var_set = var_set ;['EOS50']; ['SOS50']

;indexes to consider, CANNOT BE CHANGED
index_set =['NDVIm','CR_ad','RVI_ad']

;open pheno results to get min and max of lat lon
res = READ_CSV(csv_file, HEADER=hdr, MISSING_VALUE=!VALUES.F_NAN )
x = rename_tags(res, TAG_NAMES(res), [hdr[0:-2], 'Empty'])
;x = rename_tags(res, TAG_NAMES(res), hdr)
;;make a code variable from the long name in x.crop_type 
;crop_code = !NULL
;FOR i = 0, N_ELEMENTS(x.crop_type)-1 DO BEGIN
;  tmp = STRSPLIT(x.crop_type[i],'-',/EXTRACT)
;  tmp = STRTRIM(tmp[-1],2)
;  crop_code = [crop_code, tmp]
;ENDFOR


minmaxLat = [FLOOR(MIN(x.Lat)),CEIL(MAX(x.Lat))]
minmaxLon = [FLOOR(MIN(x.Lon)),CEIL(MAX(x.Lon))]
nLines = CEIL((minmaxLat[1] - minmaxLat[0])/FLOAT(grid_size_deg)); + 1
nColumns = CEIL((minmaxLon[1] - minmaxLon[0])/FLOAT(grid_size_deg)) ;+ 1


FOR j = 0, N_ELEMENTS(basic_var_set)-1 DO BEGIN
  tags = TAG_NAMES(x)
  subVar = WHERE(tags EQ basic_var_set[j], count)
  IF (count EQ 0) THEN STOP
  bandNames = basic_var_set[j] + ['_' + STRMID(index_set[0],0, STRLEN(index_set[0])-1) + ['_mean', '_sd', '_n'], $ ;NDVIm
                                  '_' + index_set[1] + ['_mean', '_sd', '_n'], $ ;CRad
                                  '_' + index_set[2] + ['_mean', '_sd', '_n'], $ ;RVIad
                                  '_' + STRMID(index_set[0],0, STRLEN(index_set[0])-1) + 'vs' + index_set[1] + ['_r-99ns', '_RMSE', '_MAE', '_ME', '_n'], $ ; NDVIm vs CRad
                                  '_' + STRMID(index_set[0],0, STRLEN(index_set[0])-1) + 'vs' + index_set[2] + ['_r-99ns', '_RMSE', '_MAE', '_ME', '_n']]   ; NDVIm vs RVIad
  bandContent = ['mean', 'sd', 'n', $
                 'mean', 'sd', 'n', $
                 'mean', 'sd', 'n', $
                 'r', 'RMSE', 'MAE', 'ME', 'nn', $
                 'r', 'RMSE', 'MAE', 'ME', 'nn']                                   
  firstElementGroup = [0,3,6,9,14]    ;points to the first band of each of the above lines
  ind_of_n = [2,5,8,13,18]                                
  mat = FLTARR(nColumns, nLines, N_ELEMENTS(bandNames))
  FOR u = 0, 1 DO BEGIN ;all data,  and only selected based on gaps and perror
    ;ATTENZIONE MATCH NON ACCETTA E NON VEDE DUPLICATI
    suba = !NULL
    FOR cs = 0, N_ELEMENTS(crop_set)-1 DO BEGIN
      suba = [suba, WHERE(x.crop_type EQ crop_set[cs], /NULL)]
    ENDFOR
    ;cannot use: res = raphael_data_select(x, u, indices[k], bigCropTypes[i])
    CASE u OF
      0: BEGIN 
        ind = WHERE(x.(subVar)[suba] NE -9999, count)
        ind = suba[ind]
        varSelction = 'All retrieval';, n=' +STRTRIM(N_ELEMENTS(ind),2)
        suffix = 'all_retrieval'
      END
      1: BEGIN
        ind = WHERE(x.(subVar)[suba] NE -9999, count)
        ind = suba[ind]
        ids = x.ID[ind]
        ind2Keep = !NULL
        ;check which ID has NDVI prctHeterogObs_inSeason less than 20 and keep those only
        FOR i = 0, count-1 DO BEGIN
          indNDVI = WHERE((x.ID EQ ids[i]) AND (x.Index EQ 'NDVIm'))
          IF x.prctHeterogObs_inSeason[indNDVI] LE 20 THEN ind2Keep = [ind2Keep, ind[i]]
        ENDFOR
        ind = ind2Keep
        varSelction = 'inSeasonNDVI_Het<20%'
        suffix = 'inSeasNdviHetLT20'
      END
    ENDCASE
    ;ind stores the subscripts of all records belonging to that crop_set, with no disticintion about type of index ot crop or geolocation 
    ;I have to fill several data, the mean var NDVI and VH/VV, n the number of sample, the measures of correlation and errror   
    ;Now loop on the the grid and see whchi cells can we fill
    FOR l = 0, nLines-1 DO BEGIN
      FOR c = 0, nColumns-1 DO BEGIN
        ;see what is available in this grid cell 
        ;range of lat and lon of this grid cell 
        latGrid = [minmaxLat[1] - l * grid_size_deg, minmaxLat[1] - (l+1) * grid_size_deg]
        lonGrid = [minmaxLon[0] + c * grid_size_deg, minmaxLon[0] + (c+1) * grid_size_deg]
        ;IF (latGrid[1] EQ 55) AND (lonGrid[1] EQ 0) THEN STOP
        indGrid = WHERE((x.Lat[ind] LE latGrid[0]) AND (x.Lat[ind] GT latGrid[1]) AND $
                        (x.Lon[ind] GE lonGrid[0]) AND (x.Lon[ind] LT lonGrid[1]), countGrid) ;closed on top and left, open on bottom and right
        
        IF (countGrid EQ 0) THEN BEGIN
          mat[c,l,*] = !VALUES.F_NAN
          mat[c,l,ind_of_n] = !VALUES.F_NAN   ;set numebr of samples to 0          
        ENDIF ELSE BEGIN
          indGrid = ind[indGrid]
          ;we have some data
          ;store results for single variables 
          FOR u = 0, N_ELEMENTS(index_set)-1 DO BEGIN
            indGridI = WHERE(x.index[indGrid] EQ index_set[u], countGridI)
            indGridI = indGrid[indGridI]
            IF (countGridI EQ 0) THEN BEGIN
              mat[c,l,firstElementGroup[u]:firstElementGroup[u]+1] = !VALUES.F_NAN
              mat[c,l,firstElementGroup[u]+2] = !VALUES.F_NAN
            ENDIF ELSE BEGIN
              ;'_mean'
              mat[c,l,firstElementGroup[u]] = MEAN(x.(subVar)[indGridI])
              IF (MEAN(x.(subVar)[indGridI]) EQ -9999) THEN STOP
              ;'_sd'
              mat[c,l,firstElementGroup[u]+1] = STDDEV(x.(subVar)[indGridI])
              ;'_n'
              mat[c,l,firstElementGroup[u]+2] = countGridI
            ENDELSE
          ENDFOR ; u
          ;store results for the comparison between variables
          FOR v = 1, 2 DO BEGIN
            indGridS2 = WHERE(x.index[indGrid] EQ index_set[0], countGridS2)
            indGridS2 = indGrid[indGridS2]
            indGridS1 = WHERE(x.index[indGrid] EQ index_set[v], countGridS1)
            indGridS1 = indGrid[indGridS1]
            ;'_r' to '_ME'
            mat[c,l,firstElementGroup[v+2]:firstElementGroup[v+2]+3] = !VALUES.F_NAN
            ;'_n'
            mat[c,l,firstElementGroup[v+2]+4] = 0
            IF ((countGridS2 GT 0) AND (countGridS1 GT 0)) THEN BEGIN
              ;now I have to match them so that they point to the same samples 
              idS2 = x.ID[indGridS2]
              idS1 = x.ID[indGridS1]
              MATCH, idS2, idS1, subS2, subS1, COUNT = count
              IF (count GT 0) THEN BEGIN 
                indS2 = indGridS2[subS2]
                indS1 = indGridS1[subS1]
                IF (N_ELEMENTS(indS2) NE N_ELEMENTS(indS1)) THEN STOP
                mat[c,l,firstElementGroup[v+2]:firstElementGroup[v+2]+3] = !VALUES.F_NAN
                ;'_r-99ns' 
                res = linregstat(x.(subVar)[indS2], x.(subVar)[indS1], MINSAMPLESIZE = minN4regression)
                IF (FINITE(res[3]) EQ 1) THEN BEGIN ;can be nan if there are less than minN4regression obs
                  IF (res[3] LT plevel) THEN mat[c,l,firstElementGroup[v+2]] = res[2] ELSE  mat[c,l,firstElementGroup[v+2]] = -99
                ENDIF
                es = error_stats(x.(subVar)[indS2], x.(subVar)[indS1])
                ;'_RMSE'
                mat[c,l,firstElementGroup[v+2]+1] = es.rmsd
                ;'_MAE'
                mat[c,l,firstElementGroup[v+2]+2] = es.mae
                ;'_ME'
                mat[c,l,firstElementGroup[v+2]+3] = es. me
                ; '_n'
                mat[c,l,firstElementGroup[v+2]+4] = N_ELEMENTS(indS2)    
              ENDIF
            ENDIF  
          ENDFOR ;v
        ENDELSE
      ENDFOR ; c
    ENDFOR  ;l
  ENDFOR  ;u
  ;now i have my maps of variable basic_var_set , plot them
  FOR i = 0, N_ELEMENTS(bandNames)-1 DO BEGIN
    PRINT, bandNames[i]
    ;be care filpping
    ;If GRID_UNITS is in "degrees" then IMAGE_LOCATION should be set to the longitude and lat (x, y) of the lower-left corner of the image
    ;mat2= mat
    imgLoc = [minmaxLon[0], minmaxLat[0]];FLOAT([minmaxLon[0], minmaxLat[0]])
    rgbtab = COLORTABLE(72, /REVERSE)
    ;minmaxLat = FLOAT(minmaxLat)
    ;minmaxLon=FLOAT(minmaxLon)
    cbtitle = bandNames[i]
    
    tmp = REVERSE(REFORM(mat[*,*,i]),2)
    IF (STRMID(bandNames[i],1,2,/REVERSE_OFFSET) EQ '_n') THEN BEGIN
      tmp = ALOG10(tmp)
      cbtitle = cbtitle + ' (log scale)'
    ENDIF
    ;get overall min max when plotting mean, sd and n of a single pheno var
    CASE bandContent[i] OF
      'mean': MinMax = [MIN(mat[*,*,[0,3,6]], /NAN), MAX(mat[*,*,[0,3,6]], /NAN)]
      'sd': MinMax = [MIN(mat[*,*,1+[0,3,6]], /NAN), MAX(mat[*,*,1+[0,3,6]], /NAN)]
      'n': MinMax = ALOG10([MIN(mat[*,*,2+[0,3,6]], /NAN), MAX(mat[*,*,2+[0,3,6]], /NAN)])
      'nn': MinMax = [MIN(tmp[*,*], /NAN), MAX(tmp[*,*], /NAN)] ;this is from index1 vs index2
      ELSE: MinMax = [MIN(tmp[*,*], /NAN), MAX(tmp[*,*], /NAN)]
    ENDCASE 
    
    im = IMAGE(tmp,  MAP_PROJECTION='Geographic', DIMENSIONS=[800,800], IMAGE_LOCATION=imgLoc, GRID_UNITS='meters', $
              IMAGE_DIMENSIONS=[FLOAT(grid_size_deg*nColumns), FLOAT(grid_size_deg*nLines)], RGB_TABLE=rgbtab,  MIN_VALUE = MinMax[0], MAX_VALUE = MinMax[1], $
              MARGIN=0.1, BOX_AXES=1,   COLOR='black', GRID_LONGITUDE=10, GRID_LATITUDE=10, $ 
              LATITUDE_MIN = minmaxLat[0], LATITUDE_MAX = minmaxLat[1], LONGITUDE_MIN = minmaxLon[0], LONGITUDE_MAX = minmaxLon[1])
    IF (STRMID(bandNames[i],3,4,/REVERSE_OFFSET) EQ '99ns') THEN BEGIN
      ns = tmp * !VALUES.F_NAN
      ns[WHERE(tmp EQ - 99)] = 1
      rgbtab2 = BYTARR(3, 256) + 150
      im2 = IMAGE(ns,  MAP_PROJECTION='Geographic', DIMENSIONS=[800,800], IMAGE_LOCATION=imgLoc, GRID_UNITS='meters', $
        IMAGE_DIMENSIONS=[FLOAT(grid_size_deg*nColumns), FLOAT(grid_size_deg*nLines)], RGB_TABLE=rgbtab2,  MIN_VALUE = MinMax[0], MAX_VALUE = MinMax[1], $
        MARGIN=0.1, BOX_AXES=1,   COLOR='black', GRID_LONGITUDE=10, GRID_LATITUDE=10, $
        LATITUDE_MIN = minmaxLat[0], LATITUDE_MAX = minmaxLat[1], LONGITUDE_MIN = minmaxLon[0], LONGITUDE_MAX = minmaxLon[1], /OVERPLOT)
        im.MIN_VALUE = MIN(tmp[WHERE(tmp NE -99)],/NAN)     
    ENDIF
    mg = im.mapgrid
    mg.LINESTYLE = 1
    mg.LABEL_POSITION = 1
    mg.TRANSPARENCY= 25
    FOREACH l, mg.longitudes DO l.LABEL_ANGLE = 0
    FOREACH l, mg.latitudes DO l.LABEL_ANGLE = 270
    ;mg.BOX_THICK = 3
    mc = MAPCONTINENTS(/COUNTRIES, COLOR='black', FILL_BACKGROUND=0)

    
    cb = COLORBAR(TITLE=cbtitle, MAJOR=5, TAPER=1, POSITION=[0.2,0.08,0.8,0.12], FONT_SIZE = 12, TARGET=im)
    IF (STRMID(bandNames[i],3,4,/REVERSE_OFFSET) EQ '99ns') THEN BEGIN
      cb2 = COLORBAR(TITLE = 'n.s.', MAJOR=0, TAPER=0, POSITION=[0.1,0.08,0.14,0.12], FONT_SIZE = 12, TARGET=im2, MINOR=0, TICKLAYOUT=1)
      t = TEXT(0.5,0.15,'Min sample size per grid = ' + STRTRIM(minN4regression,2), FONT_SIZE=12, ALIGNMENT=0.5)
    ENDIF
    IF (STRMID(bandNames[i],1,2,/REVERSE_OFFSET) EQ '_n') THEN BEGIN
      cb.TICKVALUES = ALOG10(ROUND(10^cb.TICKVALUES)) 
      cb.TICKNAME = STRTRIM(ROUND(10^cb.TICKVALUES),2)
    ENDIF
    ;mc = MAPCONTINENTS(/COUNTRIES, FILL_COLOR='beige')
    t = TEXT(0.1,0.97,'Crop set:', FONT_SIZE=16)
    t = TEXT(0.5,0.13,'Min,max = ' + STRING(MIN(tmp,/NAN),FORMAT='(F6.2)')+', '+STRING(MAX(tmp,/NAN),FORMAT='(F6.2)'), FONT_SIZE=12, ALIGNMENT=0.5)
    c = 0
    lineTesxt = 1
    str = ''
    FOR s = 0, N_ELEMENTS(crop_set)-1 DO BEGIN
      str = str +  crop_set[s] + ', '
      c = c + 1
      IF ((c MOD 3) EQ 0) OR (s EQ N_ELEMENTS(crop_set)-1) THEN BEGIN
        t = TEXT(0.1,0.96-lineTesxt*0.025, STRMID(str, 0, STRLEN(str)-2), FONT_SIZE=12)
        lineTesxt  = lineTesxt + 1
        str=''
      ENDIF
    ENDFOR  
    str = STRJOIN(crop_set,'_')
    str = STRJOIN(STRSPLIT(str,' ',/EXTRACT))
    im.Save, dir_out + '\' + STRTRIM(grid_size_deg,2) + 'deg_'+ bandNames[i] + '_' + suffix + '_' + str + '.png'
    im.Close
    ;PRINT, 'debug'
  ENDFOR
ENDFOR  ;j
RETURN, 0
END
