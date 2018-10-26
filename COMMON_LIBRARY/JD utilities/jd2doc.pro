FUNCTION JD2DOC, JDarray, frstDOYofCycle, AVG_VEC = avg_vec
;  Purpose:
;     Transform an array of Julian Days into an array of DOC (Day OF the Cycle, is
;     DOY considering the first day of the Cycle as DOY=1)
;     Note that if a element expected to fall in DOCx, and then fall in DOCx-1
;     or DOCx+1, it will have a negative or GT 365 DOC, respectively
;     
;  Input parameters: 
;     JDarray: array of Julian days
;     frstDOYofCycle: first day (DOY) of the cycle (1 if calendar year, 2-365 otherwise)
;

;  Return values:
;     array of DOC using positive and negative values (see debug example below)
;     optionally return an array of dimension equal to JDarray 
;     with all equal elements set to the avg DOC (expressed in JD) 

;  History:
;     V 1.0:  MM 9/8/2011
;
;  Example:
;  a = [DOY_YEAR2JD(1,2012), DOY_YEAR2JD(10,2013)]
;  print, JD2DOC(a,1)

;  Debug
;  create a JDarray 
;  x = DOY_YEAR2JD([5,10,7,3,12,340,4], [1998,1999,2000,2001,2002,2002,2004])
;  PRINT, JD2DOC(x, 2)

n = N_ELEMENTS(JDarray)

;Store positions of season failures
ind999=WHERE(JDarray EQ -999, count999)

;Manage the case when all pdekad are NaN or failures
indf = WHERE((FINITE(JDarray) EQ 1) AND (JDarray NE -999), countf)
IF (countf EQ 0) THEN BEGIN ;this profile would have no data at all (masked data). It shoul be impossible othwerise (with pheno after 17 aug 2011), because periodicity is found but all failed (all -999).) 
  dataOut = JDarray * !VALUES.F_NAN
  IF KEYWORD_SET(avg_vec) THEN avg_vec = dataOut
ENDIF ELSE BEGIN

  
  ;retrive the first year in the JD array
  indJD=WHERE((FINITE(JDarray) EQ 1) AND (JDarray NE -999), countJD)
  minyear = MIN(JD2YEAR(JDarray[indJD]), minInd)
  ;the min year so far is the minimum of those that are not NaN or -999, e.g. 2001, I have to go back to 1998 in the case of VGT
  IF (indJD[minInd] NE 0) THEN minyear = minyear - indJD[minInd]
  ;generate the cycle midpoints vector expressed in Julian Days
  DOY_MidCycle = FLOOR(frstDOYofCycle+365/2.0)
  IF (DOY_MidCycle GT 365) THEN DOY_MidCycle = DOY_MidCycle - 365
  years = (INDGEN(n+6)+minyear-3)  ;3 more eon ecah side
  midCycleJD =  DOY_YEAR2JD(years*0+DOY_MidCycle, years)
  ;Find the correct alignment between the vector of input JDs and the prescribe cycle
  ;by minimizing the distance of all JDs to the cycle midponts
  cost = FLTARR(6)
  FOR i =0, 5 DO BEGIN
    b = midCycleJD[i:i+n-1]  
    cost[i] = TOTAL(((JDarray[indf] - b[indf]))^2, /DOUBLE, /NAN)
  ENDFOR
  ;find the array of midpoint providing the minimum difference
  mincost = MIN(cost, minInd, /NAN)
  ;frstYear = JD2YEAR(midCycleJD[minInd])
  ;now compute the first day of the Correct cycle
  day0CycleJD =  midCycleJD[minInd:minInd+n-1]-FLOOR(365/2.0) 
  ;day0CycleJD = DOY_YEAR2JD(INTARR(n)*0+frstDOYofCycle-1, INDGEN(n)+frstYear)
  ;now espress the input array as the difference with the above first day
  dataOut = JDarray - day0CycleJD + 1
  IF KEYWORD_SET(avg_vec) THEN BEGIN
    avg_vec =  MEAN(dataOut[indf], /NAN)
    avg_vec = day0CycleJD + avg_vec
    ;avg_vec = DOY_YEAR2JD(INTARR(n)*0+avg_vec, INDGEN(n)+frstYear)
    ;PRINT, ''
  ENDIF
  IF (count999 NE 0) THEN dataOut[ind999] = -999
ENDELSE

RETURN, dataOut
END