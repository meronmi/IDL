FUNCTION align_JD_2_Arbitrary_Cycle, JDarray, frstYearOfTimeSeries, frstDoyOfCycleOut, frstYearOfCycleOut
; Purpose:
;   Align the output of pheno365 algorithm (Julian Day) JDarray, to 
;   an arbitrary yearly cycle starting DOY frstDoyOfCycleOut. It returns an array of index.
;   
;   PE is the pheno event used for alignement (can be SOS, EOS, other events)
;   In order to define the correct match between the PE and the arbitrary annual cycle 
;   we use the criterion of minimum distance of PEs with respect to the midpoint of the 
;   arbitrary annual cycle. 
;   We exploit here the fact that the PEs time series is ordered by algorithm definition.
;   So there are two possibility for matching it with the midpoint of the arbitrary cycle:
;   midpoints align with the previous or with the following PEs. Which one is correct is 
;   decided by finding the matching minimizing the sum of square (PEs(i)-midpoint(i)).  
;
; Input parameters: 
;   JDarray: array of PEs expressed in Julian Days
;   frstYearOfTimeSeries: the first year in the time series (1998 for SPOT)
;   frstDoyOfCycleOut: aribtrary first DOY of the of the output cycle
;   frstYearOfCycleOut: first calendar year in the output cycle. Must be < of the sirst year of the JDarray      
;
; Output parameters:
;   Array (dimension = number of years in the ouput cycle Y) pointing to
;   the correct band in the PE array (JDarray). Elements not pointing to any band
;   are set to -999
;
; Restrictions:
;  JDarray must represent a pheno events (e.g. SOS) output of Pheno so that only one
;  event is recorder each year 


n_year_in = N_ELEMENTS(JDarray)
;check that first calendar year in the output cycle is smaller than the one in input
IF (frstYearOfCycleOut GE frstYearOfTimeSeries) THEN STOP

n_year_out = n_year_in + (frstYearOfTimeSeries - frstYearOfCycleOut)

;Manage the case when all pdekad are NaN or failures
indf = WHERE((FINITE(JDarray) EQ 1) AND (JDarray NE -999), countf)
IF (countf EQ 0) THEN BEGIN ;this profile would have no data at all. It shoul be impossible with pheno after 17 aug 2011), because periodicity is found but all failed (all -999). Anyhow, data are never present, this shortcut should work (?)
  tmp = INDGEN(n_year_out) & tmp[n_year_out-1]=-999
  RETURN, tmp 
END
;Replace -999 with Nan
JDarrayNaN = JDarray
ind999 = WHERE(JDarrayNaN EQ -999, count999)
IF (count999 NE 0) THEN JDarrayNaN[ind999] = !VALUES.F_NAN
;generate the output cycle midpoints vector expressed in Julian Days
DOY_MidCycle = FLOOR(frstDoyOfCycleOut+365/2.0)
years = INDGEN(n_year_out) + frstYearOfCycleOut
midCycleJD =  DOY_YEAR2JD(years*0+DOY_MidCycle, years)

;now align it according to the criterion of minimum distance with respect to the midpoint
cost = FLTARR(n_year_out-n_year_in + 2)
indf = WHERE(FINITE(JDarrayNaN) EQ 1)
a = JDarrayNaN[indf]
;MM 17/02/2012. it may happen that the minimum is not reached within (n_year_out-n_year_in)
FOR i = 0, (n_year_out - n_year_in + 1) DO BEGIN
  ii = i + n_year_in - 1
  IF (ii GT N_ELEMENTS(midCycleJD) - 1) THEN ii = N_ELEMENTS(midCycleJD) - 1  
  b = midCycleJD[i:ii]
  b = b[indf]
  cost[i] = TOTAL((a - b)^2, /DOUBLE)
ENDFOR 

mincost = MIN(cost, minind)
res = INTARR(n_year_out) - 999
ii = minind + n_year_in - 1
IF (ii GT N_ELEMENTS(res) - 1) THEN ii = N_ELEMENTS(res) - 1
res[minind : ii] = INDGEN(ii - minind + 1)

RETURN, res
END