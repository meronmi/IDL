Function pdekad2dekoy, pdekad, ord_offset_dek, start_caldendar_year
;Purpose:
;compute dekoy (dekad of caldendar year) and the calendar year from 'pdekad',
;an array of progressive number of dekads from a given start dekad ('ord_offset_dek') 
;of a give calendar year ('start_calendar_year')
;
;Be care: aligned data starts from 0 (ord_offset_dek)!
;
;Afterwards, to avg this data, use average circular data (avg_dec)
;
;-SHOULD NEVER BE USE WITH TZP data (True Zero Point) 
;-treat NaN and -999 (season failure) properly (they are conserved)
;
;INPUT:  
;- pdekad: progressive number of dekads from 
;- ord_offset_dek: ordinal dekad offsetting the data (10 for VGT pheno results, 1 for realigned)
;- start_calendar_year (start_caldendar_year = 0, res is only one line, the first)
;
;NOTE: if only dekoy is required, start_caldendar_year set to 0 make the code faster
  
;RETURNED VALUE
; -res: array of N_ELEMENTS(pdekad) columns and two lines
;       res[*,0] = dekoy
;       res[*,1] = calendar year of the dekoy
;  if start_caldendar_year = 0, res is only one line, the first

;Example
;pdekad=[1,36,73]
;res=pdekad2dekoy(pdekad, 10, 1998)

abs_offset_dek = ord_offset_dek - 1

IF (start_caldendar_year EQ 0) THEN $
  res=FLTARR(N_ELEMENTS(pdekad))*!VALUES.F_NAN $
ELSE $
  res=FLTARR(N_ELEMENTS(pdekad),2)*!VALUES.F_NAN

;Store the position of -999 to treat them properly
ind999=WHERE(pdekad EQ -999, count999)
;Transform pdekad from ord_offset_dek to pdekad from 1
;and express it as dekad of the year (which is MOD 36)
dekoy=(pdekad+abs_offset_dek) MOD 36
;mod 36 eq 0 has to be dek 36
ind=WHERE(dekoy EQ 0, count)
IF (count GT 0) THEN dekoy[ind] = 36
;report -999 in their original position
IF (count999 NE 0) THEN dekoy[ind999] = -999
;compute calendar year of each dekad if required
IF (start_caldendar_year EQ 0) THEN BEGIN
  res = dekoy
ENDIF ELSE BEGIN
  res[*,0] = dekoy
  indf=WHERE(FINITE(pdekad) EQ 1, countf)
  ;indn=WHERE(FINITE(pdekad) EQ -999, countn) ;to suprress -999
  res[indf,1] = FLOOR((pdekad[indf]+abs_offset_dek-1)/36.0)+start_caldendar_year
  IF (count999 NE 0) THEN res[ind999,1]=-999
ENDELSE
RETURN, res
END



