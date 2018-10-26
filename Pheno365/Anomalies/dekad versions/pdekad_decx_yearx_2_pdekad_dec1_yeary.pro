
Function pdekad_decX_yearX_2_pdekad_dec1_yearY, pdekad, ord_dek_of_input_01_range, yearX, yearY
;Purpose:
;express input pdekad (expressed as progressive dekads from ordinal ord_dek_of_input_01_range of yearX) as
;pdekad from ordinal dekad 1 of yearY (e.g. ord_dek_of_input_01_range is 10 as for VGT 1998, yearX=1998, 
;yearY=1997, input is [1, 10],
;output is [46, 55])

offset_dek = ord_dek_of_input_01_range - 1

IF yearX LT yearY THEN STOP ;this request looks supid
;Store the position of -999 to treat them properly
ind999=WHERE(pdekad EQ -999, count999)
;Compute the numbers of dekads to add or subtract for the diffrent years
adddec=(yearX-yearY)*36
;Transform pdekad from start_dekoy to 1
res=(pdekad+offset_dek+adddec)
;report -999 in their original position
IF (count999 NE 0) THEN res[ind999] = -999
RETURN, res
END