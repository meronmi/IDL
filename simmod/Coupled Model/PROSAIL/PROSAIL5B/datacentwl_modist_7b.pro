; PROCEDURE COMMENTS +
; NAME: datacentwl_MODIST_7b
; AUTHOR: Michele Meroni
;
; CONTACT INFO: michele.meroni@gmail.com
; DESCRIPTION:
; MODIS Terra center wavelength 7 bands from http://modis.gsfc.nasa.gov/about/specifications.php

; CALLING SEQUENCE:
;       output = datacentwl_MODIST_7b()
; INPUTS: none
; OUTPUTS: Array 7 Columns as follows: 
; Center_wl_B1  Center_wl_B2  Center_wl_B3  Center_wl_B4  Center_wl_B5  Center_wl_B6  Center_wl_B7
;
; OPTIONAL OUTPUTS:
; OPTIONAL INPUT KEYWORD(S):
; NOTES:
; METHOD:
; EXAMPLE:
; MODIFICATION HISTORY: written on the 14 July 2014
; CODING NOTES:
; 
; 
FUNCTION datacentwl_MODIST_7b

cwl = [645.0, 858.5, 469.0, 555.0, 1240.0, 1640.0, 2130.0]

RETURN, cwl
END