FUNCTION call2s, pars
path2s = 'D:\Users\meronmi\Documents\IDL\TwoStreamModel-2.4\bin'
exe2s = '2stream.exe'
npars = N_PARAMS()
IF (npars EQ 0) THEN inputs = '' ELSE inputs = pars
;mandatory input (they are output of the model as a remainder)
input_label = ['SZA','Fdiff','LAItrue','Z','Z*','Rbgd','Rleaf','Tleaf']
; (1) sun     : Sun zenith angle in degrees. (if the illumination is purely diffuse: a non-value)
; (2) fdiff   : fraction of diffuse downward radiation.
; (3) <lai>   : True Lai of the canopy.
; (4) zeta    : Structure factor of the canopy for this Sun angle.
; (5) zeta*   : Mean Structure factor of the canopy.
; (6) rbgd    : Background reflectance.
; (7) rleaf   : Leaf reflectance (effective value).
; (8) tleaf   : Leaf transmittance (effective value).

;requested output (can be changed)
oSettings = ' -o "%Alb,%Abs,%Tran,%Abs_t1,%Abs_t3"
;possible outputs
;( 9) %Alb      Total Albedo.
;(10) %Abs      Total Absorption by the vegetation canopy.
;(11) %Tran     Total Transmission to the background level.
;(12) %Alb_t1   Albedo accounting for the Black Background component.
;(13) %Alb_t2   Albedo accounting for the Black Canopy component.
;(14) %Alb_t3   Albedo accounting for the coupled canopy-background term.
;(15) %Abs_t1   Absorption by the vegetation for the Black Background component.
;(16) %Abs_t3   Absorption by the vegetation for the coupled canopy-background term.
;(17) %Tran_t1  Transmission to the background level for the Black Background conponent.
;               It only accounts for the radiation collided by the vegetation.
;(18) %Tran_t2  Transmission to the background level for the Black Canopy conponent.
;      This is the direct transmission through the layer.
;(19) %Tran_t3  Transmission to the background level for the coupled canopy-background term.
;(20) %Rbgd1    Albedo accounting for the radiation having hit only once the background (whether collided
;      or not by the vegetation, whether upward or downward).


cmd = path2s + '\' + exe2s + ' ' + inputs + ' ' + oSettings
SPAWN, cmd, out2s, /NOSHELL, /HIDE
IF (out2s EQ '') THEN RETURN, 'Error'
input_val = FLTARR(8)


output_label = STRSPLIT(oSettings, '[%,"]', /EXTRACT, /REGEX)
output_label = output_label[1:N_ELEMENTS(output_label)-1]
output_val = FLTARR(N_ELEMENTS(output_label))

READS, out2s, input_val, output_val
PRINT, '<<<<<<< IN' 
PRINT, FORMAT='(10(A6,:,"   "))', input_label
PRINT, FORMAT='(10(F6.3,:,"   "))', input_val
PRINT, '>>>>>>> OUT'
PRINT, FORMAT='(10(A6,:,"   "))', output_label
PRINT, FORMAT='(10(F6.4,:,"   "))', output_val
RETURN, ''
END


