; PROCEDURE COMMENTS +
; NAME: Jfunc2
; AUTHOR: Translated from Matlab to IDL by Eben N. Broadbent 
;  Dept. of Biology, Stanford University.
;  
; Original Matlab code written by: Jean-Baptiste Féret (feret@ipgp.fr)
; Institut de Physique du Globe de Paris, Space and Planetary Geophysics
; during October 2009 based on a version of PROSAIL provided by
; Wout Verhoef, NLR on April/May 2003.
;  
; CONTACT INFO: ebennb@gmail.com
; DESCRIPTION:
; CALLING SEQUENCE:
;       output = Jfunc2(k,l,t)
; INPUTS:
; OUTPUTS:
; OPTIONAL OUTPUTS:
; OPTIONAL INPUT KEYWORD(S):
; NOTES:
; METHOD:
; EXAMPLE:
; MODIFICATION HISTORY:
;       Downloaded from "http://teledetection.ipgp.jussieu.fr/prosail/" on 09/10/2010
;       Translated from matlab to IDL by: Eben N. Broadbent on '09/27/2010'
; CODING NOTES:
; Same as Matlab (09/27/2010)
;-

function Jfunc2,k,l,t
 ; J2 function
 k = float(k)
 l = float(l)
 t = float(t)

 Jout=(1.0-exp(-(k+l)*t))/(k+l);
 return, Jout
end