; PROCEDURE COMMENTS +
; NAME: linspace
; AUTHOR: Modified from original IDL code by Eben N. Broadbent 
;  Dept. of Biology, Stanford University.
;
;  Original author:
;  Author: wd (Wolfgang.Dobler@ncl.ac.uk)
;  Date:   21-Jun-2001
;  Version: 0.35 (CVS: $Revision: 1.3 $)
;  http://www.capca.ucalgary.ca/~wdobler/doc/idl/index.html
;
; CONTACT INFO: ebennb@gmail.com
; DESCRIPTION:
; CALLING SEQUENCE:
;       output = linspace(x1, x2, n)
; INPUTS: x1, x2, n
; OUTPUTS: output
; OPTIONAL OUTPUTS:
; OPTIONAL INPUT KEYWORD(S):
; NOTES:
; METHOD:
; EXAMPLE:
; MODIFICATION HISTORY:
; CODING NOTES:
;-

function linspace, x1, x2, n
 list = x1+(findgen(n))*(x2-x1)/(n-1.0)
 return, list
end
