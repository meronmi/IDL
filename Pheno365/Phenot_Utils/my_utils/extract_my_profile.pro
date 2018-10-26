PRO extract_my_profile
;sample and line in idl format (envi-1)
s=[1509, 2780];[3036, 2307, 3018, 2603]
l=[2222, 2738];[2319, 3267, 2700, 2821]
path = 'Q:\HoA\SPIRITS_HOA\MTA and bil files'
fnames = 'bil_'+['AVG','SD','MIN','MAX']+'_sc'
ns = 3586
nl = 3810
nb = 36

zmat = FLTARR(N_ELEMENTS(fnames)+1,NB)
zmat = zmat *!VALUES.F_NAN
zmat[0,*]=INDGEN(36)+1
FOR i=0, N_ELEMENTS(s)-1 DO BEGIN
  FOR f=0, N_ELEMENTS(fnames)-1 DO BEGIN
    zmat[f+1,*] = zprofile(path + '\' + fnames[f], ns, nl, nb, s[i], l[i])
  ENDFOR
  WRITE_CSV, path + '\s' + STRTRIM(s[i],2) + '_l'+STRTRIM(l[i],2)+'.csv', zmat, $
  HEADER = ['DEK','AVG','SD','MIN','MAX']
ENDFOR

END