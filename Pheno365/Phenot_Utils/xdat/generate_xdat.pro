;;print, generate_xdat(1074, 550) ;high fapar
;;print, generate_xdat(789, 348) ;low fapar
;;print, generate_xdat(1194, 325) ;low fapar
;;print, generate_xdat(650, 493) ;medium fapar
;
;
;Function generate_xdat, c, r
;;c and r are sample and  line
;;input image
;;niger
;;file='X:\Niger\input\bil_niger_a_fapar_396d_sc_clean'
;;somalia
;path = 'X:\IGAD\VGT data\bil'
;filein = 'bil_IGAD_VGTa_98-10_11-14_sc_cl'; 'bil_fapar10by10clean2chen2'
;file=path+'\'+filein
;ns =3586; 2018;                   ; number of samples
;nl =3810; 1038;                   ; number of lines
;nb = 473; 396                     ;number of bands  
;;first dekad (image cannot have missing band/dekad)
;julian1_f=float(floor(JULDAY(04, 01, 1998, 13, 0, 0))) ;first dekad is 9810
;;julian1_f=float(floor(JULDAY(01, 01, 2000, 13, 0, 0)))
;;write_xdat parameters
;path_name='D:\Users\meronmi\Documents\IDL\MV Pheno\Data\Niger'
;ini_year=1998;2000
;fin_year=2011;2010  
;sensor='VGT'
;spat_res=1000.0
;site_code='R'+strtrim(r,2)+'C'+strtrim(c,2)
;site_name='r'+strtrim(r,2)+'_c'+strtrim(c,2)
;cell_size=1
;
;period=10
;site_lat=float(r)
;site_lon=float(c)
;lat1=site_lat
;lat2=site_lat
;lon1=site_lon
;lon2=site_lon
;nlines=nb
;ncols=11
;prod_fields=['fapar','sd_fapar']
;ignore_below=[0.0,0.0]
;ignore_above=[1.0,1.0]
;miss_code=[-999, -999]
;prod_desc=['VITO_cyclope','n.a.']
;date_data=SYSTIME()
;
;res=compute_j1_j2(julian1_f, nb)
;julian1=res.julian1
;julian2=res.julian2
;
;copyright=''
;
;z=zprofile(file, ns, nl, nb, c, r)
;ind_nan=where(finite(z) ne 1, count_nan)
;if count_nan ne 0 then z[ind_nan]=miss_code[0]
;products=fltarr(nb,ncols-9)
;products[*,0]=z
;products[*,1]=-999
;
;rc = write_xdat (path_name, sensor, spat_res, site_code, site_name, $
;   cell_size, ini_year, fin_year, period, site_lat, site_lon, lat1, lat2, $
;   lon1, lon2, nlines, ncols, prod_fields, ignore_below, ignore_above, $
;   miss_code, prod_desc, date_data, julian1, julian2, products, copyright)
;   
;return, 1
;End