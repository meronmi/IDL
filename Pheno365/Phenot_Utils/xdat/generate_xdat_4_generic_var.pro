;
;Function generate_xdat_4_generic_var, var
;;generates an xdat for a generic variable in IDL (to be used to construct
;;input files from synthetic data)
;;a=[0,  0.241921895599668,  0.469471562785891,  0.669130606358858,  0.829037572555042,  0.939692620785908,  0.994521895368273,  0.99026806874157,   0.927183854566787,  0.809016994374947,  0.642787609686539,  0.438371146789077,  0.207911690817759,  0,  0,  0.25,   0.433012701892219,  0.5,  0.433012701892219,  0.25,   6.1257422745431E-17,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0] 
;;c=[a,a,a,a,a,a,a,a]
;;
;julian1_f=float(floor(JULDAY(01, 01, 2000, 13, 0, 0)))
;;write_xdat parameters
;path_name='D:\Users\meronmi\Documents\IDL\MV Pheno\Data\Niger'  
;sensor='VGT'
;spat_res=1000.0
;site_code='test'
;site_name='test'
;cell_size=1
;ini_year=2000
;nyears=floor(N_elements(var)/36.0)
;nb=N_elements(var)
;fin_year=ini_year+nyears
;period=10
;site_lat=0.0
;site_lon=0.0
;lat1=site_lat
;lat2=site_lat
;lon1=site_lon
;lon2=site_lon
;nlines=N_elements(var)
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
;z=var
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