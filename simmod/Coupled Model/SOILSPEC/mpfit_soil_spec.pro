FUNCTION MPFIT_SOIL_SPEC, r, p, wl_modis=wl_modis, r_modis=r_modis, indwl = indwl
@cb_various.comm  

;COMMON wlim, wLimParFixed, wlim_type, kc, bewm, et0, bewm_et0, wpar, rsrLR, cwl, wl, P1_sen_model

sim = soilspec(wl,p[0],p[1],p[2],p[3])
sim = resample2MODIS7b(wl, sim, rsrLR)
RETURN, sim[indwl]
END