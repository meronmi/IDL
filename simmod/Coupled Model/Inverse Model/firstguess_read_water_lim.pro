FUNCTION firstGuess_read_water_lim, ini_fn, igbp, SYEAR=sYear

;read the first guesses from the ini file
;Specific Leaf Area(m2 g-1), fixed or modified it will be the one sent to PROSAIL, 0.018 comes from Ardo et al, 2009, field description, S:\Actions\FOODSEC\projects\Biomass Sahel\Eddy_Data\Sudan_Demakeya\Docs from Ardö\Demokeya_Field_description_ver_1_small
SLA =  FLOAT(read_info('SLA', ini_fn))
;(kgDM/GJ_PAR = gDM MJ-1) source: 2.54 from MARSOP3-LOTII interim report; 2.23 gC m-2 MJ-1 according to Sjstrom 2009 and using a convertion 1 mol PAR = (1/4.56) MJ PAR (from Aber et al., 1996)    
eps_max =  FLOAT(read_info('eps_max_'+igbp, ini_fn))
;respiration coefficient (1/10), re=gamma*GPP             
gamma = FLOAT(read_info('gamma', ini_fn))
;parameters controlling the leaf lifespan in DegDays, c acts as a base life span
c = FLOAT(read_info('c', ini_fn))
d = FLOAT(read_info('d', ini_fn))
;coefficients a and b for partitioning into leaves, new partitioning hyperbolic function
a = FLOAT(read_info('a', ini_fn))     ;period (in ggd)
b = FLOAT(read_info('b', ini_fn))     ;curvature
;base temperature to comput degree days (dDD = MAX(Tair-Tb, 0)
tb = FLOAT(read_info('tb_'+igbp, ini_fn))
;bewm parameters
cap = FLOAT(read_info('cap', ini_fn))   ;precipitation cap 
half_life = FLOAT(read_info('half_life', ini_fn))    ;half life in degree days
bewm_opt = FLOAT(read_info('bewm_opt', ini_fn))    ;optimal back weight exponential mean for 100 degree days (approx 4 days)
;DOY at which the LAI is laiDOY0, it is used to approximate emergence of the crop
DOY0 = FLOAT(read_info('DOY0', ini_fn))
JD0 = DOY_YEAR2JD(DOY0, sYear)
laiDOY0 = FLOAT(read_info('laiDOY0', ini_fn))           ;LAI value at DOY0, emergence
PRINT, 'First Guess: ', LONG(JD0), laiDOY0, SLA, eps_max, gamma, c, d, a, b, tb 
RETURN, DOUBLE([JD0, laiDOY0, SLA, eps_max, gamma, c, d, a, b, tb, cap, half_life, bewm_opt])
END