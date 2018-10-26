FUNCTION firstGuess_define_water_lim, conf, SYEAR=sYear
;Function used to keep track of the different parameterizations,
;given by conf


SLA = 0.018              ; Specific Leaf Area(m2 g-1), fixed or modified it will be the one sent to PROSAIL
                         ;0.018 comes from Ardo et al, 2009, field description, S:\Actions\FOODSEC\projects\Biomass Sahel\Eddy_Data\Sudan_Demakeya\Docs from Ardö\Demokeya_Field_description_ver_1_small

eps_max = 2.23             ;(kgDM/GJ_PAR = gDM MJ-1) source: 2.54 from MARSOP3-LOTII interim report; 2.23 gC m-2 MJ-1 according to Sjstrom 2009 and using a convertion 1 mol PAR = (1/4.56) MJ PAR (from Aber et al., 1996)
gamma = 0.0                ;respiration coefficient (1/10), re=gamma*GPP
;parameters controlling the leaf lifespan in DegDays, c acts as a base life span
c = 1500                ;J = c + d * GDD, as in Maas, 1993
d = 1.0                 ;d controls the lifespan (j) a function of the degree days at emergence, e.g. if d if GT 0 it makes the leaves emerging later to live longer
;coefficients a and b for partitioning into leaves (as in Maas, 1993), spring wheat
;a = 0.02                ;(P1 = Max(1-a*EXP(b*GDD), 0) as in Maas, 1993)
;b = 0.0023              ;0.002
a = 1500                ;new partitioning hyperbolic function, period (in ggd)
b = 1.2                 ;curvature
tb = 0              ;base temperature to comput degree days (dDD = MAX(Tair-Tb, 0)
cap = 100.0         ;precipitation cap 
half_life = 180;150     ;half life in degree days
;bewm_opt = 2;        ;optimal back weight exponential mean ;2 was with the old wighting scheme (one day avg)
bewm_opt = 8;         ;optimal back weight exponential mean for 100 degree days (approx 4 days)

CASE conf OF
  1: BEGIN
    ;DOY at which the LAI is laiDOY0, it is used to approximate emergence of the crop
    DOY0 = 175 & JD0 = DOY_YEAR2JD(DOY0, sYear) ;176 è giusto
    laiDOY0 = 0.02           ;LAI value at DOY0, emergence
    SLA = 0.025
  END
  2: BEGIN
    ;DOY at which the LAI is laiDOY0, it is used to approximate emergence of the crop
    DOY0 = 175 & JD0 = DOY_YEAR2JD(DOY0, sYear) ;176 è giusto
    ;DOY0 = 160 & JD0 = DOY_YEAR2JD(DOY0, sYear) ;176 è giusto
    laiDOY0 = 0.02           ;LAI value at DOY0, emergence
    a = 2000
    half_life = 90;210
  END
  ELSE: STOP
ENDCASE
PRINT, 'First Guess: ', LONG(JD0), laiDOY0, SLA, eps_max, gamma, c, d, a, b, tb 
RETURN, DOUBLE([JD0, laiDOY0, SLA, eps_max, gamma, c, d, a, b, tb, cap, half_life, bewm_opt])
END