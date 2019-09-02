;yobs=[0.554083  ,0.614109 ,0.595095 ,0.708316 ,0.423142 ,0.700534 ,1.116522 ,0.670573 ,1.393847 ,0.786484 ,0.411496 ,0.699606 ,0.493235 ,0.610577 ,0.748035 ,0.448334 ,0.543613 ,0.560927 ,0.691257 ,0.444634 ,0.479252 ,0.631655 ,0.57179  ,0.613949 ,0.701851 ,0.574728 ,0.782445 ,0.388201]
;yobsOK=[0.554083  ,0.614109 ,0.595095 ,0.708316 ,0.623142 ,0.700534 ,0.8 ,0.770573 ,0.8 ,0.786484 ,0.86 ,0.699606 ,0.653235 ,0.610577 ,0.748035 ,0.558334 ,0.543613 ,0.560927 ,0.691257 ,0.444634 ,0.479252 ,0.631655 ,0.57179  ,0.613949 ,0.701851 ,0.574728 ,0.782445 ,0.388201]
;yfit=[0.588607 ,0.589126 ,0.589803 ,0.614341 ,0.630284 ,0.757966 ,0.773343 ,0.79169  ,0.790325 ,0.775118 ,0.77061  ,0.740143 ,0.732449 ,0.687923 ,0.678343 ,0.632414 ,0.624235 ,0.59107  ,0.586032 ,0.567708 ,0.565186 ,0.556518 ,0.555384 ,0.551584 ,0.551097 ,0.549487 ,0.549283 ,0.54861]


FUNCTION ModelGoodnessOfFit, yobs, yfit, nparam
;https://en.wikipedia.org/wiki/F-test#Regression_problems
; The F-test is a Wald test. 

;H0 null hypothesis that model 2 does not provide a significantly better fit than model 1

;model 1 is the mean os the observations
;model 2 is any model used to fit the data with nparam parameters

;RSS
RSS1 = TOTAL((yobs-MEAN(yobs))^2)
RSS2 = TOTAL((yobs-yfit)^2)

Fobs = ((RSS1-RSS2) / FLOAT(nparam-1)) /(RSS2/FLOAT(yobs.LENGTH-nparam)) 


;Degrees of freedom of F critic
df1 = nparam - 1
df2 = yobs.LENGTH - nparam

;Fcritic = F_CVF(0.05, df1, df2) ;signifcance level 0.05

P_value = 1.0-F_PDF(Fobs,df1, df2)  ;Probability that H0 was true and not rejected, it i slike the P value of a regression

;PRINT, Fobs, Fcritic, P
;h = PLOT(yobs, SYMBOL='o', LINESTYLE='')
;h = PLOT(yfit, COLOR='r', LINESTYLE='-', /OVERPLOT)
;IF (Fobs LE Fcritic) THEN PRINT, 'The model does not singnicantly perform better than the mean '


RETURN, P_value
END