Pro verhoef_ladf, a, b, mta
;Mic 3 July, I found this function on my laptop, I must have created it before..

		;***************************lidf a and b
	theta_deg=[10.,20.0,30.0,40.0,50.0,60.0,70.0,80.0,82.0,84.0,86.0,88.0,90.0]
	;theta_deg = [5.,15.,25.,35.,45.,55.,65.,75.,81.,83.,85.,87.,89.]
	n_angles=N_ELEMENTS(theta_deg)
	cumF=dblarr(n_angles)
	F=dblarr(n_angles)
	;ff=dblarr(n_angles)
	for i=0,n_angles-1 do begin
		theta=!dtor*theta_deg[i]
		dx=1.0	;initial values
		x=2.0*theta
		t=1.0e-6
		
		while (abs(dx) gt t) do begin
			y = double(a) * sin(x) + (double(b) * sin(2.0*x))/2.0
			dx = (y - x + 2.0 * theta)/2.0
			x = x + dx
		endwhile
		cumF[i] = 2.0 * (y + theta)/!dpi
	endfor
	for i=1,n_angles-1 do F[i]=cumF[i]-cumF[i-1]
	F[0]=cumF[0]
	
	;***************************ellipse
		t=findgen(90)+1.0
		ff=dblarr(90)
		F_ellips=dblarr(n_angles)
	  som=0.d+0
	  n=N_ELEMENTS(t)
    excent=(mta*!dtor/9.65d+0)^(-1.d+0/1.65d+0)-3.d+0
		;   initialization of 9 angle classes
    for i=0,n-1 do begin
        aex=excent
        ;theta=5.d+0+10.d+0*dfloat(i-1)
        theta=t[i]
        theta=theta*!dtor
        gd=aex+1.744d+0*(aex+1.182d+0)^(-0.733d+0)
				ff(i)=2*aex^3*sin(theta)/(gd*(cos(theta)^2+aex^2*sin(theta)^2)^2)
        som=som+ff(i)
    endfor
    for i=0,n-1 do begin
         ff(i)=ff(i)/som
    endfor
    F_ellips[0]=TOTAL(ff[0:9])
    F_ellips[1]=TOTAL(ff[10:19])
    F_ellips[2]=TOTAL(ff[20:29])
    F_ellips[3]=TOTAL(ff[30:39])
    F_ellips[4]=TOTAL(ff[40:49])
    F_ellips[5]=TOTAL(ff[50:59])
    F_ellips[6]=TOTAL(ff[60:69])
    F_ellips[7]=TOTAL(ff[70:79])
    F_ellips[8]=TOTAL(ff[80:81])
    F_ellips[9]=TOTAL(ff[82:83])
    F_ellips[10]=TOTAL(ff[84:85])
    F_ellips[11]=TOTAL(ff[86:87])
    F_ellips[12]=TOTAL(ff[88:89])
    
    
    
		plot, theta_deg, TOTAL(F, /CUMULATIVE), xtitle='Theta (deg)', ytitle='Cumulative Leaf Angle Distrib.'
		oplot, theta_deg, TOTAL(F_ellips, /CUMULATIVE), color = 3000
		print, theta_deg
		print, F
		print, F_ellips
		;oplot, theta_deg, F, psym=2, color=
End