	subroutine FourSAIL
c
c	Improved and extended version of SAILH model that avoids numerical singularities
c	and works more efficiently if only few parameters change.
c
c	The common block "staticvar" is meant for internal use in order to keep
c	several variables static between successive calls of the subroutine.
c	For calculation of vertical flux profiles inside the canopy, the quantities 
c	m, sb, sf, ks, and rinf are needed by the calling routine, so in this case 
c	this common block must also be declared in the calling routine.
c
c	For applications to thermal IR simulations with distinct 
c	temperatures of leaves in the sun and in the shade, some extra
c	layer quantities (gammas) are computed and output in the common
c	block "thermal" 
c
c	Wout Verhoef 
c	NLR	
c	April/May 2003
c		
	implicit integer (i-n), double precision (a-h,o-z)
c
	real*8 lai,lai_old,litab(13),lidf(13),ksli,koli,ks,ko,m,
     +	    Jfunc1,Jfunc2,J1ks,J2ks,J1ko,J2ko
	logical delta_geom,delta_lidf,delta_leaf,delta_lai,delta_hot,
     +			delta_soil,flag(6),init_completed
c
      common /angles/pi,rd,tts,tto,psi
      common /sail_input/lai,a,b,q,rho,tau,rsoil
	common /staticvar/lidf,cts,cto,ctscto,dso,ddb,ddf,sdb,sdf,
     +		            dob,dof,sob,sof,m,att,sigb,rinf,ks,ko,
     +						sb,sf,vb,vf,w,sumint
      common /rfltrn/tss,too,tsstoo,rdd,tdd,rsd,tsd,rdo,tdo,rso,rsos,
     +				   rsod,rddt,rsdt,rdot,rsodt,rsost,rsot
	common /thermal/gammasdf,gammasdb,gammaso
      common /oldpar/a_old,b_old,lai_old,q_old,rho_old,tau_old,tts_old,
     +			      tto_old,psi_old,rsoil_old,init_completed
c
	data litab/5.,15.,25.,35.,45.,55.,65.,75.,81.,83.,85.,87.,89./
c
c	Raise all flags if we arrive here for the first time, lower them at other times
c
	do i=1,6
		flag(i)=.not.init_completed
	end do
c
	if (init_completed) then
c
c		Detect which inputs have changed (if it is not the first time)
c
		delta_lidf=(a.ne.a_old).or.(b.ne.b_old)
		delta_lai=(lai.ne.lai_old)
		delta_hot=(q.ne.q_old)
		delta_leaf=(rho.ne.rho_old).or.(tau.ne.tau_old)
		delta_geom=(tts.ne.tts_old).or.(tto.ne.tto_old).or.
     +				  (psi.ne.psi_old)
		delta_soil=(rsoil.ne.rsoil_old)
c
c		Raise the flags for the modules to be executed
c
		flag(1)=delta_geom
		flag(2)=delta_lidf
		flag(3)=delta_geom.or.delta_lidf
		flag(4)=flag(3).or.delta_leaf
		flag(5)=flag(4).or.delta_lai
		flag(6)=flag(3).or.delta_lai
c
	end if
c
c	Make sure that on next occasions init is regarded as being completed
c
	init_completed=.true.
c		
	if (flag(1)) then
c
c		Geometric quantities
c
		pi=atan(1.)*4.d0
		rd=pi/180.d0
c
		cts=cos(rd*tts)
		cto=cos(rd*tto)
		ctscto=cts*cto
		sts=sin(rd*tts)
		sto=sin(rd*tto)
		tants=sts/cts
		tanto=sto/cto
		cospsi=cos(rd*psi)
		dso=sqrt(tants*tants+tanto*tanto-2.*tants*tanto*cospsi)
c
	end if
c
	if (flag(2)) then
c
c		Generate leaf angle distribution from (a,b) parameters
c
		call dladgen(a,b,lidf)
c
	end if
c
	if (flag(3)) then
c
c		Calculate geometric factors associated with extinction and scattering 
c
c		Initialise sums
c
		ks=0
		ko=0
		bf=0
		sob=0
		sof=0
c
c		Weighted sums over LIDF
c      
		do ili=1,13
c
			ttl=litab(ili)
			ctl=cos(rd*ttl)
c
c			SAIL volume scattering phase function gives interception and portions to be 
c			multiplied by rho and tau
c
			call volscatt(tts,tto,psi,ttl,chi_s,chi_o,frho,ftau)
c
c			Extinction coefficients
c
			ksli=chi_s/cts
			koli=chi_o/cto
c
c			Area scattering coefficient fractions
c
			sobli=frho*pi/ctscto
			sofli=ftau*pi/ctscto
c
			bfli=ctl*ctl
c
			ks=ks+ksli*lidf(ili)
			ko=ko+koli*lidf(ili)
			bf=bf+bfli*lidf(ili)
			sob=sob+sobli*lidf(ili)
			sof=sof+sofli*lidf(ili)
c
		end do
c
c		Geometric factors to be used later with rho and tau
c
		sdb=0.5*(ks+bf)
		sdf=0.5*(ks-bf)
		dob=0.5*(ko+bf)
		dof=0.5*(ko-bf)
		ddb=0.5*(1.+bf)
		ddf=0.5*(1.-bf)
c
	end if
c
	if (flag(4)) then
c
c		Here rho and tau come in
c
		sigb=ddb*rho+ddf*tau
		sigf=ddf*rho+ddb*tau
		att=1.-sigf
		m=sqrt(att*att-sigb*sigb)
c
		sb=sdb*rho+sdf*tau
		sf=sdf*rho+sdb*tau
		vb=dob*rho+dof*tau
		vf=dof*rho+dob*tau
		w =sob*rho+sof*tau
c
	end if
c
	if (flag(5)) then	
c
c		Here the LAI comes in
c
c	   Outputs for the case LAI = 0
c
		if (lai.le.0) then
c
			tss=1.
			too=1.
			tsstoo=1.
			rdd=0.
			tdd=1.
			rsd=0.
			tsd=0.
			rdo=0.
			tdo=0.
			rso=0.
			rsos=0.
			rsod=0.
c
			rddt=rsoil
			rsdt=rsoil
			rdot=rsoil
			rsodt=0.
			rsost=rsoil
			rsot=rsoil
c
			return
c
		end if
c
c		Other cases (LAI > 0)
c
		e1=exp(-m*lai)
		e2=e1*e1
		rinf=(att-m)/sigb
		rinf2=rinf*rinf
		re=rinf*e1
		denom=1.-rinf2*e2
c
		J1ks=Jfunc1(ks,m,lai)
		J2ks=Jfunc2(ks,m,lai)
		J1ko=Jfunc1(ko,m,lai)
		J2ko=Jfunc2(ko,m,lai)
c
		Ps=(sf+sb*rinf)*J1ks
		Qs=(sf*rinf+sb)*J2ks
		Pv=(vf+vb*rinf)*J1ko
		Qv=(vf*rinf+vb)*J2ko
c
		tdd=(1.-rinf2)*e1/denom
		rdd=rinf*(1.-e2)/denom
		tsd=(Ps-re*Qs)/denom
		rsd=(Qs-re*Ps)/denom
		tdo=(Pv-re*Qv)/denom
		rdo=(Qv-re*Pv)/denom
c
c		Thermal "sd" quantities
c
		gammasdf=(1+rinf)*(J1ks-re*J2ks)/denom
		gammasdb=(1+rinf)*(-re*J1ks+J2ks)/denom
c
		tss=exp(-ks*lai)
		too=exp(-ko*lai)
c
		z=Jfunc2(ks,ko,lai)
		g1=(z-J1ks*too)/(ko+m)
		g2=(z-J1ko*tss)/(ks+m)
c
		Tv1=(vf*rinf+vb)*g1
		Tv2=(vf+vb*rinf)*g2
c		
		T1=Tv1*(sf+sb*rinf)
		T2=Tv2*(sf*rinf+sb)
		T3=(rdo*Qs+tdo*Ps)*rinf
c
c		Multiple scattering contribution to bidirectional canopy reflectance
c	
		rsod=(T1+T2-T3)/(1.-rinf2)
c
c		Thermal "sod" quantity
c
		T4=Tv1*(1+rinf)
		T5=Tv2*(1+rinf)
		T6=(rdo*J2ks+tdo*J1ks)*(1+rinf)*rinf
c
		gammasod=(T4+T5-T6)/(1-rinf2)
c
	end if
c
	if (flag(6)) then
c
c		Treatment of the hotspot-effect
c      
		alf=1e6
c
c		Apply correction 2/(K+k) suggested by F.-M. Bréon
c
		if (q.gt.0.) alf=(dso/q)*2./(ks+ko)

		if (alf.eq.0.) then
c
c			The pure hotspot
c
			tsstoo=tss
			sumint=(1-tss)/(ks*lai)
c
		else
c       
c			Outside the hotspot
c
			fhot=lai*sqrt(ko*ks)
c    
c			Integrate by exponential Simpson method in 20 steps
c			the steps are arranged according to equal partitioning
c			of the slope of the joint probability function
c
			x1=0.
			y1=0.
			f1=1.
			fint=(1.-exp(-alf))*.05
			sumint=0.
c
			do istep=1,20
c
				if (istep.lt.20) then 
    					x2=-log(1.-istep*fint)/alf
				else
					x2=1.
				endif
				y2=-(ko+ks)*lai*x2+fhot*(1.-exp(-alf*x2))/alf
				f2=exp(y2)
				sumint=sumint+(f2-f1)*(x2-x1)/(y2-y1)
				x1=x2
				y1=y2
				f1=f2
c
			end do
c	
			tsstoo=f1
c
		end if
c
	end if
c
c	Bidirectional reflectance
c
c	Single scattering contribution
c
      rsos=w*lai*sumint
	gammasos=ko*lai*sumint
c
c	Total canopy contribution
c
	rso=rsos+rsod
	gammaso=gammasos+gammasod
c
c	Interaction with the soil
c
	dn=1.-rsoil*rdd
c
	rddt=rdd+tdd*rsoil*tdd/dn
	rsdt=rsd+(tsd+tss)*rsoil*tdd/dn
	rdot=rdo+tdd*rsoil*(tdo+too)/dn
	rsodt=rsod+((tss+tsd)*tdo+(tsd+tss*rsoil*rdd)*too)*rsoil/dn
	rsost=rsos+tsstoo*rsoil
	rsot=rsost+rsodt
c
c	Before returning, save current parameters as old ones 
c
	a_old=a
	b_old=b
	lai_old=lai
	q_old=q
	rho_old=rho
	tau_old=tau
	tts_old=tts
	tto_old=tto
	psi_old=psi
	rsoil_old=rsoil
c
      return
	end


	real*8 function Jfunc1(k,l,t)
c
c	J1 function with avoidance of singularity problem
c	
	real*8 k,l,t,del
c
	del=(k-l)*t
	if (abs(del)>1e-3) then
		Jfunc1=(exp(-l*t)-exp(-k*t))/(k-l)
	else
		Jfunc1=0.5*t*(exp(-k*t)+exp(-l*t))*(1.-del*del/12.)
	end if	
c
	return
	end


	real*8 function Jfunc2(k,l,t)
c
c	J2 function
c
	real*8 k,l,t
c
	Jfunc2=(1.-exp(-(k+l)*t))/(k+l)
c
	return
	end