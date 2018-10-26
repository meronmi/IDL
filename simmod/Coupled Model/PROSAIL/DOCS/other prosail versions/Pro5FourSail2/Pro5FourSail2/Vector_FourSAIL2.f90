           !!          |in: 
           !!          |Prospect, green leaf
SUBROUTINE VecFourSAIL2(N_g,Cab_g,Car_g,Cbrown_g,Cw_g,Cm_g, &
      !!        |in: 
      !!        |Prospect, brown green             |
                N_b,Cab_b,Car_b,Cbrown_b,Cw_b,Cm_b, &
      !!        |in: 
      !!        |4SAIL2, structure of the canopy
                lai, fb, diss, mli, hot, Cv, zeta, &    !LIDFa, LIDFb replaced by mli
      !!        |in: 
      !!        |4SAIL2, soil reflectance (two columns array) and coefficient for mixing them
                rsoil, psoil, &
      !!        |in: 
      !!        |4SAIL2, sun-view gemometry and diffuse fraction
                tts, tto, psi, &
      !!        |out: 
      !!        |4SAIL2
                rsot, rdot, rsdt, rddt, alfast, alfadt)
    
      !M+
	USE ANGLE
	USE datadim
	USE VECTOR_4SAIL2
	USE output_PROSPECT
	USE staticvar
	USE oldpar
      !M-
	implicit integer (i-n), real (a-h,o-z)
      !M+
	!!!!!!!!		INPUT / OUTPUT		!!!!!!!!
!
   
!      REAL(KIND=8),INTENT(in) :: lai,lidfa,lidfb,hot,fb,diss,Cv,zeta,tto,tts,psi
!      REAL(KIND=8),INTENT(out):: rsot(nw),rdot(nw),rsdt(nw),rddt(nw),alfast(nw),alfadt(nw)
      
    REAL(KIND=8),INTENT(in) :: Cab_g,Car_g,Cbrown_g,Cw_g,Cm_g,N_g
    REAL(KIND=8),INTENT(in) :: Cab_b,Car_b,Cbrown_b,Cw_b,Cm_b,N_b
    REAL(KIND=8),INTENT(in) :: lai, fb, diss, mli   !LIDFa, LIDFb replaced by mli
    REAL(KIND=8),INTENT(in) :: hot, Cv, zeta
    REAL(KIND=8),INTENT(in) :: rsoil(2,nw), psoil
    REAL(KIND=8),INTENT(in) :: tts, tto, psi
    !!        |out: 
    REAL(KIND=8),INTENT(out) :: rsot(nw),rdot(nw),rsdt(nw)
    REAL(KIND=8),INTENT(out) :: rddt(nw),alfast(nw),alfadt(nw)      

      !!!!!!!!	END INPUT / OUTPUT		!!!!!!!!
!
      !REAL(KIND=8) :: rg(nw),tg(nw),rb(nw),tb(nw)
    REAL(KIND=8) :: rsosoil(nw),rdosoil(nw),rsdsoil(nw),rddsoil(nw)
    REAL(KIND=8) :: ttl,chi_s,chi_o,frho,ftau
    REAL(KIND=8) :: lai1,lai2,lidf_ellipse(13) !,li(13),lidf(13),ks,ko !,m,m2, lai_old
      
     !REAL(KIND=8) :: LIDFa_old,LIDFb_old
    REAL(KIND=8) :: ksli,koli    !,J1ks,J1ko,J2ks,J2ko,J3,Jfunc1,Jfunc2,Jfunc3,
	REAL(KIND=8) :: alf,ca,fint,x1,x2,y1,y2,f1,f2 !s1,s2,
     !M- 
	 
!
	logical delta_geom,delta_lidf,delta_fb,delta_lai,delta_hot,&
	delta_leaf_g,delta_leaf_b,delta_clump,delta_psoil,delta_diss
	logical do_scatter,do_hotspot
!
!	commom blocks FourSAIL2	
!	common /staticvar/cts,cto,ctscto,lidf,ks,ko,dso,&
!	ddb,ddf,sdb,sdf,dob,dof,sob,sof,&
!	Cs,Co,Fcd,Fcs,Fod,Fos,Fcdc,s1,s2
!	common /oldpar/LIDFa_old,LIDFb_old,lai_old,hot_old,fb_old,&
!	tts_old,tto_old,psi_old,Cv_old,zeta_old,&
!	N_g_old,Cab_g_old,Car_g_old,Cbrown_g_old,Cw_g_old,Cm_g_old,&
!	N_b_old,Cab_b_old,Car_b_old,Cbrown_b_old,Cw_b_old,Cm_b_old,psoil_old
  
	
	
!	Detect which parameters have changed
!
	delta_geom=(tts.ne.tts_old).or.(tto.ne.tto_old).or.(psi.ne.psi_old) !geometry:      tts,tto,psi
	delta_lidf=(mli.ne.mli_old)                                         !lidf:          now mli LIDfa,Lidfb (LIDFa.ne.LIDFa_old).or.(LIDFb.ne.LIDFb_old)
	delta_hot=(hot.ne.hot_old)                                          !hot spot:      hot
	delta_fb=(fb.ne.fb_old)                                             !fraction b.:   fb
	delta_diss=(diss.ne.diss_old)
	delta_lai=(lai.ne.lai_old)                                          !lai:           lai
	delta_clump=(Cv.ne.Cv_old).or.(zeta.ne.zeta_old)                    !clumping:      Cv,zeta
	!added
	delta_leaf_g=((N_g.ne.N_g_old).or.(Cab_g.ne.Cab_g_old).or.(Car_g.ne.Car_g_old).or.&    !Propsect
	           (Cbrown_g.ne.Cbrown_g_old).or.(Cw_g.ne.Cw_g_old).or.(Cm_g.ne.Cm_g_old))
	delta_leaf_b=((N_b.ne.N_b_old).or.(Cab_b.ne.Cab_b_old).or.(Car_b.ne.Car_b_old).or.&
	           (Cbrown_b.ne.Cbrown_b_old).or.(Cw_b.ne.Cw_b_old).or.(Cm_b.ne.Cm_b_old))
	delta_psoil=(psoil.ne.psoil_old)                                    !soil misture coeff
!
	do_scatter=(delta_geom.or.delta_lidf.or.(ifirst.eq.1))
	do_hotspot=(delta_geom.or.delta_hot.or.delta_lidf.or.delta_fb.or.delta_lai.or.(ifirst.eq.1))
!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!											!!
    !!	Soil Reflectance Properties	!!											!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!											!!
!   ---------------------------------------------------------------------------------------------
    ! rsoil1 = dry soil, rsoil2 = wet soil		
    IF (delta_psoil.or.(ifirst.eq.1)) THEN
        rsosoil=rsoil(1,:)*psoil+rsoil(2,:)*(1-psoil)   !PROPER MIX of the two soils 
        rdosoil=rsosoil                                 !Neglect soil BRDF
        rsdsoil=rsosoil
        rddsoil=rsosoil
    ENDIF
!   ______________________________________________________________________________________________
    
    !	Immediate return for lai = 0 
    if (lai.le.0.) then
		rdot=rdosoil
		rsot=rsosoil
		rsdt=rsdsoil
		rddt=rddsoil
		alfast=0.
		alfadt=0.
		tooc=1.
		return	
	end if

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!											!!
    !!	Leaf Ref-Trans Properties	!!											!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
!   ---------------------------------------------------------------------------------------------
	IF (delta_leaf_g.or.(ifirst.eq.1)) THEN
        CALL prospect_5B(N_g,Cab_g,Car_g,Cbrown_g,Cw_g,Cm_g,LRT_g)
        rg	=	LRT_g(:,2)														!!
	    tg	=	LRT_g(:,3)	
	ENDIF
!   ______________________________________________________________________________________________
	
!   ---------------------------------------------------------------------------------------------
	IF (delta_leaf_b.or.(ifirst.eq.1)) THEN
	    CALL prospect_5B(N_b,Cab_b,Car_b,Cbrown_b,Cw_b,Cm_b,LRT_b)
        rb	=	LRT_b(:,2)														!!
	    tb	=	LRT_b(:,3)	
    ENDIF
!   ______________________________________________________________________________________________
    
	!DEBUG M
!    open (2, file='out_prospect.txt')	
!	DO i=1,nw
!		write (2,100) i+399, rg(i),tg(i),rb(i),tb(i)
!	ENDDO
!	100 FORMAT(i8,2000f15.8)

!   ---------------------------------------------------------------------------------------------
    !When geometry (tts,tto,psi) or clumping (Cv,zeta) are changed	
	IF (delta_geom.or.delta_clump.or.(ifirst.eq.1)) THEN
    !		Angular factors -> f(tts,tto,psi) 					
		cts=cos(rd*tts)
		cto=cos(rd*tto)
		ctscto=cts*cto
		tants=tan(rd*tts)
		tanto=tan(rd*tto)
		cspsi=cos(rd*psi)
		dso=sqrt(tants*tants+tanto*tanto-2.*tants*tanto*cspsi)
    !		Clumping effects -> f(tts,tto,psi,Cv,zeta)
		Cs=1
		Co=1
		If (Cv.lt.1) then
			Cs=1-(1-Cv)**(1./cts)
			Co=1-(1-Cv)**(1./cto)
		End if
		Overlap=0
		If (zeta.gt.0) Overlap=min(Cs*(1-Co),Co*(1-Cs))*exp(-dso/zeta)
		Fcd=Cs*Co+Overlap                   !define various scene factors (Verhoef and Bach, 2007, p. 171)
		Fcs=(1-Cs)*Co-Overlap
		Fod=Cs*(1-Co)-Overlap
		Fos=(1-Cs)*(1-Co)+Overlap
		Fcdc= 1.-(1-Fcd)**(.5/cts+.5/cto)
	ENDIF
!   ______________________________________________________________________________________________

!   ---------------------------------------------------------------------------------------------    
    !When the fraction of brown leaves (fb), the dissociation factor (diss) or
    !one of the two leaf types (g or b) are changed
    IF (delta_fb.or.delta_diss.or.delta_leaf_g.or.delta_leaf_b.or.(ifirst.eq.1)) THEN
	    fbu=fb              !define an fb that is actually used: fbu, so that the input is not modified!
    	if (fb.eq.0) then
		    fbu=0.5
		    rb=rg
		    tb=tg
	    end if
	    if (fb.eq.1) then
		    fbu=0.5
		    rg=rb
		    tg=tb
	    end if
        s=(1.-diss)*fbu*(1.-fbu)
        rho1=((1-fbu-s)*rg+s*rb)/(1-fbu)    !leaf reflectance and transmittance of layer1 (upper)
        tau1=((1-fbu-s)*tg+s*tb)/(1-fbu)    
        rho2=(s*rg+(fbu-s)*rb)/fbu          !leaf reflectance and transmittance of layer2 (lower)
        tau2=(s*tg+(fbu-s)*tb)/fbu
    ENDIF
!   ______________________________________________________________________________________________

!   ---------------------------------------------------------------------------------------------    
!	When lidf parameters are changed (LIDFa,LIDFb), enerate LIDF from (a,b) parameters
	!if (delta_lidf.or.(ifirst.eq.1)) call ladgen(LIDFa,LIDFb,lidf)
	!Substituted with Campbell 1990 ellipsoidal function controlled by lmi (leaf mean inclination angle)
	if (delta_lidf.or.(ifirst.eq.1)) CALL ellipse(mli,lidf)
!   ______________________________________________________________________________________________

!   ---------------------------------------------------------------------------------------------
!   When geometry (tts,tto,psi) or lidf parameters are changed, compute scatter
!   scatter = f(tts,tto,psi,lidf), no effect of rho or tau 1 and 2
	if (do_scatter) then
!		Calculate geometric factors associated with extinction and scattering 
!		Initialise sums
		ks=0
		ko=0
		bf=0
		sob=0
		sof=0
!		Weighted sums over LIDF  
		do ili=1,13
			ttl=li(ili)
			ctl=cos(rd*ttl)
!			SAIL volscatt function gives interception coefficients 
!			and two portions of the volume scattering phase function to be 
!			multiplied by rho and tau, respectively
			call volscatt(tts,tto,psi,ttl,chi_s,chi_o,frho,ftau)
!			Extinction coefficients
			ksli=chi_s/cts
			koli=chi_o/cto
!			Area scattering coefficient fractions
			sobli=frho*pi/ctscto
			sofli=ftau*pi/ctscto
			bfli=ctl*ctl
			ks=ks+ksli*lidf(ili)
			ko=ko+koli*lidf(ili)
			bf=bf+bfli*lidf(ili)
			sob=sob+sobli*lidf(ili)
			sof=sof+sofli*lidf(ili)
		end do
!		Geometric factors to be used later in combination with rho and tau
		sdb=0.5*(ks+bf)
		sdf=0.5*(ks-bf)
		dob=0.5*(ko+bf)
		dof=0.5*(ko-bf)
		ddb=0.5*(1.+bf)
		ddf=0.5*(1.-bf)
	end if
!   ______________________________________________________________________________________________
!
!     LAIs in two layers
	lai1=(1-fbu)*lai
	lai2=fbu*lai

!   ---------------------------------------------------------------------------------------------
!   When geometry or hot spot parameter or lidf or fb or LAI are changed, comput hot spot
	if (do_hotspot) then
!	    Treatment of the hot spot effect for 2 layers	
	    tss=exp(-ks*lai)        !note: ks is a static var, the last one is used
	    ck=exp(-ks*lai1)
		alf=1D6
		if (hot.gt.0.) alf=(dso/hot)*2./(ks+ko)
        if (alf.gt.200.) alf=200.     !inserted H. Bach 1/3/04
		if (alf.eq.0.) then
!			The pure hotspot
		    tsstoo=tss
			s1=(1-ck)/(ks*lai)
			s2=(ck-tss)/(ks*lai)
		else       
!			Outside the hotspot
			fhot=lai*sqrt(ko*ks)
!			Integrate 2 layers by exponential simpson method in 20 steps
!			the steps are arranged according to equal partitioning
!			of the derivative of the joint probability function
			x1=0.
			y1=0.
			f1=1.
			ca=exp(alf*(fbu-1.))
			fint=(1.-ca)*.05
			s1=0.		
			do istep=1,20
				if (istep.lt.20) then 
    			    x2=-log(1.-istep*fint)/alf
				else
					x2=1.-fbu
				end if
				y2=-(ko+ks)*lai*x2+fhot*(1.-exp(-alf*x2))/alf
				f2=exp(y2)
				s1=s1+(f2-f1)*(x2-x1)/(y2-y1)
				x1=x2
				y1=y2
				f1=f2
			end do
			fint=(ca-exp(-alf))*.05
			s2=0.
			do istep=1,20
				if (istep.lt.20) then
					x2=-log(ca-istep*fint)/alf
				else
					x2=1.
				end if
				y2=-(ko+ks)*lai*x2+fhot*(1.-exp(-alf*x2))/alf
				f2=exp(y2)
				s2=s2+(f2-f1)*(x2-x1)/(y2-y1)
 				x1=x2
				y1=y2
				f1=f2
			end do
			tsstoo=f1
		end if
	end if	
!   ______________________________________________________________________________________________

!     Calculate reflectances and transmittances
!
!     Bottom layer
	tss=exp(-ks*lai2)
	too=exp(-ko*lai2)
	sb=sdb*rho2+sdf*tau2                !note: here rho and tau come in
	sf=sdf*rho2+sdb*tau2
	vb=dob*rho2+dof*tau2
	vf=dof*rho2+dob*tau2
	w2=sob*rho2+sof*tau2
    sigb=ddb*rho2+ddf*tau2
	sigf=ddf*rho2+ddb*tau2
    att=1.-sigf
	m2=(att+sigb)*(att-sigb)
	
	WHERE (m2.lt.0) m2=0 !M+ old: !if (m2.lt.0) m2=0
	m=sqrt(m2)
!   M+ call the relevant function for all the casis (m>0.01 or not)
    CALL Jfunc1(ks,m,lai2,J1ks)         !for: m>0.01
	CALL Jfunc2(ks,m,lai2,J2ks)         !for: m>0.01
	CALL Jfunc1(ko,m,lai2,J1ko)         !for: m>0.01
	CALL Jfunc2(ko,m,lai2,J2ko)         !for: m>0.01
	CALL Jfunc2bis(ks,ko,lai2,z)        !for: m>0.01	
	CALL Jfunc3(m,lai2,J3)              !for: m<0.01
    
    WHERE (m.gt.0.01)   !M+ old: !if (m.gt.0.01) then
!		Normal case
		e1=exp(-m*lai2)
		e2=e1*e1
		rinf=(att-m)/sigb
		rinf2=rinf*rinf
		re=rinf*e1
		denom=1.-rinf2*e2
!		CALL Jfunc1(ks,m,lai2,J1ks) !		CALL Jfunc2(ks,m,lai2,J2ks)!		CALL Jfunc1(ko,m,lai2,J1ko)!		CALL Jfunc2(ko,m,lai2,J2ko)
		Ps=(sf+sb*rinf)*J1ks
		Qs=(sf*rinf+sb)*J2ks
		Pv=(vf+vb*rinf)*J1ko
		Qv=(vf*rinf+vb)*J2ko
		tdd=(1.-rinf2)*e1/denom         !
		rdd=rinf*(1.-e2)/denom
		tsd=(Ps-re*Qs)/denom
		rsd=(Qs-re*Ps)/denom
		tdo=(Pv-re*Qv)/denom
		rdo=(Qv-re*Pv)/denom
!		CALL Jfunc2(ks,ko,lai2,z)
		g1=(z-J1ks*too)/(ko+m)
		g2=(z-J1ko*tss)/(ks+m)
		Tv1=(vf*rinf+vb)*g1             !
		Tv2=(vf+vb*rinf)*g2		
		T1=Tv1*(sf+sb*rinf)             !
		T2=Tv2*(sf*rinf+sb)
		T3=(rdo*Qs+tdo*Ps)*rinf
!		Multiple scattering contribution to bidirectional canopy reflectance	
		rsod=(T1+T2-T3)/(1.-rinf2)
	ELSEWHERE !else
!		Near or complete conservative scattering
!		CALL Jfunc3(m,lai2,J3)
		amsig=att-sigb
		apsig=att+sigb
		rtp=(1-amsig*J3)/(1+amsig*J3)
		rtm=(-1+apsig*J3)/(1+apsig*J3)
		rdd=.5*(rtp+rtm)
		tdd=.5*(rtp-rtm)
		dns=ks*ks-m*m                       !
		dno=ko*ko-m*m
		cks=(sb*(ks-att)-sf*sigb)/dns
		cko=(vb*(ko-att)-vf*sigb)/dno
		dks=(-sf*(ks+att)-sb*sigb)/dns
		dko=(-vf*(ko+att)-vb*sigb)/dno
		ho=(sf*cko+sb*dko)/(ko+ks)				
		rsd=cks*(1-tss*tdd)-dks*rdd         !
		rdo=cko*(1-too*tdd)-dko*rdd
		tsd=dks*(tss-tdd)-cks*tss*rdd
		tdo=dko*(too-tdd)-cko*too*rdd
!		Multiple scattering contribution to bidirectional canopy reflectance
		rsod=ho*(1-tss*too)-cko*tsd*too-dko*rsd
	ENDWHERE !end if

!     Set background properties equal to those of the bottom layer on a black soil
    rddb=rdd
    rsdb=rsd
    rdob=rdo
    rsodb=rsod
	tddb=tdd
	tsdb=tsd
	tdob=tdo
	toob=too
	tssb=tss
!     Top layer
	tss=exp(-ks*lai1)
	too=exp(-ko*lai1)
	sb=sdb*rho1+sdf*tau1
	sf=sdf*rho1+sdb*tau1
	vb=dob*rho1+dof*tau1
	vf=dof*rho1+dob*tau1
	w1=sob*rho1+sof*tau1
    sigb=ddb*rho1+ddf*tau1
	sigf=ddf*rho1+ddb*tau1
    att=1.-sigf
	m2=(att+sigb)*(att-sigb)
	WHERE (m2.lt.0) m2=0        !M+ old: 	!if (m2.lt.0) m2=0
	m=sqrt(m2)
!   M+ call the relevant function for all the casis (m>0.01 or not)	
    CALL Jfunc1(ks,m,lai1,J1ks)         !for: m>0.01
	CALL Jfunc2(ks,m,lai1,J2ks)         !for: m>0.01
	CALL Jfunc1(ko,m,lai1,J1ko)         !for: m>0.01
	CALL Jfunc2(ko,m,lai1,J2ko)         !for: m>0.01
	CALL Jfunc2bis(ks,ko,lai1,z)        !for: m>0.01
	CALL Jfunc3(m,lai1,J3)              !for: m<0.01
	WHERE (m.gt.0.01)    !if (m.gt.0.01) then
!		Normal case
		e1=exp(-m*lai1)
		e2=e1*e1
		rinf=(att-m)/sigb
		rinf2=rinf*rinf
		re=rinf*e1
		denom=1.-rinf2*e2
!		CALL Jfunc1(ks,m,lai1,J1ks)!		CALL Jfunc2(ks,m,lai1,J2ks)!		CALL Jfunc1(ko,m,lai1,J1ko)!		CALL Jfunc2(ko,m,lai1,J2ko)
		Ps=(sf+sb*rinf)*J1ks
		Qs=(sf*rinf+sb)*J2ks
		Pv=(vf+vb*rinf)*J1ko
		Qv=(vf*rinf+vb)*J2ko
		tdd=(1.-rinf2)*e1/denom
		rdd=rinf*(1.-e2)/denom
		tsd=(Ps-re*Qs)/denom
		rsd=(Qs-re*Ps)/denom
		tdo=(Pv-re*Qv)/denom
		rdo=(Qv-re*Pv)/denom
!		CALL Jfunc2(ks,ko,lai1,z)
		g1=(z-J1ks*too)/(ko+m)
		g2=(z-J1ko*tss)/(ks+m)
		Tv1=(vf*rinf+vb)*g1
		Tv2=(vf+vb*rinf)*g2
		T1=Tv1*(sf+sb*rinf)
		T2=Tv2*(sf*rinf+sb)
		T3=(rdo*Qs+tdo*Ps)*rinf
!		Multiple scattering contribution to bidirectional canopy reflectance
		rsod=(T1+T2-T3)/(1.-rinf2)
	ELSEWHERE !else
!		Near or complete conservative scattering
!		CALL Jfunc3(m,lai1,J3)
		amsig=att-sigb
		apsig=att+sigb
		rtp=(1-amsig*J3)/(1+amsig*J3)
		rtm=(-1+apsig*J3)/(1+apsig*J3)
		rdd=.5*(rtp+rtm)
		tdd=.5*(rtp-rtm)
		dns=ks*ks-m*m
		dno=ko*ko-m*m
		cks=(sb*(ks-att)-sf*sigb)/dns
		cko=(vb*(ko-att)-vf*sigb)/dno
		dks=(-sf*(ks+att)-sb*sigb)/dns
		dko=(-vf*(ko+att)-vb*sigb)/dno
		ho=(sf*cko+sb*dko)/(ko+ks)				
		rsd=cks*(1-tss*tdd)-dks*rdd
		rdo=cko*(1-too*tdd)-dko*rdd
		tsd=dks*(tss-tdd)-cks*tss*rdd
		tdo=dko*(too-tdd)-cko*too*rdd
!		Multiple scattering contribution to bidirectional canopy reflectance
		rsod=ho*(1-tss*too)-cko*tsd*too-dko*rsd
	ENDWHERE !end if

!     Combine with bottom layer reflectances and transmittances (adding method)
!
    rn=1.-rdd*rddb
    tup=(tss*rsdb+tsd*rddb)/rn
    tdn=(tsd+tss*rsdb*rdd)/rn
    rsdt=rsd+tup*tdd
    rdot=rdo+tdd*(rddb*tdo+rdob*too)/rn
    rsodt=rsod+(tss*rsodb+tdn*rdob)*too+tup*tdo
	rsost=(w1*s1+w2*s2)*lai	    
    rsot=rsost+rsodt
!	Diffuse reflectances at the top and the bottom are now different 
	rddt_t=rdd+tdd*rddb*tdd/rn
	rddt_b=rddb+tddb*rdd*tddb/rn
!	Transmittances of the combined canopy layers
	tsst=tss*tssb
	toot=too*toob
	tsdt=tss*tsdb+tdn*tddb
	tdot=tdob*too+tddb*(tdo+rdd*rdob*too)/rn
	tddt=tdd*tddb/rn
!	Apply clumping effects to vegetation layer
	rddcb=Cv*rddt_b
	rddct=Cv*rddt_t
	tddc=1-Cv+Cv*tddt
	rsdc=Cs*rsdt
	tsdc=Cs*tsdt
	rdoc=Co*rdot
	tdoc=Co*tdot
	tssc=1-Cs+Cs*tsst
	tooc=1-Co+Co*toot
!	New weight function Fcdc for crown contribution (W. Verhoef, 22-05-08)	
	rsoc=Fcdc*rsot
	tssooc=Fcd*tsstoo+Fcs*toot+Fod*tsst+Fos

!	Canopy absorptance for black background (W. Verhoef, 02-03-04)
	alfas=1.-tssc-tsdc-rsdc
	alfad=1.-tddc-rddct

!	Add the soil background 
	rn=1-rddcb*rddsoil
    tup=(tssc*rsdsoil+tsdc*rddsoil)/rn
    tdn=(tsdc+tssc*rsdsoil*rddcb)/rn
	rddt=rddct+tddc*rddsoil*tddc/rn
    rsdt=rsdc+tup*tddc
    rdot=rdoc+tddc*(rddsoil*tdoc+rdosoil*tooc)/rn
    rsot=rsoc+tssooc*rsosoil+tdn*rdosoil*tooc+tup*tdoc
    
!	Effect of soil background on canopy absorptances (W. Verhoef, 02-03-04)
	alfast=alfas+tup*alfad
	alfadt=alfad*(1.+tddc*rddsoil/rn)
!
!	Before returning, save current input parameters as old ones
    N_g_old=N_g
    Cab_g_old=Cab_g
    Car_g_old=Car_g
    Cbrown_g_old=Cbrown_g
    Cw_g_old=Cw_g
    Cm_g_old=Cm_g
    N_b_old=N_b
    Cab_b_old=Cab_b
    Car_b_old=Car_b
    Cbrown_b_old=Cbrown_b
    Cw_b_old=Cw_b
    Cm_b_old=Cm_b
	lai_old=lai
	mli_old=mli
	!LIDFa_old=LIDFa
	!LIDFb_old=LIDFb
	fb_old=fb
	diss_old=diss
	psoil_old=psoil
	hot_old=hot
	Cv_old=Cv
	zeta_old=zeta
	tts_old=tts
	tto_old=tto
	psi_old=psi
!
	ifirst=0
!
      return
	end


!	real function Jfunc1(k,l,t)
!!
!!	J1 function with avoidance of singularity problem
!!	
!	real k,l,t,del
!!
!	del=(k-l)*t
!	if (abs(del)>1e-3) then
!		Jfunc1=(exp(-l*t)-exp(-k*t))/(k-l)
!	else
!		Jfunc1=0.5*t*(exp(-k*t)+exp(-l*t))*(1.-del*del/12.)
!	end if	
!!
!	return
!	end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!****************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE Jfunc1(k,l,t,Jout)

	USE datadim
	IMPLICIT NONE

!	J1 function with avoidance of singularity problem
!	
REAL(KIND=8),INTENT(in) :: k,l(nw),t
REAL(KIND=8),INTENT(out) :: Jout(nw)
REAL(KIND=8) :: del(nw)


del=(k-l)*t
WHERE (ABS(del)>1e-3)
	Jout=(EXP(-l*t)-EXP(-k*t))/(k-l)
ELSEWHERE
	Jout=0.5*t*(EXP(-k*t)+EXP(-l*t))*(1.-del*del/12.)
END WHERE

END

!	real function Jfunc2(k,l,t)
!!
!!	J2 function
!!
!	real k,l,t
!!
!	Jfunc2=(1.-exp(-(k+l)*t))/(k+l)
!!
!	return
!	end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!****************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE Jfunc2(k,l,t,Jout)

	USE datadim
	IMPLICIT NONE

!	J2 function

REAL(KIND=8),INTENT(in) :: k,l(nw),t
REAL(KIND=8),INTENT(out) :: Jout(nw)
!REAL(KIND=8) :: Jfunc2(nw)
REAL(KIND=8) :: del(nw)

Jout=(1.-EXP(-(k+l)*t))/(k+l)

END

SUBROUTINE Jfunc2bis(k,l,t,Jout)    !M+ made to treat the call with scalar

	USE datadim
	IMPLICIT NONE

!	J2 function

REAL(KIND=8),INTENT(in) :: k,l,t
REAL(KIND=8),INTENT(out) :: Jout(nw)
!REAL(KIND=8) :: Jfunc2(nw)
REAL(KIND=8) :: del(nw)

Jout=(1.-EXP(-(k+l)*t))/(k+l)

END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!****************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!	real function Jfunc3(m,t)
!!
!!	J3 function for treating (near) conservative scattering
!!
!	real m,t,del
!!
!	del=m*t
!!
!	if (del.gt.1e-3) then
!!
!		e=exp(-del)
!		Jfunc3=(1-e)/(m*(1+e))
!!
!	else
!!
!		Jfunc3=0.5*t*(1.-del*del/12.)
!!
!	end if
!!
!	return
!	end
	
SUBROUTINE Jfunc3(m,t,Jout)

	USE datadim
	IMPLICIT NONE

!	J2 function
REAL(KIND=8),INTENT(in) :: m(nw),t
REAL(KIND=8),INTENT(out) :: Jout(nw)
REAL(KIND=8) :: del(nw), e(nw)

del=m*t
WHERE (del>1e-3)
	e=exp(-del)
	Jout=(1-e)/(m*(1+e))
ELSEWHERE
	Jout=0.5*t*(1.-del*del/12.)
END WHERE
END