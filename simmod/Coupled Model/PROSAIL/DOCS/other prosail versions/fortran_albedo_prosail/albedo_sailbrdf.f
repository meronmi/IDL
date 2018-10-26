c     ********************************************************************
c     sailbrdf
c     ********************************************************************
c     M Modifico il codice in modo che al posto di res ci sia un assorbimento septtrale
c	della canopy modellato con solo i flussi diffusi
c	abs=([E-(0) + E+(-1)]-[E+(0) + E-(-1)])/E-(0)
      subroutine sailbrdf()

      double precision integr,xx

c     canopy

      double precision l,tl,sl,vis,rsoil(421),alph_soil
      double precision res(4,421),ares(421),tres(421),albres(421)
      double precision Theta_s(10),Theta_v(10),Phi_v(10)
      double precision pi,rd,ff(9)
      double precision excent,rtp,rtm,rs1,rs2,rs3
      double precision costs,sints,tgs,costo,sinto,tgo,psir,cospsi
      double precision calph,alph2,sab
      double precision a,sig,ks,ko,s,ss,u,v,wo
      double precision tll,snl,sn2l,cs2l,csl,tgl
      double precision bs,bo,bt1,bt2,b1,b2,b3
      double precision fl,sks,sko,ci1,ci2,ci3
      double precision m,h1,h2,cks,cko,co,cs,doo,ds,ho
      double precision tss,too,g,rdd,tdd,rsd,tsd,rdo,tdo,rso,q
      double precision xy1,xy2,hskorr,xy,eq2,eq3,eq1,eqq

      double precision xo,zd,zs,ad,bd,cd,df,skyl,ci0,zdd

      common /sailin/l,tl,sl,vis,rsoil,alph_soil
      common /sailout/res,ares,tres,albres
      common /geom/Theta_s,Theta_v,Phi_v
      common /cst/pi,rd,ff

c     leaf

      double precision n_leaf,vai,k_leaf
      double precision cab,cw,cm
      double precision refl,tran
      double precision refra(421),ke_leaf(421),
     & kab_leaf(421),kw_leaf(421),km_leaf(421)
      double precision long(421)

      common /leafin/n_leaf,vai,k_leaf
      common /leafpar/cab,cw,cm
      common /leafout/refl,tran
      common /dat/refra,ke_leaf,kab_leaf,kw_leaf,km_leaf
      common /wave/long

c     satellite

      integer isat,ihot,linf,lsup,na

      common /satin/isat,ihot,linf,lsup,na

      intrinsic dfloat,dabs,dsqrt,dexp,dsin,dcos,dtan,dacos

      integr(xx)=(1.0d+0-dexp(-xx))/xx
      pi=dacos(0.0d+0)*2.0d+0
      rd=pi/180.0d+0
      excent=(tl*rd/9.65d+0)**(-1.d+0/1.65d+0)-3.d+0

      call ellips(excent)
      call valeur

      do j=linf,lsup
         n_leaf=refra(j)
         k_leaf=ke_leaf(j)+(cab*kab_leaf(j)+cw*kw_leaf(j)
     &          +cm*km_leaf(j))/vai
         call leaf
         rtp=(refl+tran)*0.5d+0
         rtm=(refl-tran)*0.5d+0
         rs1=alph_soil*rsoil(j)
         rs2=alph_soil*rsoil(j)
         rs3=alph_soil*rsoil(j)
         do jj=1,na
	    costs=dcos(Theta_s(jj)*rd)
            sints=dsin(Theta_s(jj)*rd)
            tgs=dtan(Theta_s(jj)*rd)
            costo=dcos(Theta_v(jj)*rd)
            sinto=dsin(Theta_v(jj)*rd)
            tgo=dtan(Theta_v(jj)*rd)
            psir=Phi_v(jj)*rd
            cospsi=dcos(psir)
            calph=sints*sinto*cospsi+costs*costo
            alph2=dabs(dacos(calph)/2.0d+0)
            xy=dabs(costs*costs+costo*costo-2.0d+0*costs*costo*calph)
            sab=dsqrt(xy)/(costs*costo)

            a=0.d+0
            sig=0.d+0
            ks=0.d+0
            ko=0.d+0
            s=0.d+0
            ss=0.d+0
            u=0.d+0
            v=0.d+0
            wo=0.d+0

c     loop on 9 angle classes

            do i=1,9
               tll=10.d+0*dfloat(i)-5.d+0
               snl=dsin(tll*rd)
               sn2l=snl*snl
               cs2l=1.d+0-sn2l
               csl=dsqrt(cs2l)
               tgl=dtan(tll*rd)

c     determination of betas, betao, beta1, beta2, beta3

               bs=pi
               bo=pi
               if(tll+Theta_s(jj).gt.90.d+0) bs=dacos(-1.d+0/(tgs*tgl))
               if(tll+Theta_v(jj).gt.90.d+0) bo=dacos(-1.0d+0/(tgo*tgl))
               bt1=dabs(bs-bo)
               bt2=2.d+0*pi-bs-bo
               if(psir.le.bt1) then
                  b1=psir
                  b2=bt1
                  b3=bt2
               elseif (psir.ge.bt2) then
                  b1=bt1
                  b2=bt2
                  b3=psir
               else
                  b1=bt1
                  b2=psir
                  b3=bt2
               endif

c     determination of scattering coefficients a,sig,ks,ko,s,ss,u,v,w

               fl=ff(i)*l
c			 a(tl), della foglia: rtp=(refl+tran)*0.5d+0, rtm=(refl-tran)*0.5d+0
               a=a+(1.d+0-rtp+rtm*cs2l)*fl
c			 sigma(tl)
               sig=sig+(rtp+rtm*cs2l)*fl
c			 k(tl)/fl
               sks=((bs-pi*0.5d+0)*csl+dsin(bs)*tgs*snl)*2.d+0/pi
c			 K(tl)/fl
               sko=((bo-pi*0.5d+0)*csl+dsin(bo)*tgo*snl)*2.d+0/pi
c			 k(tl)               
			 ks=ks+sks*fl
c			 K(tl)/fl
               ko=ko+sko*fl
c			 s(tl)
               s=s+(rtp*sks-rtm*cs2l)*fl
c			 ss(tl)
               ss=ss+(rtp*sks+rtm*cs2l)*fl
c			 u(tl)               
			 u=u+(rtp*sko-rtm*cs2l)*fl
c			 v(tl)
               v=v+(rtp*sko+rtm*cs2l)*fl
c			 w(tl)
               ci1=sn2l*tgs*tgo
               ci2=2.d+0*cs2l+ci1*dcos(psir)
               ci3=dsin(b2)*(2.d+0*cs2l/(dcos(bs)*dcos(bo))+dcos(b1)
     &             *dcos(b3)*ci1)
               wo=wo+((pi*refl*0.5-b2*rtp)*ci2+rtp*ci3)*fl/pi

            enddo

c     determination of intermediate variables

            m=a*a-sig*sig
            if(m.le.0.d+0) m=0.d+0
            m=dsqrt(m)
            h1=(a+m)/sig
            h2=1.d+0/h1
            cks=ks*ks-m*m
            cko=ko**2-m**2
            co=(v*(ko-a)-u*sig)/cko
            cs=(ss*(ks-a)-s*sig)/cks
            doo=(-u*(ko+a)-v*sig)/cko
            ds=(-s*(ks+a)-ss*sig)/cks
            if(ihot.eq.0) then
               ho=(s*co+ss*doo+wo)/(ko+ks)
            else
               ho=(s*co+ss*doo)/(ko+ks)
            endif

c     determination of reflectances  and transmittances of one stratum

            tss=dexp(-ks)
            too=dexp(-ko)
            g=h1*dexp(m)-h2*dexp(-m)
            rdd=(dexp(m)-dexp(-m))/g
            tdd=(h1-h2)/g
            rsd=cs*(1-tss*tdd)-ds*rdd
            tsd=ds*(tss-tdd)-cs*tss*rdd
            rdo=co*(1.d+0-too*tdd)-doo*rdd
            tdo=doo*(too-tdd)-co*too*rdd
            if(ihot.eq.0) then
               rso=ho*(1.0-tss*too)-co*tsd*too-doo*rsd
            else
               q=ko+ks
               xy1=sab/sl
               if(xy1.lt.0.1d-6) then
                  xy2=1.0
               elseif (xy1.gt.20.d+0) then
                  xy2=1.0/xy1
               else
                  xy2=integr(xy1)
               endif
               hskorr=dsqrt(ks*ko)*xy2
               xy=alph2/(dsqrt(dabs(costs*costo))*sl)
               xy1=q/2.0+xy
               if(xy1.gt.20.d+0) then
                  xy2=1.d+0
               else
                  xy2=1.d+0-dexp(-xy1)
               endif
               eq2=xy2/xy1
               xy1=q+xy
               if(xy1.gt.20.d+0) then
                  xy2=1.d+0
               else
                  xy2=1.d+0-dexp(-xy1)
               endif
               eq3=xy2/xy1
               eq1=integr(q)
               eqq=eq1+eq2-eq3
               rso=ho*(1.d+0-tss*too)+wo*eqq-co*tsd*too-doo*rsd
            endif

c     hemispherical/direct (zd) and bidirectional (zs) reflectances

            xo=1.d+0-rs3*rdd
            zd=rdo+tdd*(rs3*tdo+rs2*too)/xo
            if(ihot.eq.0) then
               zs=rso+tss*rs1*too
            else
               zs=rso+rs1*dexp(-(q-hskorr))
            endif
            zs=zs+((tss*rs2+tsd*rs3)*tdo+(tsd+tss*rs2*rdd)*rs2*too)/xo

c	hemispherical ref for hemispherical incidence
            zdd=rdd+(tdd*rs3*tdd)/xo
c     bidirectional reflectance (res)
c	M
c	absortion
          ad=(0.0003514d+0*Theta_s(jj)*Theta_s(jj)-0.01412d+0
     &    *Theta_s(jj)+1.1379d+0)*(0.01641d+0*vis-2.5823d+0)
     &    /(0.03938d+0*vis+0.04093d+0)+0.00009061d+0
     &    *Theta_s(jj)*Theta_s(jj)+0.0005452d+0*Theta_s(jj)
     &    -0.07179d+0
          bd=(0.0002153d+0*Theta_s(jj)*Theta_s(jj)-0.007015d+0
     &    *Theta_s(jj)+1.0428d+0)*(0.05398d+0*vis+1.9824d+0)
     &    /(0.06569d+0*vis+0.1213d+0)+0.000004612d+0
     &    *Theta_s(jj)*Theta_s(jj)*Theta_s(jj)-0.0004266d+0
     &    *Theta_s(jj)*Theta_s(jj)+0.01349d+0*Theta_s(jj)-0.1430d+0
          cd=(0.0005937d+0*Theta_s(jj)*Theta_s(jj)-0.02650d+0
     &    *Theta_s(jj)+1.3005d+0)*(-0.002792d+0*vis+0.3844d+0)
     &    /(2.5084d+0*vis-3.3878d+0)+0.0001d+0*
     &    (0.007934d+0*Theta_s(jj)*Theta_s(jj)-0.2284d+0
     &    *Theta_s(jj)+0.8088d+0)
          df=dexp((-0.0076d+0*long(j)+1.4570d+0)/(0.0012d+0
     &    *long(j)+0.7657d+0))
            skyl=ad*df*df+bd*df+cd
c		  R=E0/([E-(z=0)]+[Es(z=0)])
c		  dato che skyl=Esky/(Esky+Esun)=[E-(z=0)]/([E-(z=0)]+[Es(z=0)])
c		  se pongo Es(z=0)=1, allora Esky=[E-(z=0)]=skyl/(1-skyl)=ci0 e
c		  ([E-(z=0)]+[Es(z=0)])= ci0 + 1
            ci0=skyl/(1.d+0-skyl)
	      res(jj,j)=(zs+ci0*zd)/(ci0+1.d+0)
c		ALBEDO diffuse fluxes
		  albres(j)=zdd
c		ABSORPTION diffuse fluxes
		  ares(j)=1.d+0-zdd+((tdd/xo)*(rs3-1))
c		SW TRNSMITTANCE diffuse fluxes
		  tres(j)=tdd/xo
         enddo
      enddo

      return
      end

c     ********************************************************************
c     ellips : determination of the foliar angle distribution function
c     from the mean leaf angle tl: ellipsoidal distribution
c     ********************************************************************

      subroutine ellips(excent)

      double precision excent,pi,rd,ff(9)
      double precision som,gd,theta,aex

      intrinsic dfloat,dcos,dsin

      common /cst/pi,rd,ff

      som=0.d+0

c     initialization of 9 angle classes

      do i=1,9
         aex=excent
         theta=5.d+0+10.d+0*dfloat(i-1)
         theta=theta*rd
	 gd=aex+1.744d+0*(aex+1.182d+0)**(-0.733d+0)
	 ff(i)=2*aex**3*dsin(theta)/
     &	       (gd*(dcos(theta)**2+aex**2*dsin(theta)**2)**2)
         som=som+ff(i)
      enddo
      do i=1,9
         ff(i)=ff(i)/som
      enddo

      return
      end

c     ********************************************************************
