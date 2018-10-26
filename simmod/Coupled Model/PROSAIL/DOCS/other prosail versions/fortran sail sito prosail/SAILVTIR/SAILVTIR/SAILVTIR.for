	program SAILVTIR

	implicit integer (i-n), double precision (a-h,o-z)

	character*100 inputname,outname,argv
	logical file_exists

	real*8 lai,lam
	real, allocatable :: wl(:),Es(:),Ed(:),rs(:),rl(:),tl(:),ro(:),Lo(:)

      common /angles/pi,rd,tts,tto,psi
      common /sail_input/lai,a,b,q,rho,tau,rsoil
	common /thermal/gammasdf,gammasdb,gammaso
      common /rfltrn/tss,too,tsstoo,rdd,tdd,rsd,tsd,rdo,tdo,rso,rsos,
     +				   rsod,rddt,rsdt,rdot,rsodt,rsost,rsot
	
	pi=4.d0*atan(1.)
	rd=pi/180.

	c1=3.741856e-16		! This is pi * as much, to obtain fluxes instead of radiances!
	c2=14388.				! c2 in units of micron * Kelvin

	narg=iargc()
	if (narg.ne.1) then
		if (narg.eq.0) then
			print '("input file name: ",$)'
			read (*,'(a)') inputname
		else
			print '("not more than one argument please")'
			stop
		end  if
	else
		call getarg(1,argv)
		read (argv,*) inputname	
	end if

	inquire (file=inputname,exist=file_exists)
	if (.not.file_exists) then
		print '("file ",a," does not exist")',trim(inputname)
		stop
	end if
	
	l=len_trim(inputname)
	outname=trim(inputname)
	outname(l-2:l)="out" 
	open (2,file=outname)	

	open (1,file=inputname)

	read (1,*) nb
	read (1,*) tts
	read (1,*) azi
	read (1,*)
	
	allocate (wl(nb),Es(nb),Ed(nb),rs(nb),rl(nb),tl(nb),ro(nb),Lo(nb))

	do ib=1,nb
		read (1,*) wl(ib),Es(ib),Ed(ib),rs(ib),rl(ib),tl(ib)
	end do
	read (1,*) lai				! Not used in this program
	read (1,*) a				! LIDF a
	read (1,*) b				! LIDF b
	read (1,*) q				! hot spot
	read (1,*) ThC				! sunlit leaves Celsius
	read (1,*) TcC				! shaded leaves Celsius
	read (1,*) TsC				! sunlit soil Celsius
	read (1,*) TdC				! shaded soil Celsius

	close(1)

	Tvh=ThC+273.15
	Tvc=TcC+273.15
	Tbs=TsC+273.15
	Tbd=TdC+273.15

	tto=0
	psi=0

	do ilai=0,24

		lai=ilai*0.25

		do ib=1,nb

			lam=wl(ib)

			top=1.e-6*c1*(lam*1e-6)**(-5)
			Hc=top/(exp(c2/(lam*Tvc))-1.)
			Hh=top/(exp(c2/(lam*Tvh))-1.)
			Hd=top/(exp(c2/(lam*Tbd))-1.)
			Hs=top/(exp(c2/(lam*Tbs))-1.)

			Esun=Es(ib)
			Esky=Ed(ib)

			if (ib.eq.nb) then
				Esky=top/(exp(c2/(lam*(Ed(nb)+273.15)))-1.)
			end if

			rho=rl(ib)
			tau=tl(ib)
			rsoil=rs(ib)
			emv=1-rho
			ems=1-rsoil

			call FourSAIL
			
			Etot=Esun+Esky
			ro(ib)=(rsot*Esun+rdot*Esky)/Etot
			rad=ro(ib)*Etot/pi

			gammad=1.-rdd-tdd	
			gammao=1.-rdo-tdo-too
			ttot=(too+tdo)/(1.-rsoil*rdd)
			gammaot=gammao+ttot*rsoil*gammad
			gammasot=gammaso+ttot*rsoil*gammasdf
			tso=tsstoo+tss*(tdo+rsoil*rdd*too)/(1.-rsoil*rdd)
			aeev=gammaot
			aees=ttot*ems
			aeet=aeev+aees
			rad=rad+(aeev*Hc+gammasot*emv*(Hh-Hc)
     +			+aees*Hd+tso*ems*(Hs-Hd))/pi
			Lo(ib)=rad
			dnoem1=top/(rad*pi)
			Tbright=c2/(lam*log(dnoem1+1.))-273.15
	
		end do

c		print '(f8.1,17f8.3)',tt,1.-too,aeet,ro,Lo,Tbright		
		write (2,'(f8.2,17f10.5)') lai,1.-too,aeet,ro,Lo,Tbright

	end do

	close (2)
c
	stop
	end	