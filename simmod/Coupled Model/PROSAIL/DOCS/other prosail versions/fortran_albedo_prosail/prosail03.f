     ********************************************************************
c     ProSail - VERSION 2
c	
c	modifico per simulare il dais 31 lug 02
c      
c
c     ********************************************************************
c     main program - 8 Mars 1999
c     ********************************************************************

c     canopy

      double precision l,tl,sl,vis,rsoil(421),alph_soil
      double precision Theta_s,Phi_s,Theta_v(299),Phi_v(299)
      double precision Phi_vv(299)

      common /sailin/l,tl,sl,vis,rsoil,alph_soil
      common /geom/Theta_s,Theta_v,Phi_v

c     leaf

      double precision n_leaf,vai,k_leaf
      double precision cab,cw,cm

      common /leafin/n_leaf,vai,k_leaf
      common /leafpar/cab,cw,cm

c     satellite

      double precision long(421),rsat(299,421)
      double precision longavi(224),largavi(224)
      double precision longmod(25),largmod(25)
      double precision longmer(15),largmer(15)
      double precision longtm(6),tm(6,421)
      double precision longmisr(4),largmisr(4)
      double precision longhrv(3),hrv(3,421)
      double precision longavhrr(2),avhrr(2,421)
      double precision longpoldersat(4),poldersat(4,421)
	double precision longpolderaero(5),polderaero(5,421)
      integer isat,ihot,linf,lsup,na
      integer nwave(11)
      double precision ldais(63), ldais0(63)
      double precision longdais(63),largdais(63)
      
      common /satin/isat,ihot,linf,lsup,na
      common /wave/long
      common /satout/rsat
      common /aviris/longavi,largavi
      common /modis/longmod,largmod
      common /meris/longmer,largmer
      common /landsat/longtm,tm
      common /misr/longmisr,largmisr
      common /spot/longhrv,hrv
      common /noaa/longavhrr,avhrr
      common /adeos/longpoldersat,poldersat
	common /arat/longpolderaero,polderaero
      common /dais/longdais,largdais  
      
      data (nwave(i),i=1,11)/421,224,25,15,6,4,3,2,4,5,63/

c     ********************************************************************
c     input file = 'inp_sail.m'
c     isat = sensor: 0 = spectroradiometer (400,2500,5)
c                    1 = AVIRIS
c                    2 = MODIS
c                    3 = MERIS
c                    4 = TM
c                    5 = MISR
c                    6 = HRV
c                    7 = AVHRR
c                    8 = POLDER - SATELLITE
c				   9 = POLDER - AIRBORNE 
c                   10 = dais 63 bands
c     ihot		= Hot spot parameter (0->no hot spot, 1->hot spot)
c     Theta_s		= Solar zenith angle
c     tl			= Mean leaf inclination
c     l			= Leaf Area Index
c     vai			= leaf internal structure parameter
c     cab			= leaf chlorophyll a+b content
c     cw			= leaf equivalent water thickness
c     cm			= leaf dry matter content
c     sl			= Leaf size / crop height
c     vis			= Horizontal visibility
c	alph_soil	= Soil parameter
c     na			= number of interpolation directions (Theta_v,Phi_v)
c     Rsoil		= soil albedo
c     ********************************************************************

      open (1,file='inp_sail.m',status='unknown')
      read (1,*)
      read (1,*) isat
      read (1,*) ihot
      read (1,*) Theta_s,Phi_s
      read (1,*) tl
      read (1,*) l
      read (1,*) vai
      read (1,*) cab
      read (1,*) cw
      read (1,*) cm
      read (1,*) sl
      read (1,*) vis
	read (1,*) alph_soil
      read (1,*) na
      do i=1,na
         read (1,*) Theta_v(i),Phi_vv(i)
	   Phi_v(i)=Phi_vv(i)
         if(Phi_v(i).gt.180)  then
	      Phi_v(i)=(360-Phi_v(i))
	   endif
      enddo
      close(1)

      open (3,file='rsoil.txt',status='unknown')
      do i=1,421
         read (3,*) rsoil(i)
      enddo
      close(3)

		
      call sailsat

      open (2,file='out_sail.m',form='formatted',status='unknown')
      
      write (2,*) '                       0  %'
      write (2,*) '             400    2500  %'
      write (2,*) '             930    1010  %'
      write (2,*) '            1650    1900  %' 
      write (2,*) '                       6  %'
      write (2,*) '          %  *******positionvalue   (fixed)'
      write (2,*) '               1    1.35  %'
      write (2,*) '               6   56.46  %'
      write (2,*) '          %  *******positionvalue   (free)'
      write (2,*) '               2      50  %'
      write (2,*) '               3    0.01  %'
      write (2,*) '               4   0.006  %'
      write (2,*) '               5       3  %'
      write (2,*) '               7    0.05  %'
      write (2,*) '               8       1  %'
      write (2,*) '          %  ***********************************'
      write (2,*) '               1     2.5  %'
      write (2,*) '               1     100  %'
      write (2,*) '           0.005    0.05  %'
      write (2,*) '           0.001    0.03  %'
      write (2,*) '            0.05      10  %'
      write (2,*) '               5      85  %'
      write (2,*) '           0.001       1  %'
      write (2,*) '             0.5       2  %'
      write (2,*) '          %  ********************************'
      write (2,*) '               1  %'
      write (2,*) '              40  %'
      write (2,*) '               3  %'
      write (2,*) '              10  %'
      write (2,10) Theta_s,Phi_s
      write (2,10) Theta_s,Phi_s
      write (2,10) Theta_s,Phi_s
      write (2,10) Theta_v(1), Phi_v(1)
      write (2,10) Theta_v(2), Phi_v(2)
      write (2,10) Theta_v(3), Phi_v(3)
      do i=1,nwave(isat+1)
         goto (100,200,300,400,500,600,700,800,900,1000),isat     
         write (2,20) long(i),(rsat(j,i),j=1,na)
         goto 2000
100      write (2,20) longavi(i),(rsat(j,i),j=1,na)
         goto 2000
200      write (2,20) longmod(i),(rsat(j,i),j=1,na)
         goto 2000
300      write (2,20) longmer(i),(rsat(j,i),j=1,na)
         goto 2000
400      write (2,20) longtm(i),(rsat(j,i),j=1,na)
         goto 2000
500      write (2,20) longmisr(i),(rsat(j,i),j=1,na)
         goto 2000
600      write (2,20) longhrv(i),(rsat(j,i),j=1,na)
         goto 2000
700      write (2,20) longavhrr(i),(rsat(j,i),j=1,na)
         goto 2000
800      write (2,20) longpoldersat(i),(rsat(j,i),j=1,na)
         goto 2000
900	   write (2,20) longpolderaero(i),(rsat(j,i),j=1,na)
         goto 2000
1000     write (2,20) longdais(i),(rsat(j,i),j=1,na)
         goto 2000
2000     continue
      enddo
      close (2)
10    format(1200(2x,f7.1))
20    format(2x,f7.2,1199(2x,f7.4))
      
      stop
      end

c     ********************************************************************
