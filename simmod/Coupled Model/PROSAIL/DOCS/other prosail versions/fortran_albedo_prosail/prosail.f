c     ********************************************************************
c     ProSail - VERSION 2
c	
c	prise en compte d'un paramètre alpha du sol
c     ********************************************************************
c
c     Stephane Jacquemoud (1), Cedric Bacour (1), Frederic BARET (2),
c     Andres KUUSK (3)
c
c     (1) Universite Paris 7
c         Laboratoire Environnement et Developpement
c         Case postale 7071
c         2 place Jussieu
c         75251 Paris Cedex 05
c         France
c         tel: (33)-01-44-27-60-47
c         fax: (33)-01-44-27-81-46
c         Email: jacquemo@ccr.jussieu.fr
c
c     (2) Institut National de la Recherche Agronomique
c         Station de Bioclimatologie
c         BP 91
c         84914 Avignon Cedex 09 (France)
c         tel: (33)-04-90-31-60-82
c         fax: (33)-04-90-89-98-10
c         email: baret@avignon.inra.fr
c
c     (3) Tartu Observatory
c         EE2444 Toravere
c         Estonia
c         tel: (7)-1437-10358
c         fax: (7)-1434-10205
c         email: andres@aai.ee
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
      integer nwave(10)

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
      
      data (nwave(i),i=1,10)/421,224,25,15,6,4,3,2,4,5/

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
      write (2,10) Theta_s,(Theta_v(j),j=1,na)
      write (2,10) Phi_s,(Phi_vv(j),j=1,na)
      do i=1,nwave(isat+1)
         goto (100,200,300,400,500,600,700,800,900),isat     
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
2000     continue
      enddo
      close (2)
10    format(1200(2x,f7.1))
20    format(2x,f7.2,1199(2x,f7.4))
      
      stop
      end

c     ********************************************************************
