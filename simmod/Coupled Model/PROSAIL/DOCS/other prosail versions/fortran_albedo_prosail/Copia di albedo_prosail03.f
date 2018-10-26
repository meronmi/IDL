c     ********************************************************************
c     ProSail - VERSION albedo feb 2004
c     ********************************************************************
c     main program
c     ********************************************************************

c	albedo
	double precision w_sw(140), w_par(61)
	double precision albedo_sw, albedo_par


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

      double precision long(421),rsat(4,421)
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
	
	data (w_sw(i), i=1,140)/
     &0.008 ,0.0083,0.0087,0.0088,0.0087,0.0082,0.0084,0.0092,0.0097,
     &0.0105,0.0108,0.0108,0.0109,0.0108,0.0109,0.0112,0.0108,0.0105,
     &0.0108,0.0108,0.0107,0.0108,0.0105,0.0102,0.0103,0.0106,0.0107,
     &0.0105,0.0105,0.0105,0.0105,0.0104,0.0103,0.0102,0.0102,0.0103,
     &0.0103,0.0098,0.0093,0.0098,0.0099,0.0099,0.0097,0.0097,0.0097,
     &0.0095,0.0095,0.0095,0.0095,0.0092,0.0089,0.0088,0.0091,0.0092,
     &0.0091,0.009,0.0089,0.0075,0.008,0.0081,0.0081,0.0082,0.0081,
     &0.0063,0.007,0.0065,0.0071,0.0075,0.0076,0.0078,0.0077,0.0064,
     &0.0043,0.0067,0.0074,0.0073,0.0073,0.007,0.0069,0.0069,0.0068,
     &0.0067,0.006,0.0052,0.0049,0.0056,0.0056,0.0062,0.0063,0.0062,
     &0.0059,0.006,0.0061,0.006,0.0061,0.0061,0.006,0.006,0.0057,0.0043,
     &0.004,0.004,0.0039,0.0037,0.0045,0.0039,0.0014,0.0014,0.0019,
     &0.0011,0.0012,0.0021,0.0025,0.0035,0.0039,0.0039,0.004,0.0044,
     &0.0047,0.0047,0.0046,0.0046,0.0045,0.0045,0.0044,0.0044,0.0044,
     &0.0043,0.0043,0.0042,0.0042,0.0041,0.0041,0.004,0.0039,0.0037,
     &0.0038,0.0036,0.0033,0.0035/
	data (w_par(i), i=1,61)/
     &0.0135,0.014,0.0147,0.0148,0.0145,0.0137,0.0141,0.0155,0.0163,
     &0.0176,0.0182,0.0182,0.0183,0.0182,0.0183,0.0187,0.0181,0.0177,
     &0.0182,0.0181,0.0179,0.0181,0.0177,0.0171,0.0172,0.0177,0.0179,
     &0.0176,0.0176,0.0177,0.0176,0.0174,0.0172,0.0172,0.0172,0.0173,
     &0.0173,0.0165,0.0155,0.0164,0.0167,0.0166,0.0163,0.0163,0.0163,
     &0.016,0.0159,0.016,0.0159,0.0154,0.0149,0.0148,0.0154,0.0154,
     &0.0152,0.0151,0.0149,0.0126,0.0135,0.0135,0.0136/



      open (1,file='lai.txt',status='unknown')
      isat=0
      ihot=1
      Theta_s =43
	Phi_s = 106
	tl= 56.5
      read (1,*) l
      vai = 1.37
      cab = 45
      cw = 0.0092
      cm = 0.0065
      sl = 0.005
      vis = 40
	alph_soil = 1
      na = 1
      Theta_v(1)=0
	Phi_vv(1)=0
	Phi_v(1)=1
      close(1)
 
      open (3,file='rsoil.txt',status='unknown')
      do i=1,421
         read (3,*) rsoil(i)
      enddo
      close(3)

      call sailsat
	
c	calcolo albedo short wave radition e PAR
	albedo_sw=0
	albedo_par=0
	do i=1, 140
		albedo_sw=albedo_sw+w_sw(i)*rsat(1,i)
	enddo
	do i=1, 61
		albedo_par=albedo_par+w_par(i)*rsat(1,i)
	enddo
      open (2,file='albedo.txt',form='formatted',status='unknown')
      write (2,30) albedo_sw, albedo_par
c	write (2,10) Theta_s,Phi_s
c      write (2,10) Theta_v(1), Phi_v(1)

c      do i=1,nwave(isat+1)
c         goto (100,200,300,400,500,600,700,800,900,1000),isat     
c         write (2,20) long(i),(rsat(j,i),j=1,na)
c         goto 2000
c100      write (2,20) longavi(i),(rsat(j,i),j=1,na)
c         goto 2000
c200      write (2,20) longmod(i),(rsat(j,i),j=1,na)
c         goto 2000
c300      write (2,20) longmer(i),(rsat(j,i),j=1,na)
c         goto 2000
c400      write (2,20) longtm(i),(rsat(j,i),j=1,na)
c         goto 2000
c500      write (2,20) longmisr(i),(rsat(j,i),j=1,na)
c         goto 2000
c600      write (2,20) longhrv(i),(rsat(j,i),j=1,na)
c         goto 2000
c700      write (2,20) longavhrr(i),(rsat(j,i),j=1,na)
c         goto 2000
c800      write (2,20) longpoldersat(i),(rsat(j,i),j=1,na)
c         goto 2000
c900	   write (2,20) longpolderaero(i),(rsat(j,i),j=1,na)
c         goto 2000
c1000     write (2,20) longdais(i),(rsat(j,i),j=1,na)
c         goto 2000
c2000     continue
c      enddo
      close (2)
10    format(1200(2x,f7.1))
20    format(2x,f7.2,1199(2x,f7.4))
30    format(2f7.4)      
      stop
      end



