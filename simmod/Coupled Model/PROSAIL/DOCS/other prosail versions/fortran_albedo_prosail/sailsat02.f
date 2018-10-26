c     ********************************************************************
c     sailsat : simulation of the AVIRIS, MODIS, MERIS, TM, MISR, HRV,
c     AVHRR, POLDER (Satellite and Airborne), XSTAR, DAIS-63bands IT2002 wavebands
c     ********************************************************************
c	MODIS bands B1 and B2 are modified by Michele according to ftp://ftp.mcst.ssai.biz/pub/permanent/MCST/PFM_L1B_LUT_4-30-99/
c     in sailsat they are band number 2 and 6
      subroutine sailsat()

c     canopy

      double precision res(4,421),ares(421),tres(421),albres(421)

      common /sailout/res,ares,tres,albres

c     satellite

      double precision long(421),rsat(4,421)
	double precision asat(421), tsat(421), albsat(421)
      double precision longavi(224),largavi(224)
      double precision longmod(25),largmod(25)
      double precision longmer(15),largmer(15)
      double precision longtm(6),tm(6,421),sumtm(6),rotm
      double precision longmisr(4),largmisr(4)
      double precision longhrv(3),hrv(3,421),sumhrv(3),rohrv
      double precision longavhrr(2),avhrr(2,421),sumavhrr(2),roavhrr
      double precision longpoldersat(4),poldersat(4,421),
     &				 sumpoldersat(4),ropoldersat
      double precision longpolderaero(5),polderaero(5,421),
     &				 sumpolderaero(5),ropolderaero
c     dais_mod
      double precision longdais(63),largdais(63)
      double precision longrosis(95),largrosis(95)      
c	mivis_mod
	double precision longmivis(92),largmivis(92)
      double precision idat(5,421),odat(5,2000)
      double precision iwvl(421),owvl(224),owdt(224)

      integer isat,ihot,linf,lsup,na
      integer iclm,inmb,onmb
      integer inftm(6),suptm(6),numtm(6)
      integer infhrv(3),suphrv(3),numhrv(3)
      integer infavhrr(2),supavhrr(2),numavhrr(2)
      integer infpoldersat(4),suppoldersat(4),numpoldersat(4)
      integer infpolderaero(5),suppolderaero(5),numpolderaero(5)
      
      common /satin/isat,ihot,linf,lsup,na
      common /satout/rsat,asat,tsat,albsat
      common /wave/long
      common /aviris/longavi,largavi
      common /modis/longmod,largmod
      common /meris/longmer,largmer
      common /landsat/longtm,tm
      common /misr/longmisr,largmisr
      common /spot/longhrv,hrv
      common /noaa/longavhrr,avhrr
      common /adeos/longpoldersat,poldersat
      common /arat/longpolderaero,polderaero
c     dais_mod
      common /dais/longdais,largdais
      common /rosis/longrosis,largrosis
c	mivis_mod
	common /mivis/longmivis, largmivis      

      data (inftm(i),i=1,6)/7,21,40,67,224,316/
      data (suptm(i),i=1,6)/29,49,68,108,293,400/
      data (numtm(i),i=1,6)/23,29,29,42,70,85/
      data (sumtm(i),i=1,6)/12.209d+0,15.262d+0,13.15d+0,24.119d+0,
     &                      43.047d+0,47.899d+0/

      data (infhrv(i),i=1,3)/14,39,69/
      data (suphrv(i),i=1,3)/61,72,109/
      data (numhrv(i),i=1,3)/48,34,41/
      data (sumhrv(i),i=1,3)/17.58d+0,12.88d+0,20.09d+0/

      data (infavhrr(i),i=1,2)/29,58/
      data (supavhrr(i),i=1,2)/76,132/
      data (numavhrr(i),i=1,2)/48,75/
      data (sumavhrr(i),i=1,2)/22.4682d+0,45.3802d+0/

      data (infpoldersat(i),i=1,4)/4,48,62,81/
      data (suppoldersat(i),i=1,4)/16,60,86,105/
      data (numpoldersat(i),i=1,4)/13,13,25,25/
      data (sumpoldersat(i),i=1,4)/4.092d+0,4.220d+0,7.57d+0,7.602d+0/

      data (infpolderaero(i),i=1,5)/5,28,52,83,99/
      data (suppolderaero(i),i=1,5)/15,35,60,105,109/
      data (numpolderaero(i),i=1,5)/11,8,9,23,11/
      data (sumpolderaero(i),i=1,5)/2.2369d+0,1.1631d+0,1.3078d+0,
     &				  3.9078d+0,2.3170d+0/

      call wavesat
      call valeur
c     dais_mod; mivis_mod      
      goto (100,200,300,400,500,600,700,800,900,1000,1100,1200),isat


c     simulation of the spectral reflectance with a 5 nm step

      linf=1
      lsup=421
      call sailbrdf
      do j=1,na
         do i=linf,lsup
            rsat(j,i)=res(j,i)
				asat(i)=ares(i)
				tsat(i)=tres(i)
				albsat(i)=albres(i)
         enddo
      enddo
      goto 2000

c     simulation of the AVIRIS wavebands

100   linf=1
      lsup=421
      inmb=421
      onmb=224
      iclm=1
      call sailbrdf
      do j=1,na
         do i=1,inmb
            iwvl(i)=long(i)
            idat(1,i)=res(j,i)
         enddo
         do i=1,onmb
            owvl(i)=longavi(i)
            owdt(i)=largavi(i)
         enddo
         call gauss(inmb,iwvl,idat,iclm,onmb,owvl,owdt,odat)
         do i=1,onmb
            rsat(j,i)=odat(1,i)
         enddo
      enddo
      goto 2000

c     simulation of the MODIS wavebands

200   linf=10
      lsup=417
      inmb=408
      onmb=25
      iclm=1
      call sailbrdf
      do j=1,na
         do i=1,inmb
            iwvl(i)=long(i+linf-1)
            idat(1,i)=res(j,i+linf-1)
         enddo
         do i=1,onmb
            owvl(i)=longmod(i)
            owdt(i)=largmod(i)
         enddo
         call gauss(inmb,iwvl,idat,iclm,onmb,owvl,owdt,odat)
         do i=1,onmb
            rsat(j,i)=odat(1,i)
         enddo
      enddo
      goto 2000

c     simulation of the MERIS wavebands

300   linf=1
      lsup=146
      inmb=146
      onmb=15
      iclm=1
      call sailbrdf 
      do j=1,na
         do i=1,inmb
            iwvl(i)=long(i+linf-1)
            idat(1,i)=res(j,i+linf-1)
         enddo
         do i=1,onmb
            owvl(i)=longmer(i)
            owdt(i)=largmer(i)
         enddo
         call gauss(inmb,iwvl,idat,iclm,onmb,owvl,owdt,odat)
         do i=1,onmb
            rsat(j,i)=odat(1,i)
         enddo
      enddo
      goto 2000

c     simulation of the TM wavebands

400   do i=1,6
         linf=inftm(i)
         lsup=suptm(i)
         call sailbrdf
         do j=1,na
            rotm=0.0d+0
            do jj=1,numtm(i)
               rotm=rotm+tm(i,jj+linf-1)*res(j,jj+linf-1)
            enddo
            rsat(j,i)=rotm/sumtm(i)
         enddo
      enddo
      goto 2000

c     simulation of the MISR wavebands

500   linf=1
      lsup=113
      inmb=113
      onmb=4
      iclm=1
      call sailbrdf
      do j=1,na
         do i=1,inmb
            iwvl(i)=long(i+linf-1)
            idat(1,i)=res(j,i+linf-1)
         enddo
         do i=1,onmb
            owvl(i)=longmisr(i)
            owdt(i)=largmisr(i)
         enddo
         call gauss(inmb,iwvl,idat,iclm,onmb,owvl,owdt,odat)
         do i=1,onmb
            rsat(j,i)=odat(1,i)
         enddo
      enddo
      goto 2000

c     simulation of the HRV wavebands

600   do i=1,3
         linf=infhrv(i)
         lsup=suphrv(i)
         call sailbrdf
         do j=1,na
            rohrv=0.0d+0
            do jj=1,numhrv(i)
               rohrv=rohrv+hrv(i,jj+linf-1)*res(j,jj+linf-1)
            enddo
            rsat(j,i)=rohrv/sumhrv(i)
         enddo
      enddo
      goto 2000

c     simulation of the AVHRR wavebands

700   do i=1,2
         linf=infavhrr(i)
         lsup=supavhrr(i)
         call sailbrdf
         do j=1,na
            roavhrr=0.0d+0
            do jj=1,numavhrr(i)
               roavhrr=roavhrr+avhrr(i,jj+linf-1)*res(j,jj+linf-1)
            enddo
            rsat(j,i)=roavhrr/sumavhrr(i)
         enddo
      enddo
      goto 2000

c     simulation of the POLDER-SATELLITE wavebands

800   do i=1,4
         linf=infpoldersat(i)
         lsup=suppoldersat(i)
         call sailbrdf
         do j=1,na
            ropoldersat=0.0d+0
            do jj=1,numpoldersat(i)
               ropoldersat=ropoldersat+
     &			poldersat(i,jj+linf-1)*res(j,jj+linf-1)
            enddo
            rsat(j,i)=ropoldersat/sumpoldersat(i)     
         enddo
      enddo
      goto 2000

c     simulation of the POLDER-AIRBORNE wavebands

900   do i=1,4
         linf=infpolderaero(i)
         lsup=suppolderaero(i)
         call sailbrdf
         do j=1,na
            ropolderaero=0.0d+0
            do jj=1,numpolderaero(i)
               ropolderaero=ropolderaero+
     &			polderaero(i,jj+linf-1)*res(j,jj+linf-1)
            enddo
            rsat(j,i)=ropolderaero/sumpolderaero(i)     
         enddo
      enddo
      goto 2000

c     dais_mod: simulation of the DAIS wavebands


1000  linf=21
      lsup=400
      inmb=382
      onmb=63
      iclm=1
      call sailbrdf
      do j=1,na
         do i=1,inmb
            iwvl(i)=long(i+linf-1)
            idat(1,i)=res(j,i+linf-1)
c	      iwvl(i)=long(i)
c           idat(1,i)=res(j,i)
         enddo
         do i=1,onmb
            owvl(i)=longdais(i)
            owdt(i)=largdais(i)
         enddo
         call gauss(inmb,iwvl,idat,iclm,onmb,owvl,owdt,odat)
         do i=1,onmb
            rsat(j,i)=odat(1,i)
         enddo
      enddo
      goto 2000
      
c     dais_mod: simulation of the ROSIS wavebands


1100  linf=19
      lsup=96
      inmb=78
      onmb=95
      iclm=1
      call sailbrdf
      do j=1,na
         do i=1,inmb
            iwvl(i)=long(i+linf-1)
            idat(1,i)=res(j,i+linf-1)
c     	  iwvl(i)=long(i)
c           idat(1,i)=res(j,i)
         enddo
         do i=1,onmb
            owvl(i)=longrosis(i)
            owdt(i)=largrosis(i)
         enddo
         call gauss(inmb,iwvl,idat,iclm,onmb,owvl,owdt,odat)
         do i=1,onmb
            rsat(j,i)=odat(1,i)
         enddo
      enddo
      goto 2000

c	mivis_mod	

1200	linf=10
      lsup=416
      inmb=407
      onmb=92
      iclm=1
      call sailbrdf
      do j=1,na
         do i=1,inmb
            iwvl(i)=long(i+linf-1)
            idat(1,i)=res(j,i+linf-1)
         enddo
         do i=1,onmb
            owvl(i)=longmivis(i)
            owdt(i)=largmivis(i)
         enddo
         call gauss(inmb,iwvl,idat,iclm,onmb,owvl,owdt,odat)
         do i=1,onmb
            rsat(j,i)=odat(1,i)
         enddo
      enddo
      goto 2000

2000  return
      end

c     ********************************************************************
c     gauss : applies gaussian filters of selected widths and positions
c     ********************************************************************
c
c     Robert O. GREEN
c
c     Jet Propulsion Laboratory
c     California Institute of Technology
c     4800 Oak Grove Drive
c     Pasadena, CA 91109 (USA)
c
c     ********************************************************************

c     iwvl contine i centribanda a 5nm simultai (400+(linf-1)*5, 400+(linf-1)*5+5, .., 400+(linf-1)*5+(lsup-1)*5)
c     idat contine le riflettanze simulate ogni 5nm (R400+(linf-1)*5, R400+(linf-1)*5+5, .., R400+(linf-1)*5+(lsup-1)*5)
c	owvl contine i centribanda del sensore sa simulare
c	owdt contine la FWHM corrispondente ai vari centribanda da simulare
c     onmb è il numerodi bande da simulare
c     inmb è il numero di bande simulate (tra linf e lsup)
      
	subroutine gauss(inmb,iwvl,idat,iclm,onmb,owvl,owdt,odat)

      integer iclm,inmb,onmb
      integer iind(2000),oind(2000)
      double precision h,lwr,upp
      double precision iwvl(421),idat(5,421)
      double precision owvl(224),owdt(224),odat(5,2000)
      double precision sum(2000),flt(2000)

      intrinsic dexp

      kg=0
      do i=1,onmb
c     defines gaussian full width at half maximum
         h=1.665d+0/owdt(i)
c     lower and upper bounds for each filter
         lwr=owvl(i)-(1.5d+0*owdt(i))
         upp=owvl(i)+(1.5d+0*owdt(i))
c     generates the filter shapes
         do j=1,inmb
            if (iwvl(j).ge.lwr.and.iwvl(j).le.upp) then
               kg=kg+1
c     indecies for input and output data
               oind(kg)=i
               iind(kg)=j
c     filter shape
               flt(kg)=dexp(-(owvl(i)-iwvl(j))**2*h*h)
            endif
         enddo
      enddo

      npnt=kg

c     initialized array

      do i=1,onmb
         sum(i)=0.0d+0
      enddo

c     area under filters

      do i=1,npnt
         sum(oind(i))=sum(oind(i))+flt(i)
      enddo

c     normalizes filters

      do i=1,npnt
         if (sum(oind(i)).eq.0.0d+0) sum(oind(i))=1.0d-10
         flt(i)=flt(i)/sum(oind(i))
      enddo

c     applies filters

      do i=1,iclm
         do j=1,npnt
            odat(i,j)=0.0d+0
         enddo
         do j=1,npnt
            odat(i,oind(j))=odat(i,oind(j))+flt(j)*idat(i,iind(j))
         enddo
      enddo

      return
      end

c     ********************************************************************
c     wavesat : widths and positions of the AVIRIS, MODIS, MERIS, TM,
c     HRV, AVHRR, POLDER (Satellite and Airborne), and XSTAR wavebands
c     ********************************************************************

      subroutine wavesat()

      double precision longavi0(224),largavi0(224)
      double precision longmod0(25),largmod0(25)
      double precision longmer0(15),largmer0(15)
      double precision longtm0(6),tm0(6,421)
      double precision longmisr0(4),largmisr0(4)
      double precision longhrv0(3),hrv0(3,421)
      double precision longavhrr0(2),avhrr0(2,421)
      double precision longpoldersat0(4),poldersat0(4,421)
      double precision longpolderaero0(5),polderaero0(5,421)
c     dais_mod
      double precision longdais0(63),largdais0(63)
      double precision longrosis0(95),largrosis0(95)
c	mivis_mod
	double precision longmivis0(92),largmivis0(92)

      double precision longavi(224),largavi(224)
      double precision longmod(25),largmod(25)
      double precision longmer(15),largmer(15)
      double precision longtm(6),tm(6,421)
      double precision longmisr(4),largmisr(4)
      double precision longhrv(3),hrv(3,421)
      double precision longavhrr(2),avhrr(2,421)
      double precision longpoldersat(4),poldersat(4,421)
      double precision longpolderaero(5),polderaero(5,421)
c     dais_mod
      double precision longdais(63),largdais(63)
      double precision longrosis(95),largrosis(95)
c	mivis_mod
	double precision longmivis(92),largmivis(92)
	      
      common /aviris/longavi,largavi
      common /modis/longmod,largmod
      common /meris/longmer,largmer
      common /landsat/longtm,tm
      common /misr/longmisr,largmisr
      common /spot/longhrv,hrv
      common /noaa/longavhrr,avhrr
      common /adeos/longpoldersat,poldersat
      common /arat/longpolderaero,polderaero
c     dais_mod
      common /dais/longdais,largdais 
      common /rosis/longrosis,largrosis      
c	mivis_mod
	common /mivis/longmivis,largmivis
			
      data (longavi0(i),i=1,112)/
     & 400.88,410.69,420.51,430.33,440.16,449.99,459.83,469.67,479.52,
     & 489.37,499.22,509.08,518.95,528.82,538.69,548.57,558.45,568.34,
     & 578.23,588.12,598.02,607.93,617.84,627.75,637.67,647.59,657.52,
     & 667.45,677.39,678.85,687.33,688.49,697.28,698.14,707.23,707.79,
     & 717.44,727.08,736.73,746.38,756.02,765.67,775.32,784.96,794.61,
     & 804.25,813.90,823.54,833.19,842.83,852.48,862.12,871.77,881.41,
     & 891.06,900.70,910.34,919.99,929.63,939.27,948.92,958.56,968.20,
     & 977.85,987.49,997.13,
     & 1006.77,1016.42,1026.06,1035.70,1045.34,1054.98,1064.62,1074.27,
     & 1083.91,1093.55,1103.19,1112.83,1122.47,1132.11,1141.75,1151.39,
     & 1161.03,1170.67,1180.31,1189.94,1199.58,1209.22,1218.86,1228.50,
     & 1238.14,1243.09,1247.78,1253.02,1257.41,1262.94,1267.05,1272.87,
     & 1276.69,1282.80,1286.33,1292.72,1302.65,1312.57,1322.49,1332.41,
     & 1342.32,1352.24,1362.15,1372.06,1381.97,1391.89/
      data (longavi0(i),i=113,224)/
     & 1401.79,1411.70,1421.60,1431.51,1441.41,1451.31,1461.21,1471.11,
     & 1481.01,1490.90,1500.80,1510.69,1520.58,1530.47,1540.36,1550.24,
     & 1560.13,1570.01,1579.90,1589.78,1599.66,1609.53,1619.41,1629.29,
     & 1639.16,1649.03,1658.90,1668.77,1678.64,1688.51,1698.37,1708.23,
     & 1718.10,1727.96,1737.82,1747.67,1757.53,1767.39,1777.24,1787.09,
     & 1796.94,1806.79,1816.64,1826.48,1829.60,1836.33,1839.65,1846.17,
     & 1849.70,1856.02,1859.74,1865.85,1869.78,1879.81,1889.84,1899.86,
     & 1909.89,1919.90,1929.92,1939.93,1949.93,1959.94,1969.93,1979.93,
     & 1989.92,1999.90,2009.89,2019.86,2029.84,2039.81,2049.77,2059.74,
     & 2069.70,2079.65,2089.60,2099.55,2109.49,2119.43,2129.36,2139.29,
     & 2149.22,2159.14,2169.06,2178.98,2188.89,2198.79,2208.70,2218.60,
     & 2228.49,2238.38,2248.27,2258.15,2268.03,2277.91,2287.78,2297.65,
     & 2307.51,2317.37,2327.23,2337.08,2346.93,2356.77,2366.61,2376.45,
     & 2386.28,2396.10,2405.93,2415.75,2425.56,2435.38,2445.19,2454.99/
      data (largavi0(i),i=1,112)/
     & 10.57,10.59,10.62,10.64,10.66,10.67,10.68,10.69,10.69,10.69,
     & 10.68,10.67,10.66,10.64,10.62,10.60,10.57,10.54,10.50,10.47,
     & 10.42,10.38,10.33,10.27,10.22,10.16,10.09,10.02,9.95,9.51,9.87,
     & 9.51,9.79,9.51,9.71,9.51,9.50,9.50,9.50,9.50,9.50,9.50,9.50,9.50,
     & 9.50,9.50,9.50,9.50,9.50,9.51,9.51,9.51,9.51,9.51,9.51,9.52,9.52,
     & 9.52,9.52,9.52,9.53,9.53,9.53,9.54,9.54,9.54,9.55,9.55,9.56,9.56,
     & 9.57,9.57,9.58,9.58,9.59,9.59,9.60,9.60,9.61,9.61,9.62,9.63,9.63,
     & 9.64,9.65,9.65,9.66,9.67,9.68,9.68,9.69,9.66,9.70,9.70,9.71,9.75,
     & 9.71,9.79,9.72,9.83,9.73,9.87,9.91,9.95,9.99,10.02,10.05,10.09,
     & 10.12,10.15,10.17,10.20/
      data (largavi0(i),i=113,224)/
     & 10.23,10.25,10.27,10.29,10.31,10.33,10.35,10.36,10.38,10.39,
     & 10.40,10.41,10.42,10.43,10.43,10.44,10.44,10.44,10.44,10.44,
     & 10.44,10.43,10.43,10.42,10.41,10.41,10.39,10.38,10.37,10.35,
     & 10.34,10.32,10.30,10.28,10.26,10.23,10.21,10.18,10.16,10.13,
     & 10.10,10.06,10.03,10.00,11.32,9.96,11.39,9.92,11.45,9.89,11.52,
     & 9.85,11.58,11.64,11.70,11.76,11.81,11.86,11.91,11.96,12.01,12.05,
     & 12.09,12.13,12.17,12.20,12.23,12.26,12.29,12.32,12.34,12.36,
     & 12.38,12.40,12.41,12.43,12.44,12.44,12.45,12.45,12.45,12.45,
     & 12.45,12.45,12.44,12.43,12.42,12.40,12.39,12.37,12.35,12.32,
     & 12.30,12.27,12.24,12.21,12.18,12.14,12.10,12.06,12.02,11.98,
     & 11.93,11.88,11.83,11.77,11.72,11.66,11.60,11.54,11.47,11.41/

      data (longmod0(i),i=1,25)/
     & 547,646.5,707,745,786,856.7,875,910,945,1623,
     & 1680,1730,1780,1830,
     & 1880,1930,1980,2030,2080,2142,2180,2230,2280,2330,2380/
      data (largmod0(i),i=1,25)/
     & 43,41.8,42,40,40,39.4,41,31,43,57,50,50,50,50,50,50,50,
     & 50,50,47,50,
     & 50,50,50,50/

      data (longmer0(i),i=1,15)/
     & 410,445,490,520,565,620,665,682.5,710,755,760.5,765,880,900,
     & 1022.5/
      data (largmer0(i),i=1,15)/
     & 10,10,10,10,10,10,10,5,10,10,2.5,2.5,10,10,25/

      data (longtm0(i),i=1,6)/
     & 500,595,677.5,800,1707.5,2187.5/
      data (tm0(1,i),i=1,421)/7*0.0,
     & 0.018,0.041,0.070,0.427,0.690,0.728,0.796,0.827,0.862,0.914,
     & 0.935,0.959,0.985,1.000,0.974,0.830,0.719,0.276,0.076,0.041,
     & 0.022,0.012,0.007,391*0.0/
      data (tm0(2,i),i=1,421)/21*0.0,
     & 0.011,0.024,0.041,0.178,0.396,0.597,0.677,0.745,0.797,0.847,
     & 0.873,0.897,0.908,0.909,0.909,0.917,0.959,0.992,1.000,0.939,
     & 0.781,0.459,0.199,0.094,0.048,0.032,0.020,0.010,0.003,371*0.0/
      data (tm0(3,i),i=1,421)/40*0,
     & 0.008,0.021,0.035,0.109,0.298,0.471,0.580,0.751,0.823,0.876,
     & 0.901,0.906,0.906,0.934,0.969,1.000,1.000,0.960,0.813,0.434,
     & 0.154,0.072,0.048,0.031,0.021,0.014,0.008,0.005,0.002,352*0.0/
      data (tm0(4,i),i=1,421)/67*0.0,
     & 0.004,0.010,0.010,0.017,0.035,0.069,0.134,0.275,0.452,0.659,
     & 0.843,0.942,0.981,1.000,0.990,0.979,0.956,0.938,0.917,0.917,
     & 0.919,0.921,0.924,0.924,0.907,0.886,0.882,0.884,0.882,0.861,
     & 0.826,0.786,0.765,0.717,0.528,0.230,0.086,0.034,0.016,0.007,
     & 0.004,0.002,312*0.0/
      data (tm0(5,i),i=1,421)/224*0.0,
     & 0.002,0.006,0.013,0.016,0.029,0.058,0.107,0.210,0.308,0.476,
     & 0.615,0.757,0.853,0.917,0.951,0.947,0.942,0.946,0.953,0.964,
     & 0.977,0.991,0.987,0.979,0.976,0.970,0.970,0.978,0.982,0.988,
     & 0.988,0.988,0.984,0.979,0.979,0.987,1.000,1.000,1.000,1.000,
     & 0.994,0.982,0.974,0.965,0.954,0.954,0.958,0.967,0.954,0.925,
     & 0.870,0.731,0.593,0.436,0.300,0.216,0.137,0.105,0.065,0.056,
     & 0.038,0.027,0.018,0.013,0.013,0.009,0.009,0.006,0.004,0.001,
     & 127*0.0/
      data (tm0(6,i),i=1,421)/316*0.0,
     & 0.002,0.005,0.005,0.005,0.007,0.009,0.012,0.016,0.018,0.024,
     & 0.033,0.040,0.049,0.061,0.070,0.086,0.102,0.137,0.170,0.207,
     & 0.264,0.338,0.418,0.478,0.564,0.648,0.716,0.806,0.871,0.915,
     & 0.942,0.949,0.949,0.949,0.949,0.949,0.949,0.952,0.966,0.938,
     & 0.997,0.997,0.997,0.997,0.994,0.994,0.989,0.989,0.985,0.968,
     & 0.955,0.939,0.937,0.925,0.918,0.915,0.912,0.912,0.903,0.893,
     & 0.878,0.839,0.808,0.762,0.748,0.725,0.760,0.786,0.821,0.869,
     & 0.869,0.823,0.664,0.556,0.415,0.325,0.181,0.149,0.105,0.069,
     & 0.027,0.017,0.012,0.006,0.001,20*0.0/

      data (longmisr0(i),i=1,4)/
     & 440,550,670,860/
      data (largmisr0(i),i=1,4)/
     & 35,20,15,40/

      data (longhrv0(i),i=1,3)/
     & 565,650,830/
      data (hrv0(1,i),i=1,421)/14*0.0,
     & 0.01,0.01,0.02,0.03,0.09,0.15,0.25,0.45,0.58,0.73,0.77,0.82,
     & 0.85,0.88,0.91,0.94,0.96,0.98,0.99,1.00,0.97,0.93,0.84,0.74,
     & 0.58,0.42,0.35,0.28,0.22,0.17,0.13,0.10,0.07,0.04,0.04,0.03,
     & 0.03,0.03,0.03,0.03,0.02,0.02,0.02,0.02,0.02,0.01,0.01,0.01,
     & 359*0.0/
      data (hrv0(2,i),i=1,421)/39*0.0,
     & 0.01,0.01,0.11,0.22,0.42,0.63,0.75,0.88,0.92,0.97,0.99,1.00,
     & 0.97,0.94,0.82,0.69,0.56,0.43,0.35,0.26,0.22,0.18,0.14,0.11,
     & 0.08,0.05,0.04,0.03,0.03,0.02,0.02,0.01,0.01,0.01,348*0.0/
      data (hrv0(3,i),i=1,421)/69*0.0,
     & 0.01,0.01,0.02,0.03,0.06,0.10,0.21,0.32,0.45,0.57,0.70,0.83,
     & 0.89,0.96,0.98,0.99,0.99,1.00,0.98,0.98,0.97,0.96,0.94,0.92,
     & 0.90,0.87,0.84,0.67,0.59,0.45,0.32,0.24,0.15,0.09,0.03,0.02,
     & 0.01,0.01,0.01,0.01,0.01,311*0.0/

      data (longavhrr0(i),i=1,2)/
     & 680,760/
      data (avhrr0(1,i),i=1,421)/29*0.0,
     & 0.0052,0.0229,0.0875,0.1542,0.3292,0.4667,0.5698,0.6333,0.7125,
     & 0.7625,0.7844,0.8000,0.7802,0.7729,0.7875,0.8000,0.8531,0.8834,
     & 0.8886,0.8833,0.8490,0.8229,0.8000,0.7875,0.8355,0.8813,0.9521,
     & 1.0000,0.8917,0.7646,0.5719,0.3875,0.2864,0.1896,0.1302,0.0833,
     & 0.0625,0.0438,0.0344,0.0250,0.0198,0.0167,0.0136,0.0125,0.0115,
     & 0.0083,0.0052,0.0042,344*0.0/
      data (avhrr0(2,i),i=1,421)/58*0.0,
     & 0.0051,0.0257,0.0462,0.1513,0.2564,0.4282,0.6000,0.7256,0.8513,
     & 0.8975,0.9436,0.9641,0.9846,0.9923,1.0000,0.9795,0.9590,0.9410,
     & 0.9231,0.9000,0.8769,0.8616,0.8462,0.8487,0.8513,0.8539,0.8564,
     & 0.8616,0.8667,0.8590,0.8513,0.8359,0.8205,0.8103,0.8000,0.8000,
     & 0.8000,0.7898,0.7795,0.7898,0.8000,0.8077,0.8154,0.7949,0.7744,
     & 0.7641,0.7538,0.7153,0.6769,0.6564,0.6359,0.6308,0.6256,0.6436,
     & 0.6615,0.6769,0.6923,0.6462,0.6000,0.4872,0.3744,0.2770,0.1795,
     & 0.1334,0.0872,0.0641,0.0410,0.0333,0.0256,0.0205,0.0154,0.0129,
     & 0.0103,0.0077,0.0051,288*0.0/

      data(longpoldersat0(i),i=1,4)/443,670,765,865/   
      data (poldersat0(1,i),i=1,421)/3*0.0,
     & 0.0050,0.0040,0.0120,0.1230,0.6220,0.9090,0.9980,0.9020,0.3830,
     & 0.0940,0.0290,0.0110,
     & 406*0.0/ 	
      data (poldersat0(2,i),i=1,421)/47*0.0,
     & 0.0013,0.0033,0.0163,0.0880,0.5103,0.9870,0.9573,0.9140,0.5823,
     & 0.1203,0.0257,0.0143,
     & 362*0.0/	
      data (poldersat0(3,i),i=1,421)/61*0.0,
     & 0.0010,0.0010,0.0040,0.0090,0.0200,0.0410,0.0950,0.2380,0.5180,
     & 0.7820,0.8860,0.9200,0.9500,0.9900,0.9690,0.6850,0.2900,0.1000,
     & 0.0360,0.0140,0.0090,0.0050,0.0040,0.0030,
     & 336*0.0/
      data (poldersat0(4,i),i=1,421)/80*0.0,
     & 0.0010,0.0000,0.0010,0.0013,0.0090,0.0343,0.1070,0.3377,0.7557,
     & 0.9873,0.9730,0.9350,0.9163,0.9007,0.8047,0.5053,0.2133,0.0757,
     & 0.0253,0.0087,0.0040,0.0040,0.0003,0.0013,
     & 317*0.0/

      data(longpolderaero0(i),i=1,5)/443,550,670,864,910/
      data (polderaero0(1,i),i=1,421)/4*0.0,
     & 0.0036,0.0334,0.3684,0.5517,0.5821,0.5364,0.1361,0.0206,0.0040,
     & 0.0007,
     & 407*0.0/
      data (polderaero0(2,i),i=1,421)/27*0.0,
     & 0.0006,0.0041,0.1456,0.5933,0.4074,0.0113,0.0007,
     & 387*0.0/
      data (polderaero0(3,i),i=1,421)/51*0.0,
     & 0.0000,0.0050,0.0651,0.6052,0.5700,0.0489,0.0084,0.0052,
     & 362*0.0/
      data (polderaero0(4,i),i=1,421)/82*0.0,
     & 0.0020,0.0045,0.0092,0.0195,0.0449,0.1075,0.2451,0.4118,
     & 0.4882,0.4941,0.4700,0.4445,0.4042,0.3355,0.2206,0.1139,
     & 0.0505,0.0219,0.0109,0.0052,0.0028,0.0009,
     & 317*0.0/
      data (polderaero0(5,i),i=1,421)/98*0.0,
     & 0.0033,0.0102,0.0715,0.5572,0.6462,0.6617,0.3337,0.0268,
     & 0.0041,0.0023,
     & 313*0.0/

c     dais_mod
      data (longdais0(i),i=1,63)/
     & 502,517,535,554,571,589,607,625,641,
     & 659,678,695,711,729,747,766,783,802,
     & 819,837,854,873,890,906,923,939,955,
     & 972,990,1005,1020,1033,1542,1573,1606,1639,
     & 1671,1700,1729,1755,1995,2010,
     & 2026,2042,2059,2075,2092,2108,2122,2137,2151,
     & 2166,2179,2193,2207,2224,2236,2252,2264,2279,
     & 2291,2304,2317/ 
      data (largdais0(i),i=1,63)/
     & 23,21,20,18,22,20,20,22,22,24,25,25,
     & 28,27,28,29,29,27,30,27,28,28,27,31,
     & 28,30,28,28,39,39,37,33,41,47,47,50,
     & 49,47,46,40,50,57,60,61,59,
     & 56,54,52,49,47,45,42,37,33,32,28,25,
     & 23,21,20,18,17,17/
     
      data (longrosis0(i),i=1,95)/
     &494.9,498.9,502.8,506.9,510.9,514.8,518.8,
     &522.8,526.8,530.8,534.9,538.8,542.8,546.8,
     &550.8,554.8,558.8,562.8,566.8,570.8,574.8,
     &578.8,582.8,586.8,590.8,594.9,598.8,602.8,
     &606.8,610.8,614.8,618.8,622.9,626.8,630.8,
     &634.8,638.8,642.8,646.8,650.8,654.8,658.8,
     &662.8,666.8,670.8,674.8,678.8,682.9,686.8,
     &690.8,694.8,698.8,702.8,706.8,710.9,714.8,
     &718.8,722.8,726.8,730.8,734.8,738.8,742.8,
     &746.8,750.8,754.8,758.8,762.8,766.8,770.9,
     &774.8,778.8,782.8,786.8,790.8,794.8,798.8,
     &802.8,806.8,810.8,814.8,818.8,822.8,826.8,
     &830.9,834.8,838.8,842.8,846.8,850.8,854.8,
     &858.9,862.8,866.8,870.8/ 
      data (largrosis0(i),i=1,95)/
     &7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,
     &7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,
     &7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,
     &7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,
     &7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,
     &7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,
     &7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,
     &7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,
     &7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,     	       
     &7.5,7.5,7.5,7.5,7.5/

c	mivis_mod
	data(longmivis0(i), i=1,92)/
     &446.0,465.0,484.5,505.0,525.0,544.0,563.5,583.5,603.0,622.5,
     &642.0,661.0,680.5,700.0,719.5,738.5,758.0,776.5,796.5,816.0,
     &1155.5,1214.5,1266.5,1318.0,1372.5,1429,1480,1552.5,1994.5,1998.5,
     &2012,2017.5,2029.5,2032,2045,2048,2056.5,2065,2079,2081,
     &2095,2098,2112,2114.5,2128,2131,2144,2147.5,2161,2163.5,
     &2176.5,2179.5,2193,2195.5,2209,2211.5,2224,2226.5,2239.5,2242.5,
     &2256,2258.5,2266,2273.5,2286.5,2288.5,2301.5,2303.5,2311.5,2319.5,
     &2331,2333.5,2345.5,2347.5,2360,2362.5,2374,2376.5,2388,2390.5,
     &2403,2404.5,2417,2418.5,2431,2432.5,2444.5,2446.5,2458,2462,
     &2472,2473.5/
	data(largmivis0(i), i=1,92)/
     &22,22,21,22,22,22,21,23,22,21,
     &22,22,21,22,21,21,20,21,21,22,
     &81,53,53,54,55,52,52,47,17,11,
     &18,11,11,10,16,8,9,8,16,8,
     &16,8,16,9,16,8,16,9,16,9,
     &17,9,16,9,16,9,16,9,15,9,
     &16,9,8,9,15,9,15,9,9,9,
     &14,9,15,9,14,9,14,9,14,9,
     &14,9,14,9,14,9,13,9,14,6,
     &14,9/

      do i=1,224
         longavi(i)=longavi0(i)
         largavi(i)=largavi0(i)
      enddo
      do i=1,25
         longmod(i)=longmod0(i)
         largmod(i)=largmod0(i)
      enddo
      do i=1,15
         longmer(i)=longmer0(i)
         largmer(i)=largmer0(i)
      enddo
      do i=1,4
         longmisr(i)=longmisr0(i)
         largmisr(i)=largmisr0(i)
      enddo
      
c     dais_mod
      do i=1,63
         longdais(i)=longdais0(i)
         largdais(i)=largdais0(i)
      enddo
     
      do i=1,95
         longrosis(i)=longrosis0(i)
         largrosis(i)=largrosis0(i)
      enddo
c	mivis_mod
	do i=1,92
         longmivis(i)=longmivis0(i)
         largmivis(i)=largmivis0(i)
      enddo
      
      do i=1,421
         do j=1,6
            longtm(j)=longtm0(j)
            tm(j,i)=tm0(j,i)
         enddo
         do j=1,3
            longhrv(j)=longhrv0(j)
            hrv(j,i)=hrv0(j,i)
         enddo
         do j=1,2
            longavhrr(j)=longavhrr0(j)
            avhrr(j,i)=avhrr0(j,i)
         enddo
         do j=1,4
            longpoldersat(j)=longpoldersat0(j)
            poldersat(j,i)=poldersat0(j,i)
         enddo
	   do j=1,5
            longpolderaero(j)=longpolderaero0(j)
            polderaero(j,i)=polderaero0(j,i)
         enddo
      enddo

      return
      end

c     ********************************************************************
