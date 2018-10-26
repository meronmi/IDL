    SUBROUTINE ellipse(mli,ff)
    !mli is mean leaf inclination (0 -> horizontal leaves, 90 -> vertical leaves)
    
    USE ANGLE
    REAL(KIND=8) :: mli,ff(13),ffcalc(90)
    REAL(KIND=8) :: excent
    REAL(KIND=8) :: som,gd,theta,aex
    INTEGER :: i, ncalc
    !DATA (li(i),i=1,13)/5.,15.,25.,35.,45.,55.,65.,75.,81.,83.,85.,87.,89./
    !REAL(KIND=8) :: a(13)
    !DATA (a(i),i=1,13)/10.,20.0,30.0,40.0,50.0,60.0,70.0,80.0,82.0,84.0,86.0,88.0,90.0/ !in accordo a Ladgen  
    !intrinsic dfloat,dcos,dsin

    !common /cst/pi,rd,ff
    ncalc=90
    som=0.d+0
    excent=(mli*rd/9.65d+0)**(-1.d+0/1.65d+0)-3.d+0
!     initialization of 9 angle classes

    do i=1,ncalc
        aex=excent
        !theta=5.d+0+10.d+0*dfloat(i-1)
        theta=i
        theta=theta*rd
        gd=aex+1.744d+0*(aex+1.182d+0)**(-0.733d+0)
	    ffcalc(i)=2*aex**3*dsin(theta)/(gd*(dcos(theta)**2+aex**2*dsin(theta)**2)**2)
        som=som+ffcalc(i)
    enddo
    do i=1,ncalc
         ffcalc(i)=ffcalc(i)/som
    enddo
    
    ff(1)= SUM(ffcalc(1:9))
    ff(2)= SUM(ffcalc(10:19))
    ff(3)= SUM(ffcalc(22:29))
    ff(4)= SUM(ffcalc(30:39))
    ff(5)= SUM(ffcalc(40:49))
    ff(6)= SUM(ffcalc(50:59))
    ff(7)= SUM(ffcalc(60:69))
    ff(8)= SUM(ffcalc(70:79))
    ff(9)= SUM(ffcalc(81:82))
    ff(10)=SUM(ffcalc(83:84))
    ff(11)=SUM(ffcalc(85:86))
    ff(12)=SUM(ffcalc(87:88))
    ff(13)=SUM(ffcalc(89:90))
    
    return
    end