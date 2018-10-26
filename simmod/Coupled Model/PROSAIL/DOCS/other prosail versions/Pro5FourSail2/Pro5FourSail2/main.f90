	PROGRAM FourSAIL2vector
    
	USE ANGLE
	USE datadim
	USE output_PROSPECT
	USE staticvar
	USE oldpar
	USE LUT_RANGES
	USE VECTOR_4SAIL2
	USE IFPORT
	
	IMPLICIT NONE

	!time check +
	REAL(KIND=8) ::  START_TIME, STOP_TIME !, DCLOCK
	!EXTERNAL DCLOCK
	!time -
    INTEGER :: narg
    INTEGER (KIND=8) :: numrow
	character*120 Rsoil_filename,skylarr_filename,lutin_filename,singleexe_filename
	character*120 outputname,outLUTname, dummychar
	CHARACTER*105000 luthdr
	LOGICAL doLUT, file_exists
	!REAL(4) :: tt, TA(2)
    
	REAL(KIND=8) :: rsosoil(nw), rdosoil(nw), rsdsoil(nw), rddsoil(nw)
	REAL(KIND=8),ALLOCATABLE,SAVE :: rsot(:),rdot(:),rsdt(:),rddt(:),alfast(:),alfadt(:)
	REAL(KIND=8) :: wl(nw), skylarr(nw), rsoil(2,nw), hcrf(nw)
	! SAIL
	REAL(KIND=8) :: lai,mli,hot,fB,diss,Cv,zeta,tto,tts,psi,psoil   !lidfa,lidfb replaced by mli ellipsoidal distrib
	! LEAF BIOCHEMISTRY
    REAL(KIND=8) :: N_g,Cab_g,Car_g,Cbrown_g,Cw_g,Cm_g
    REAL(KIND=8) :: N_b,Cab_b,Car_b,Cbrown_b,Cw_b,Cm_b
    
    REAL(KIND=8) :: delta
    ! SAIL
	ALLOCATE (lidf(13),rsot(nw),rdot(nw),rsdt(nw),rddt(nw),alfast(nw),alfadt(nw))
	!M+
	!include 'SLC.h'
	ALLOCATE (amsig(nw),apsig(nw),att(nw),alfas(nw),alfad(nw))
	ALLOCATE (cks(nw),cko(nw))
	ALLOCATE (denom(nw),dks(nw),dko(nw),dns(nw),dno(nw))
	ALLOCATE (e1(nw),e2(nw))
	ALLOCATE (g1(nw),g2(nw),ho(nw))
	ALLOCATE (j1ks(nw),j2ks(nw),j1ko(nw),j2ko(nw),j3(nw))
	ALLOCATE (m(nw),m2(nw))
	ALLOCATE (Ps(nw),Qs(nw),Pv(nw),Qv(nw))
	ALLOCATE (re(nw),rn(nw),rinf(nw),rinf2(nw))
	ALLOCATE (rdd(nw),rsd(nw),rdo(nw),rtp(nw),rtm(nw))
	ALLOCATE (rho1(nw),rho2(nw),rsod(nw))
	ALLOCATE (rddb(nw),rsdb(nw),rdob(nw),rsodb(nw))
	ALLOCATE (rsodt(nw),rsost(nw),rddt_t(nw),rddt_b(nw),rddcb(nw),rddct(nw))
	ALLOCATE (rsdc(nw),rdoc(nw),rsoc(nw))
	ALLOCATE (sb(nw),sf(nw),sigb(nw),sigf(nw))
	ALLOCATE (tau1(nw), tau2(nw))
	ALLOCATE (tdd(nw),tsd(nw),tdo(nw),tup(nw),tdn(nw),tddt(nw),tsdt(nw))
	ALLOCATE (tddb(nw),tsdb(nw),tdob(nw),tdot(nw),toob(nw),tssb(nw),tsst(nw),toot(nw))
	ALLOCATE (tssc(nw),tooc(nw),tssooc(nw),tddc(nw),tsdc(nw),tdoc(nw))
	ALLOCATE (T1(nw),T2(nw),T3(nw),Tv1(nw),Tv2(nw))
	ALLOCATE (vb(nw),vf(nw),w1(nw),w2(nw))
	ALLOCATE (z(nw))
    !M-
	! PROSPECT
	ALLOCATE (LRT_g(nw,3),LRT_b(nw,3),rg(nw),tg(nw),rb(nw),tb(nw))
        
    !SELECTABLE FILES (for both single run and LUT)
    !INPUT
    Rsoil_filename='.\data_in\2Rsoil.txt'         !reflectance of dry and wet soils
    skylarr_filename='.\data_in\skylarr.txt'      !diffuse sky fraction (Esky/Etot)
    lutin_filename='.\data_in\lutin.txt'
    singleexe_filename='.\data_in\sein.txt'
    !OUTPUT
    outputname='.\data_out\out_sail.txt'
    outLUTname='.\data_out\out_LUT.txt'
    
    !SELECT single run (doLUT=.false. or lut generation doLUT=.true.
    !TYPE OF EXECUTION
    doLUT=.true.
    
    
    
    pi=datan(1.d0)*4.
	rd=pi/180. 
    
    !read skyl file
    inquire (file=skylarr_filename,exist=file_exists)
	if (.not.file_exists) then
		print '("file ",a," does not exist. Enter to stop")',trim(skylarr_filename)
		read (*,'(a)') dummychar
		stop
	end if
    OPEN (unit=11,file=skylarr_filename)
   	DO i=1,2101
		READ(11,*) wl(i),skylarr(i)
	ENDDO
    CLOSE(11)
    !read rsoils file (2 reflectances, dry and wet soil reflectance)
    inquire (file=Rsoil_filename,exist=file_exists)
	if (.not.file_exists) then
		print '("file ",a," does not exist. Enter to stop")',trim(Rsoil_filename)
		read (*,'(a)') dummychar
		stop
	end if
    OPEN (unit=11,file=Rsoil_filename)
   	DO i=1,2101
		READ(11,*) wl(i), rsoil(1,i), rsoil(2,i)
	ENDDO
    CLOSE(11)
    
    
    IF (doLUT) THEN
        !READ the LUT_input_file
        inquire (file=lutin_filename,exist=file_exists)
	    if (.not.file_exists) then
		    print '("file ",a," does not exist. Enter to stop")',trim(lutin_filename)
		    read (*,'(a)') dummychar
		    stop
	    end if
        OPEN (unit=11,file=lutin_filename)
            READ(11,*) Lut_N_g%min, Lut_N_g%max, Lut_N_g%n
            READ(11,*) Lut_Cab_g%min, Lut_Cab_g%max, Lut_Cab_g%n
            READ(11,*) Lut_Car_g%min, Lut_Car_g%max, Lut_Car_g%n
            READ(11,*) Lut_Cbrown_g%min, Lut_Cbrown_g%max, Lut_Cbrown_g%n
            READ(11,*) Lut_Cw_g%min, Lut_Cw_g%max, Lut_Cw_g%n
            READ(11,*) Lut_Cm_g%min, Lut_Cm_g%max, Lut_Cm_g%n
            
            READ(11,*) Lut_N_b%min, Lut_N_b%max, Lut_N_b%n
            READ(11,*) Lut_Cab_b%min, Lut_Cab_b%max, Lut_Cab_b%n
            READ(11,*) Lut_Car_b%min, Lut_Car_b%max, Lut_Car_b%n
            READ(11,*) Lut_Cbrown_b%min, Lut_Cbrown_b%max, Lut_Cbrown_b%n
            READ(11,*) Lut_Cw_b%min, Lut_Cw_b%max, Lut_Cw_b%n
            READ(11,*) Lut_Cm_b%min, Lut_Cm_b%max, Lut_Cm_b%n
            
            READ(11,*) Lut_lai%min, Lut_lai%max, Lut_lai%n
            READ(11,*) Lut_mli%min, Lut_mli%max, Lut_mli%n
            READ(11,*) Lut_hot%min, Lut_hot%max, Lut_hot%n
            READ(11,*) Lut_fB%min, Lut_fB%max, Lut_fB%n
            READ(11,*) Lut_diss%min, Lut_diss%max, Lut_diss%n
            READ(11,*) Lut_Cv%min, Lut_Cv%max, Lut_Cv%n
            READ(11,*) Lut_zeta%min, Lut_zeta%max, Lut_zeta%n
            READ(11,*) Lut_psoil%min, Lut_psoil%max, Lut_psoil%n
            READ(11,*) Lut_tto%min, Lut_tto%max, Lut_tto%n
            READ(11,*) Lut_tts%min, Lut_tts%max, Lut_tts%n
            READ(11,*) Lut_psi%min, Lut_psi%max, Lut_psi%n
        CLOSE(11)
        
        numrow=Lut_N_g%n*Lut_Cab_g%n*Lut_Car_g%n*Lut_Cbrown_g%n*Lut_Cw_g%n*Lut_Cm_g%n*&
        Lut_N_b%n*Lut_Cab_b%n*Lut_Car_b%n*Lut_Cbrown_b%n*Lut_Cw_b%n*Lut_Cm_b%n*&
        Lut_lai%n*Lut_mli%n*Lut_hot%n*Lut_fB%n*Lut_diss%n*Lut_Cv%n*Lut_zeta%n*&
        Lut_psoil%n*Lut_tto%n*Lut_tts%n*Lut_psi%n
        print '("TOTAL combination number=",i)',numrow
        
        
		!read (*,'(a)') dummychar
		luthdr=''
		DO i=1,nw
		    Write(dummychar, '(i4)' )  i+399
		    luthdr=TRIM(luthdr) // 'rso' // TRIM(ADJUSTL(dummychar)) // CHAR(9)
		ENDDO
		DO i=1,nw
		    Write(dummychar, '(i4)' )  i+399
		    luthdr=TRIM(luthdr) //'rdo' // TRIM(ADJUSTL(dummychar)) // CHAR(9)
		ENDDO
        open (2, file=outLUTname)
        open (3, file=TRIM(outLUTname) // '.bin', FORM='BINARY')
        write (3),SNGL(numrow)
        write (2,200) 'N_g','Cab_g','Car_g','Cbrown_g','Cw_g','Cm_g','N_b','Cab_b','Car_b','Cbrown_b','Cw_b','Cm_b',&                
                      'lai','fb','diss','mli','hot','Cv','zeta','psoil',&
                      'tts','tto','psi',TRIM(luthdr)
        START_TIME = DCLOCK()              
        !LUT loops
        !PROSPECT: green leaf*****************************************************************
        DO iNg=1,Lut_N_g%n  !iNg
         IF (Lut_N_g%n .eq. 1) THEN ; delta=0.0 
         ELSE ; delta = ((Lut_N_g%max-Lut_N_g%min)/(Lut_N_g%n-1.0)) ; ENDIF
         N_g=Lut_N_g%min+(iNg-1)*delta
         DO iCABg=1,Lut_Cab_g%n !iCABg
          IF (Lut_Cab_g%n .eq. 1) THEN ; delta=0.0 
          ELSE ; delta = ((Lut_Cab_g%max-Lut_Cab_g%min)/(Lut_Cab_g%n-1.0)) ; ENDIF
          Cab_g=Lut_Cab_g%min+(iCABg-1)*delta
          DO iCARg=1,Lut_Car_g%n !iCARg
           IF (Lut_Car_g%n .eq. 1) THEN ; delta=0.0 
           ELSE ; delta = ((Lut_Car_g%max-Lut_Car_g%min)/(Lut_Car_g%n-1.0)) ; ENDIF
           Car_g=Lut_Car_g%min+(iCARg-1)*delta
           DO iCBROWNg=1,Lut_Cbrown_g%n !iCBROWNg
            IF (Lut_Cbrown_g%n .eq. 1) THEN ; delta=0.0 
            ELSE ; delta = ((Lut_Cbrown_g%max-Lut_Cbrown_g%min)/(Lut_Cbrown_g%n-1.0)) ; ENDIF
            Cbrown_g=Lut_Cbrown_g%min+(iCBROWNg-1)*delta
            DO iCWg=1,Lut_Cw_g%n !iCWg
             IF (Lut_Cw_g%n .eq. 1) THEN ; delta=0.0 
             ELSE ; delta = ((Lut_Cw_g%max-Lut_Cw_g%min)/(Lut_Cw_g%n-1.0)) ; ENDIF
             Cw_g=Lut_Cw_g%min+(iCWg-1)*delta
             DO iCMg=1,Lut_Cm_g%n !iCMg
              IF (Lut_Cm_g%n .eq. 1) THEN ; delta=0.0 
              ELSE ; delta = ((Lut_Cm_g%max-Lut_Cm_g%min)/(Lut_Cm_g%n-1.0)) ; ENDIF
              Cm_g=Lut_Cm_g%min+(iCMg-1)*delta
              !PROSPECT: brown leaf*****************************************************************
              DO iNb=1,Lut_N_b%n  !iNg
               IF (Lut_N_b%n .eq. 1) THEN ; delta=0.0 
               ELSE ; delta = ((Lut_N_b%max-Lut_N_b%min)/(Lut_N_b%n-1.0)) ; ENDIF
               N_b=Lut_N_b%min+(iNb-1)*delta
               DO iCABb=1,Lut_Cab_b%n !iCABg
                IF (Lut_Cab_b%n .eq. 1) THEN ; delta=0.0 
                ELSE ; delta = ((Lut_Cab_b%max-Lut_Cab_b%min)/(Lut_Cab_b%n-1.0)) ; ENDIF
                Cab_b=Lut_Cab_b%min+(iCABb-1)*delta
                DO iCARb=1,Lut_Car_b%n !iCARg
                 IF (Lut_Car_b%n .eq. 1) THEN ; delta=0.0 
                 ELSE ; delta = ((Lut_Car_b%max-Lut_Car_b%min)/(Lut_Car_b%n-1.0)) ; ENDIF
                 Car_b=Lut_Car_b%min+(iCARb-1)*delta
                 DO iCBROWNb=1,Lut_Cbrown_b%n !iCBROWNg
                  IF (Lut_Cbrown_b%n .eq. 1) THEN ; delta=0.0 
                  ELSE ; delta = ((Lut_Cbrown_b%max-Lut_Cbrown_b%min)/(Lut_Cbrown_b%n-1.0)) ; ENDIF
                  Cbrown_b=Lut_Cbrown_b%min+(iCBROWNb-1)*delta
                  DO iCWb=1,Lut_Cw_b%n !iCWg
                   IF (Lut_Cw_b%n .eq. 1) THEN ; delta=0.0 
                   ELSE ; delta = ((Lut_Cw_b%max-Lut_Cw_b%min)/(Lut_Cw_b%n-1.0)) ; ENDIF
                   Cw_b=Lut_Cw_b%min+(iCWb-1)*delta
                   DO iCMb=1,Lut_Cm_b%n !iCMg
                    IF (Lut_Cm_b%n .eq. 1) THEN ; delta=0.0 
                    ELSE ; delta = ((Lut_Cm_b%max-Lut_Cm_b%min)/(Lut_Cm_b%n-1.0)) ; ENDIF
                    Cm_b=Lut_Cm_b%min+(iCMb-1)*delta 
                    !SAIL *****************************************************************
                    DO iLAI=1,Lut_lai%n !iLAI
                     IF (Lut_lai%n .eq. 1) THEN ; delta=0.0 
                     ELSE ; delta = ((Lut_lai%max-Lut_lai%min)/(Lut_lai%n-1.0)) ; ENDIF 
                     lai=Lut_lai%min+(iLAI-1)*delta  
                     DO iMLI=1,Lut_mli%n !iLAI
                      IF (Lut_mli%n .eq. 1) THEN ; delta=0.0 
                      ELSE ; delta = ((Lut_mli%max-Lut_mli%min)/(Lut_mli%n-1.0)) ; ENDIF 
                      mli=Lut_mli%min+(iMLI-1)*delta  
                      DO iHOT=1,Lut_hot%n !iHOT
                       IF (Lut_hot%n .eq. 1) THEN ; delta=0.0 
                       ELSE ; delta = ((Lut_hot%max-Lut_hot%min)/(Lut_hot%n-1.0)) ; ENDIF 
                       hot=Lut_hot%min+(iHOT-1)*delta  
                       DO iFB=1,Lut_fB%n !iFB
                        IF (Lut_fB%n .eq. 1) THEN ; delta=0.0 
                        ELSE ; delta = ((Lut_fB%max-Lut_fB%min)/(Lut_fB%n-1.0)) ; ENDIF 
                        fb=Lut_fB%min+(iFB-1)*delta  
                        DO iDISS=1,Lut_diss%n !iDISS
                         IF (Lut_diss%n .eq. 1) THEN ; delta=0.0 
                         ELSE ; delta = ((Lut_diss%max-Lut_diss%min)/(Lut_diss%n-1.0)) ; ENDIF 
                         diss=Lut_diss%min+(iDISS-1)*delta  
                         DO iCV=1,Lut_Cv%n !iCV
                          IF (Lut_Cv%n .eq. 1) THEN ; delta=0.0 
                          ELSE ; delta = ((Lut_Cv%max-Lut_Cv%min)/(Lut_Cv%n-1.0)) ; ENDIF 
                          Cv=Lut_Cv%min+(iCV-1)*delta  
                          DO iZETA=1,Lut_zeta%n !iCV
                           IF (Lut_zeta%n .eq. 1) THEN ; delta=0.0 
                           ELSE ; delta = ((Lut_zeta%max-Lut_zeta%min)/(Lut_zeta%n-1.0)) ; ENDIF 
                           zeta=Lut_zeta%min+(iZETA-1)*delta  
                           DO iPSOIL=1,Lut_psoil%n !iCV
                            IF (Lut_psoil%n .eq. 1) THEN ; delta=0.0 
                            ELSE ; delta = ((Lut_psoil%max-Lut_psoil%min)/(Lut_psoil%n-1.0)) ; ENDIF 
                            psoil=Lut_psoil%min+(iPSOIL-1)*delta  
                            DO iTTO=1,Lut_tto%n !iCV
                             IF (Lut_tto%n .eq. 1) THEN ; delta=0.0 
                             ELSE ; delta = ((Lut_tto%max-Lut_tto%min)/(Lut_tto%n-1.0)) ; ENDIF 
                             tto=Lut_tto%min+(iTTO-1)*delta  
                             DO iTTS=1,Lut_tts%n !iCV
                              IF (Lut_tts%n .eq. 1) THEN ; delta=0.0 
                              ELSE ; delta = ((Lut_tts%max-Lut_tts%min)/(Lut_tts%n-1.0)) ; ENDIF 
                              tts=Lut_tts%min+(iTTS-1)*delta  
                              DO iPSI=1,Lut_psi%n !iCV
                               IF (Lut_psi%n .eq. 1) THEN ; delta=0.0 
                               ELSE ; delta = ((Lut_psi%max-Lut_psi%min)/(Lut_psi%n-1.0)) ; ENDIF 
                               psi=Lut_psi%min+(iPSI-1)*delta  
!                              print '(i)',MCLOCK( )-numrow
!                              numrow= MCLOCK( )
                               !tt=DTIME(TA)
                               !print '("time=",f15.8)', tt
                               !LUT call to PROSAIL
                               !!        |in: 
                               !!        |Prospect, green leaf             |Prospect, brown leaf   
                               CALL VecFourSAIL2(N_g,Cab_g,Car_g,Cbrown_g,Cw_g,Cm_g,N_b,Cab_b,Car_b,Cbrown_b,Cw_b,Cm_b,&                
                               !!        |in: 
                               !!        |4SAIL2, structure of the canopy, , soil reflectance (two columns array) and coefficient for mixing them
                                         lai, fb, diss, mli, hot, Cv, zeta,rsoil, psoil,&
                               !!        |in: 
                               !!        |4SAIL2, sun-view gemometry and diffuse fraction
                                         tts, tto, psi, &
                               !!        |out: 
                               !!        |4SAIL2
                                         rsot, rdot, rsdt, rddt, alfast, alfadt)
                               !! PROPER TREATMENT OF SKYL TO BUILD TH HCRF
                               !!hcrf=(1-skylarr)*rsot+skylarr*rdot
                             
                               !LUT write results
                               write (2,300),N_g,Cab_g,Car_g,Cbrown_g,Cw_g,Cm_g,N_b,Cab_b,Car_b,Cbrown_b,Cw_b,Cm_b,&                
                                            lai,fb,diss,mli,hot,Cv,zeta,psoil,&
                                            tts,tto,psi, &
                                            rsot,rdot
!                               write (3),N_g,Cab_g,Car_g,Cbrown_g,Cw_g,Cm_g,N_b,Cab_b,Car_b,Cbrown_b,Cw_b,Cm_b,&                
!                                            lai,fb,diss,mli,hot,Cv,zeta,psoil,&
!                                            tts,tto,psi, &
!                                            rsot,rdot
!                                            rsot,rdot
                               write (3),SNGL(N_g),SNGL(Cab_g),SNGL(Car_g),SNGL(Cbrown_g),SNGL(Cw_g),SNGL(Cm_g),SNGL(N_b),SNGL(Cab_b),SNGL(Car_b),SNGL(Cbrown_b),SNGL(Cw_b),SNGL(Cm_b),&                
                                            SNGL(lai),SNGL(fb),SNGL(diss),SNGL(mli),SNGL(hot),SNGL(Cv),SNGL(zeta),SNGL(psoil),&
                                            SNGL(tts),SNGL(tto),SNGL(psi), &
                                            SNGL(rsot),SNGL(rdot)
                               300 FORMAT(2500(2x,f15.8))	
!	                           DO i=1,nw
!		                        write (2,100) i+399, hcrf(i),rsot(i),rdot(i),rsdt(i),rddt(i),alfast(i),alfadt(i)
!	                           ENDDO
                               
                              ENDDO   !iPSI 
                             ENDDO   !iTTS
                            ENDDO   !iTTO
                           ENDDO   !iPSOIL
                          ENDDO   !iZETA
                         ENDDO   !iCV
                        ENDDO   !iDISS
                       ENDDO   !iFB
                      ENDDO   !iHOT    
                     ENDDO   !iMLI  
                    ENDDO   !iLAI
                    
                   ENDDO !iCMb 
                  ENDDO !iCWb 
                 ENDDO !iCBROWNb 
                ENDDO !iCARb
               ENDDO !iCABb
              ENDDO !iNb
                
             ENDDO !iCMg 
            ENDDO !iCWg 
           ENDDO !iCBROWNg 
          ENDDO !iCARg
         ENDDO !iCABg
        ENDDO !iNg
        close (2) 
        close (3)
          
        STOP_TIME=DCLOCK()
        PRINT *, 'took:', STOP_TIME - START_TIME, 'seconds.'
        !for debug
        READ *, STOP_TIME
    ELSE
        !READ the single_exec_input_file
        inquire (file=singleexe_filename,exist=file_exists)
	    if (.not.file_exists) then
		    print '("file ",a," does not exist. Enter to stop")',trim(singleexe_filename)
		    read (*,'(a)') dummychar
		    stop
	    end if
        OPEN (unit=11,file=singleexe_filename)
            ! PROSPECT
            READ(11,*)  N_g          ! structure coefficient
            READ(11,*)  Cab_g        ! chlorophyll content (µg.cm-2) 
            READ(11,*)  Car_g        ! carotenoid content (µg.cm-2)
            READ(11,*)  Cbrown_g     ! brown pigment content (arbitrary units)
            READ(11,*)  Cw_g         ! EWT (cm)
            READ(11,*)  Cm_g         ! LMA (g.cm-2)

            READ(11,*)  N_b          ! structure coefficient
            READ(11,*)  Cab_b        ! chlorophyll content (µg.cm-2) 
            READ(11,*)  Car_b        ! carotenoid content (µg.cm-2)
            READ(11,*)  Cbrown_b     ! brown pigment content (arbitrary units)
            READ(11,*)  Cw_b         ! EWT (cm)
            READ(11,*)  Cm_b	        ! LMA (g.cm-2)
            !SAIL
	        READ(11,*)  lai          ! LAI (greg: 4.0)
	        READ(11,*)  mli          ! Mean Leaf Inclination angle (90 is erctophile, 5 is plagophile)
            READ(11,*)  hot          ! hot: hot spot effect parameter, estimated as ratio of average leaf width and canopy height 
            READ(11,*)  fB           ! fB: fraction brown lai (greg: 0.2)
            READ(11,*)  diss         ! diss: dissociation factor [0,1]
            READ(11,*)  Cv           ! Cv: vertical crown coverage [0,1]
            READ(11,*)  zeta         ! tree shape factor (diameter/height)
            READ(11,*)  psoil        ! soil linear coefficient  	  
            READ(11,*)  tto          ! VZA
            READ(11,*)  tts          ! SZA
            READ(11,*)  psi          ! RAA
        CLOSE(11)
      
                !!        |in: 
                !!        |Prospect, green leaf             |Prospect, brown leaf   
        CALL VecFourSAIL2(N_g,Cab_g,Car_g,Cbrown_g,Cw_g,Cm_g,N_b,Cab_b,Car_b,Cbrown_b,Cw_b,Cm_b,&                
                !!        |in: 
                !!        |4SAIL2, structure of the canopy, , soil reflectance (two columns array) and coefficient for mixing them
                          lai, fb, diss, mli, hot, Cv, zeta,rsoil, psoil,&
                !!        |in: 
                !!        |4SAIL2, sun-view gemometry and diffuse fraction
                          tts, tto, psi, &
                !!        |out: 
                !!        |4SAIL2
                          rsot, rdot, rsdt, rddt, alfast, alfadt)
      
        !! PROPER TREATMENT OF SKYL TO BUILD TH HCRF
        hcrf=(1-skylarr)*rsot+skylarr*rdot
        open (2, file=outputname)	
        write (2,200) 'Wl(nm)','HCRF','Rso','Rdo','Rsd','Rdd','ABSs','ABSd'
	    DO i=1,nw
		    write (2,100) i+399, hcrf(i),rsot(i),rdot(i),rsdt(i),rddt(i),alfast(i),alfadt(i)
	    ENDDO
	    100 FORMAT(2x,i8,2000(2x,f15.8))	
	    200 FORMAT(1000(2x,a))
	    
	    close (2)    
    ENDIF

   


	stop
	end