        !COMPILER-GENERATED INTERFACE MODULE: Wed May 12 15:19:52 2010
        MODULE FOURSAIL2__genmod
          INTERFACE 
            SUBROUTINE FOURSAIL2(RG,TG,RB,TB,RSOSOIL,RDOSOIL,RSDSOIL,   &
     &RDDSOIL,LAI,LIDFA,LIDFB,HOT,FB,DISS,CV,ZETA,TTO,TTS,PSI,RSOT,RDOT,&
     &RSDT,RDDT,ALFAST,ALFADT)
              REAL(KIND=8) :: RG(2101)
              REAL(KIND=8) :: TG(2101)
              REAL(KIND=8) :: RB(2101)
              REAL(KIND=8) :: TB(2101)
              REAL(KIND=8), INTENT(IN) :: RSOSOIL(2101)
              REAL(KIND=8), INTENT(IN) :: RDOSOIL(2101)
              REAL(KIND=8), INTENT(IN) :: RSDSOIL(2101)
              REAL(KIND=8), INTENT(IN) :: RDDSOIL(2101)
              REAL(KIND=8), INTENT(IN) :: LAI
              REAL(KIND=8), INTENT(IN) :: LIDFA
              REAL(KIND=8), INTENT(IN) :: LIDFB
              REAL(KIND=8), INTENT(IN) :: HOT
              REAL(KIND=8), INTENT(IN) :: FB
              REAL(KIND=8), INTENT(IN) :: DISS
              REAL(KIND=8), INTENT(IN) :: CV
              REAL(KIND=8), INTENT(IN) :: ZETA
              REAL(KIND=8), INTENT(IN) :: TTO
              REAL(KIND=8), INTENT(IN) :: TTS
              REAL(KIND=8), INTENT(IN) :: PSI
              REAL(KIND=8), INTENT(OUT) :: RSOT(2101)
              REAL(KIND=8), INTENT(OUT) :: RDOT(2101)
              REAL(KIND=8), INTENT(OUT) :: RSDT(2101)
              REAL(KIND=8), INTENT(OUT) :: RDDT(2101)
              REAL(KIND=8), INTENT(OUT) :: ALFAST(2101)
              REAL(KIND=8), INTENT(OUT) :: ALFADT(2101)
            END SUBROUTINE FOURSAIL2
          END INTERFACE 
        END MODULE FOURSAIL2__genmod
