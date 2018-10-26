        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 07 15:50:12 2010
        MODULE VECFOURSAIL2__genmod
          INTERFACE 
            SUBROUTINE VECFOURSAIL2(N_G,CAB_G,CAR_G,CBROWN_G,CW_G,CM_G, &
     &N_B,CAB_B,CAR_B,CBROWN_B,CW_B,CM_B,LAI,FB,DISS,MLI,HOT,CV,ZETA,   &
     &RSOIL,PSOIL,TTS,TTO,PSI,RSOT,RDOT,RSDT,RDDT,ALFAST,ALFADT)
              REAL(KIND=8), INTENT(IN) :: N_G
              REAL(KIND=8), INTENT(IN) :: CAB_G
              REAL(KIND=8), INTENT(IN) :: CAR_G
              REAL(KIND=8), INTENT(IN) :: CBROWN_G
              REAL(KIND=8), INTENT(IN) :: CW_G
              REAL(KIND=8), INTENT(IN) :: CM_G
              REAL(KIND=8), INTENT(IN) :: N_B
              REAL(KIND=8), INTENT(IN) :: CAB_B
              REAL(KIND=8), INTENT(IN) :: CAR_B
              REAL(KIND=8), INTENT(IN) :: CBROWN_B
              REAL(KIND=8), INTENT(IN) :: CW_B
              REAL(KIND=8), INTENT(IN) :: CM_B
              REAL(KIND=8), INTENT(IN) :: LAI
              REAL(KIND=8), INTENT(IN) :: FB
              REAL(KIND=8), INTENT(IN) :: DISS
              REAL(KIND=8), INTENT(IN) :: MLI
              REAL(KIND=8), INTENT(IN) :: HOT
              REAL(KIND=8), INTENT(IN) :: CV
              REAL(KIND=8), INTENT(IN) :: ZETA
              REAL(KIND=8), INTENT(IN) :: RSOIL(2,2101)
              REAL(KIND=8), INTENT(IN) :: PSOIL
              REAL(KIND=8), INTENT(IN) :: TTS
              REAL(KIND=8), INTENT(IN) :: TTO
              REAL(KIND=8), INTENT(IN) :: PSI
              REAL(KIND=8), INTENT(OUT) :: RSOT(2101)
              REAL(KIND=8), INTENT(OUT) :: RDOT(2101)
              REAL(KIND=8), INTENT(OUT) :: RSDT(2101)
              REAL(KIND=8), INTENT(OUT) :: RDDT(2101)
              REAL(KIND=8), INTENT(OUT) :: ALFAST(2101)
              REAL(KIND=8), INTENT(OUT) :: ALFADT(2101)
            END SUBROUTINE VECFOURSAIL2
          END INTERFACE 
        END MODULE VECFOURSAIL2__genmod
