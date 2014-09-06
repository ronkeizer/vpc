!*********************************COPYRIGHT******************************************
!                                                                                   !
!       THE NONMEM SYSTEM MAY BE DISTRIBUTED ONLY BY ICON DEVELOPMENT               !
!       SOLUTIONS.                                                                  !
!                                                                                   !
!       COPYRIGHT BY ICON DEVELOPMENT SOLUTIONS                                     !
!       2009-2013 ALL RIGHTS RESERVED.                                              !
!                                                                                   !
!       DO NOT ATTEMPT TO MODIFY CODE WITHOUT FIRST CONSULTING WITH                 !
!       ICON DEVELOPMENT SOLUTIONS.                                                 !
!                                                                                   !
!************************************************************************************
!
!-----------------------------HISTORY------------------------------------------------
! VERSION     : NONMEM VII
! AUTHOR      : ALISON J. BOECKMANN
! CREATED ON  : AUG/1984
! LANGUAGE    : FORTRAN 90/95
! LAST UPDATE : JUL/2008 - COMMON BLOCKS REPLACED WITH MODULES
!               NOV/2008 - INTRODUCED HEADER INFORMATIONS AND RESTRUCTURED AS PER
!                          THE NONMEM STANDARDS
!               DEC/2008 - STATEMENT FUNCTIONS REPLACED WITH INTERNAL FUNCTIONS
!               JAN/2009 - ASSUMED SIZE ARRAY'S REPLACED WITH ASSUMED SHAPE
!               JUN/2009 - INTEGRATED BETA 5.5B4 MODIFICATIONS
!               FEB/2010 - CHANGED SIZES TO PRSIZES
!
!----------------------------- FCN1.F90 ---------------------------------------------
! SUBROUTINE FCN1(Y,YPRIME,NY,T)
!
! DESCRIPTION : Evaluate YPRIME(1),...,YPRIME(N). FCN1 calls DES to obtain DADT. It
!               then adds in the rate of any infusions that are in progress. For
!               elements of YPRIME that are rates of change of the partials of A wrt
!               ETAS, it adds in terms to complete the derivative: + partials of
!               infusion rate + DT* DT/DETA + DA * DA/DETA + DP*DP/DETA (where DT is
!               D(DADT)/DT, DP is D(DADT)/DP, DA is D(DADT)/DA).
!
! ARGUMENTS   : Y,YPRIME,NY,T
!               IN     - NY
!                        NY     - Dimension of Y
!               OUT    - YPRIME
!                        YPRIME - Modified rates of change of Y (DADT)
!               IN OUT - Y,T
!                        Y      - Vector of values (A)
!                        T      - Time
!
! CALLED BY   : ADVAN6 - ADVAN6 routine in PREDPP's library which implement the
!                        general  non-linear  model. The general non-linear model is used for
!                        systems in which a drug is distributed between compartments  according
!                        to  first-order processes.  ADVAN8 may be used in preference to ADVAN6
!                        when the differential equations describing the processes are stiff.
!               ADVAN8 - ADVAN8 routine in PREDPP's library which implement the
!                        general  non-linear  model. The general non-linear model is used for
!                        systems in which a drug is distributed between compartments  according
!                        to  first-order processes.  ADVAN8 may be used in preference to ADVAN6
!                        when the differential equations describing the processes are stiff.
!
! CALLS       : DES - Solve diffrential equations
!
! ALGORITHM   : - If Mapped, begin ON/OFF feature
!                 - Case III: Mapped, compact arrays
!                   - Call user subroutine - DES
!                   - Add infusions
!                 - Case IV: Mapped, full storage arrays
!                   - Call user subroutine - DES
!                   - Add infusions
!                   - Compute partial derivatives
!               - If not Mapped, perform following steps.
!                 - Case I: Not mapped, compact arrays
!                   - Call user subroutine - DES
!                   - Add infusions
!                 - Case II: Not mapped, full storage arrays
!                   - Call user subroutine - DES
!                   - Add infusions
!                 - Compute partial derivatives
!                 - Search DAC for DADA and DADP derivatives for this DADT(I)
!                 - Search DPC for DPDP derivatives for this DADT(I)
!                 - Search DAC for DPDA derivatives for this DADT(I)
!                 - Search DTC for DTDP derivatives for this DADT(I)
!
! MODULES USED: PRSIZES,NMPRD_INT,PRCM_INT,PRCOM_INT,PROCM_INT,PRCM_REAL,PRCOM_REAL,
!               PRCM_LOG,PRCOM_LOG,PR_INTERFACE
!
! CONTAINS    : NONE
!
! LOCAL'S     : AXI,G2,GG,I,IP,IXI,J,JJ,JJY,K,L,LL,LLY,NN,TEMP,TXI,XA,XAPR
!
!---------------------------- END OF HEADER -----------------------------------------
!
      SUBROUTINE FCN1(Y,YPRIME,NY,T)
!      
      USE PRSIZES,      ONLY: ISIZE,DPSIZE,PC,PG,PIR
! INTEGER
      USE NMPRD_INT,    ONLY: IERPRD
      USE PRCM_INT,     ONLY: AA,IAI,IAJ,IAK,IPI,IPJ,IPK,ITI,ITK,LEND,LST,MAP,MAPINV, &
                              NIAI,NIAI1,NIPI,NIPI1,NIT,NIT1,PP,TT,XNCM1
      USE PRCOM_INT,    ONLY: CALCTR,IH,MAXCAL,MCOMP,NBP,NCM1
      USE PROCM_INT,    ONLY: IDXETA
! REAL
      USE PRCM_REAL,    ONLY: DAC,DPC,DTC
      USE PRCOM_REAL,   ONLY: D2TINT,DA,DET,DP,DTINT,G3,H2ETA,HETA,HR,ZERO,G3
! LOGICAL
      USE PRCM_LOG,     ONLY: COMPAC,MAPPED
      USE PRCOM_LOG,    ONLY: DTDER,SECOND
! INTERFACE
      USE PR_INTERFACE, ONLY: DES
!      
      IMPLICIT NONE
!      
      INTEGER(KIND=ISIZE), INTENT(IN)     :: NY
      REAL(KIND=DPSIZE),   INTENT(OUT)    :: YPRIME(*)
      REAL(KIND=DPSIZE),   INTENT(IN OUT) :: Y(*),T
!      
      SAVE
!      
!------------------------------------------------------------------------------------
!     COMMON /NMPRD1/ IERPRD,NETEXT
!     INTEGER IERPRD,NETEXT
!     COMMON /PRCOM0/ NP,NBP,YFORM
!     COMMON /PRCOM0/ MAXKF,IFORM
!     COMMON /PRCOM0/ IDC,IDO,MAXIC,ISV,IINST,ITURN
!     COMMON /PRCOM0/ JTIME,JCONT,JEVENT,JAMT,JRATE,JSS,JDELTA
!     COMMON /PRCOM0/ JCOMPT,JCOMPF,JERROR,SSC,KREC,JMORE,JDUM
!     COMMON /PRCOM1/ NOETAS,SECOND
!     LOGICAL NOETAS,SECOND
!     COMMON /PRCOM4/ G3,HH,DELTA,DT,DTE
!     COMMON /PRCOM4/ YMULT,ZERO,ONE,XR,XD,TSTART,DTSTAR
!     COMMON /PRCOM4/ DDELTA,D2DELT,ADTE,D2ADTE
!     COMMON /PRCOMF/ MAXCAL,CALCTR
!     INTEGER MAXCAL,CALCTR
!     DOUBLE PRECISION DELTA,G3,HH
!     DOUBLE PRECISION YMULT,ZERO,XR,XD
!     DOUBLE PRECISION ONE,TSTART,DTSTAR(PE)
!     DOUBLE PRECISION DDELTA(PE),D2DELT(PE,PE),ADTE(PE),D2ADTE(PE,PE)
!     DOUBLE PRECISION DT,DTE(PE)
!     DIMENSION G3(PG+1,PE+1,PE+1),HH(PE,PE)
!     INTEGER JCONT,JTIME,JEVENT,JAMT,JRATE,JSS,JDELTA
!     INTEGER JCOMPT,JCOMPF,JERROR
!     INTEGER IDC,IDO,NP,NBP,SSC,KREC,JMORE,JDUM
!     INTEGER ISV(PC)
!     INTEGER IINST(PC),ITURN(PC)
!     INTEGER MAXKF,IFORM(PG+1)
!     INTEGER YFORM,MAXIC
!     INTEGER NY
!     COMMON /PRCOMD/ DA,DP,DET,HR,HETA,H2ETA
!     COMMON /PRCOME/ NRD,MCOMP,NCM1,IH
!     DOUBLE PRECISION DA,DP,HR,HETA,DET,H2ETA
!     DOUBLE PRECISION Y,YPRIME,T
!     DIMENSION Y(*),YPRIME(*) !,DA(PM,PM),DP(PM,PG),HR(PM,2),DET(PM)
!     DIMENSION HETA(PM,PE,2),H2ETA(PM,PE,PE,2)
!     INTEGER NRD,MCOMP,NCM1,IH
!     COMMON /PRCMX1/ GENMOD,MAPPED,COMPAC
!     LOGICAL GENMOD,MAPPED,COMPAC
!     COMMON /PRCMX2/ XNC,XNCM1,MAP,MAPINV,NBRON,XNBRON
!     INTEGER XNC,XNCM1,MAP(PC),MAPINV(PC),NBRON,XNBRON
!     DOUBLE PRECISION XA(PM),XAPR(PM)
!     COMMON /PRCOMX/ DTINT(PE),D2TINT(PE,PE)
!     DOUBLE PRECISION DTINT,D2TINT
!     COMMON /PRCMET/ LST,LEND,SLEND(PE),SLST(PE),SPW,SLOOP
!     INTEGER LST,LEND,SLEND,SLST,SPW,SLOOP
!     COMMON /PROCM5/ NACTIV,M(0:PE)
!     INTEGER NACTIV,M
!     COMMON /PRCOMT/ DTIME,LFLAG,DTDER
!     LOGICAL DTIME,LFLAG,DTDER
!     COMMON /PRCMDE/ NIAI,NIPI,NIT,XNIAI,XNIPI,XNIT
!     INTEGER NIAI,NIPI,NIT,XNIAI,XNIPI,XNIT
!     COMMON /PRCMDE/ IAI(PIR),IAJ(PIR),IAK(PIR)
!     COMMON /PRCMDE/ IPI(PIR),IPJ(PIR),IPK(PIR)
!     COMMON /PRCMDE/ ITI(PIR),ITK(PIR)
!     COMMON /PRCMDE/ XIAI(PIR),XIAJ(PIR),XIAK(PIR)
!     COMMON /PRCMDE/ XIPI(PIR),XIPJ(PIR),XIPK(PIR)
!     COMMON /PRCMDE/ XITI(PIR),XITK(PIR)
!     COMMON /PRCMDE/ IAC(PIR),IPC(PIR),ITC(PIR)
!     COMMON /PRCMDE/ NIAI1,NIPI1,NIT1,XNIAI1,XNIPI1,XNIT1
!     COMMON /PRCMDE/ AA,PP,TT,XAA,XPP,XTT
!     INTEGER IAI,IAJ,IAK,IPI,IPJ,IPK,ITI,ITK
!     INTEGER XIAI,XIAJ,XIAK,XIPI,XIPJ,XIPK,XITI,XITK
!     INTEGER IAC,IPC,ITC
!     INTEGER NIAI1,NIPI1,NIT1,XNIAI1,XNIPI1,XNIT1
!     INTEGER AA(PIR),PP(PIR),TT(PIR),XAA(PIR),XPP(PIR),XTT(PIR)
!     COMMON /PRCMDC/ DAC(PIR,1),DPC(PIR,1),DTC(PIR)
!     DOUBLE PRECISION DAC,DPC,DTC
!------------------------------------------------------------------------------------
!
! Local Variables
!
      INTEGER(KIND=ISIZE), PARAMETER :: PM=PC-1
      INTEGER(KIND=ISIZE) :: AXI,I,IP,IXI,J,JJ,JJY,K,L,LL,LLY,NN,TXI 
!
      REAL(KIND=DPSIZE)   :: GG,G2,TEMP,XA(PM),XAPR(PM)
!      
      G2(I,J,K)=G3(I,IDXETA(J)+1,IDXETA(K)+1) ! Statement function
      GG(I,J)=G3(I,IDXETA(J-1)+1,1)
!
! Variables supplied by user's subroutine DES:
!   DADT(I) = Derivative of A(I) wrt time T
!   DA(I,K) = Derivative of DADT(I) wrt A(K)
!   DP(I,J) = Derivative of DADT(I) wrt GG(J)
! For implementation of general ADVANS' compartment on/off feature
! GENMOD true for general ADVAN (Set by PREDI)
! XNC,XNCM1,XNBRON are original values of NC,NCM1,NBRON (Set by PREDI)
! NBRON tells how many compartments are now ON (Not counting output)
! Mapped is TRUE when a mapping in effect (Set by SADVAN,SSS)
! MAP,MAPINV: Maps 'REAL' compartment nos. to 'Reduced set' and V.V.-Added 1/92
! Added 3/94- For loops on ETAS in ADVAN6, ADVAN8, SS6- Added 6/94
! Counters for indices for the COMPACT arrays (X versions are unmapped)
! Indices for the COMPACT arrays (X versions are unmapped)
! IAC maps the contents of the COMPACT array DAC, etc. the compact arrays themselves
!
      CALCTR=CALCTR+1
! 
      IF (CALCTR > MAXCAL) THEN
        IERPRD=999; GO TO 999
      END IF
!      
      IF (MAPPED) THEN   
        DO I=XNCM1,1,-1 ! Begin ON/OFF feature
          IF (MAP(I) == 0) THEN
            XA(I)=ZERO
          ELSE
            XA(I)=Y(MAP(I))
          END IF
        END DO
!        
        IF (COMPAC) THEN    ! Case III: Mapped, compact arrays
          CALL DES(XA,G3,T,XAPR,PIR,DAC,DPC,DTC)    ! Call user subroutine
          IF (IERPRD > 0) GO TO 999
          DO I=1,NCM1
            YPRIME(I)=XAPR(MAPINV(I))+HR(I,IH)  ! Add infusions
          END DO  
          IF (NY == NCM1) GO TO 999
        ELSE    ! Case IV: Mapped, full storage arrays 
          CALL DES(XA,G3,T,XAPR,MCOMP,DA,DP,DET)  ! Call user subroutine
          IF (IERPRD <= 0) THEN 
            DO I=1,NCM1
              YPRIME(I)=XAPR(MAPINV(I))+HR(I,IH)! Add infusions
            END DO  
            IF (NY /= NCM1) THEN    ! Compute partial derivatives
              K=0
              DO L=LST,LEND
                K=K+NCM1
                DO I=1,NCM1
                  NN=K+I
                  YPRIME(NN)=HETA(I,L,IH)+DET(MAPINV(I))*DTINT(L)
                  DO J=1,NCM1
                    YPRIME(NN)=YPRIME(NN)+DA(MAPINV(I),MAPINV(J))*Y(K+J)
                  END DO
                  DO J=1,NBP
                    YPRIME(NN)=YPRIME(NN)+DP(MAPINV(I),J)*GG(J,L+1)
                  END DO
                END DO
              END DO 
            END IF  
          END IF
          GO TO 999
        END IF
      ELSE  ! If not mapped 
        IF (COMPAC) THEN    ! Case I: Not mapped, compact arrays
          CALL DES(Y,G3,T,YPRIME,PIR,DAC,DPC,DTC)   ! Call user subroutine
          IF (IERPRD > 0) GO TO 999 ! Add infusions
          YPRIME(1:NCM1)=YPRIME(1:NCM1)+HR(1:NCM1,IH)
          IF (NY == NCM1) GO TO 999
        ELSE
          CALL DES(Y,G3,T,YPRIME,MCOMP,DA,DP,DET)   ! Case II: Not mapped, full storage arrays
          IF (IERPRD <= 0) THEN 
            YPRIME(1:NCM1)=YPRIME(1:NCM1)+HR(1:NCM1,IH) ! Add infusions
            IF (NY /= NCM1) THEN 
              K=0
              DO L=LST,LEND
                K=K+NCM1
                DO I=1,NCM1
                  NN=K+I
                  YPRIME(NN)=HETA(I,L,IH)+DET(I)*DTINT(L)
                  DO J=1,NCM1
                    YPRIME(NN)=YPRIME(NN)+DA(I,J)*Y(K+J)
                  END DO
                  DO J=1,NBP
                    YPRIME(NN)=YPRIME(NN)+DP(I,J)*GG(J,L+1)
                  END DO
                END DO
              END DO
            END IF  
          END IF  
          GO TO 999
        END IF
      END IF
!
      K=0 ! Compute partial derivatives
!      
      DO L=1,LEND
        K=K+NCM1
        DO I=1,NCM1
          YPRIME(K+I)=HETA(I,L,IH)
        END DO  
        IF (DTDER) THEN
          DO I=1,NIT1
            NN=K+ITI(I)
            YPRIME(NN)=YPRIME(NN)+DTC(TT(I))*DTINT(L)
          END DO
        END IF
        DO I=1,NIAI1
          NN=K+IAI(I)
          YPRIME(NN)=YPRIME(NN)+DAC(AA(I),1)*Y(K+IAJ(I))
        END DO
        DO I=1,NIPI1
          NN=K+IPI(I)
          YPRIME(NN)=YPRIME(NN)+DPC(PP(I),1)*GG(IPJ(I),L+1)
        END DO
      END DO
!      
      IF (.NOT. SECOND) GO TO 999
!      
      DO LL=LST,LEND
        DO JJ=1,LL
          JJY=JJ*NCM1
          K=K+NCM1
          LLY=LL*NCM1
          DO I=1,NCM1
            YPRIME(K+I)=H2ETA(I,LL,JJ,IH)
          END DO  
          IF (DTDER) THEN
            DO I=1,NIT1
              NN=K+ITI(I)
              YPRIME(NN)=YPRIME(NN)+DTC(TT(I))*D2TINT(LL,JJ)
              TEMP=ZERO
              DO IP=NIT1+1,NIT
                IF (ITK(IP) <= MCOMP) THEN
                  TEMP=TEMP+DTC(TT(IP))*Y(ITK(IP)+LLY)
                  CYCLE
                END IF
                IF (ITK(IP) < PC+PG+1) THEN
                  TEMP=TEMP+DTC(TT(IP))*GG(ITK(IP)-MCOMP-1,LL+1)
                  CYCLE
                END IF
                TEMP=TEMP+DTC(TT(IP))*DTINT(LL)
              END DO
              YPRIME(NN)=YPRIME(NN)+TEMP*DTINT(JJ)
            END DO
          END IF
          IXI=NIAI1+1
          TXI=NIT1+1
!          
          DO I=1,NIAI1
            NN=K+IAI(I)
            YPRIME(NN)=YPRIME(NN)+DAC(AA(I),1)*Y(K+IAJ(I))
            TEMP=ZERO ! Search DAC for DADA and DADP derivatives for this DADT(I)
            DO WHILE (IXI <= NIAI)
              IF (IAI(IXI) > IAI(I) .OR. IAJ(IXI) > IAJ(I) ) EXIT
              IF (IAK(IXI) <= MCOMP+1) THEN
                TEMP=TEMP+DAC(AA(IXI),1)*Y(IAK(IXI)+LLY)
              ELSE
                TEMP=TEMP+DAC(AA(IXI),1)*GG(IAK(IXI)-MCOMP-1,LL+1)
              END IF
              IXI=IXI+1
            END DO
!
            IF (DTDER) THEN
              DO WHILE (TXI <= NIT)
                IF (ITI(TXI) > IAI(I) .OR. ITK(TXI) > IAJ(I)) EXIT
                TEMP=TEMP+DTC(TT(TXI))*DTINT(LL)
                TXI=TXI+1
              END DO
            END IF
            YPRIME(NN)=YPRIME(NN)+TEMP*Y(JJY+IAJ(I))
          END DO
! 
          TXI=NIT1+1 ! With only P's in DE
          IXI=NIPI1+1
! 
          DO I=1,NIPI1
            NN=K+IPI(I)
            YPRIME(NN)=YPRIME(NN)+DPC(PP(I),1)*G2(IPJ(I),LL,JJ)
            TEMP=ZERO
            DO WHILE (IXI <= NIPI)  ! Search DPC for DPDP derivatives for this DADT(I)
              IF (IPI(IXI) > IPI(I) .OR. IPJ(IXI) > IPJ(I)) EXIT
              TEMP=TEMP+DPC(PP(IXI),1)*GG(IPK(IXI),LL+1)
              IXI=IXI+1
            END DO
!
            IF (NIAI1 < NIAI) THEN ! Search DAC for DPDA derivatives for this DADT(I)
              AXI=NIAI1+1              
              DO WHILE (AXI <= NIAI)
                IF (IAI(AXI) >= IPI(I)) THEN
                  IF (IAI(AXI) > IPI(I)) EXIT
                  IF (IAK(AXI) == IPJ(I)+MCOMP+1) THEN
                    TEMP=TEMP+DAC(AA(AXI),1)*Y(IAJ(AXI)+LLY)
                  END IF
                END IF  
                AXI=AXI+1
              END DO
            END IF
!            
            IF (DTDER) THEN ! Search DTC for DTDP derivatives for this DADT(I)
              DO WHILE (TXI <= NIT)
                IF (ITI(TXI) > IPI(I)) EXIT
                IF (ITI(TXI) == IPI(I)) THEN 
                  IF (ITK(TXI) > IPJ(I)+MCOMP+1) EXIT
                  IF (ITK(TXI) == IPJ(I)+MCOMP+1) THEN
                    TEMP=TEMP+DTC(TT(TXI))*DTINT(LL)
                  END IF
                END IF  
                TXI=TXI+1
              END DO
            END IF
            YPRIME(NN)=YPRIME(NN)+TEMP*GG(IPJ(I),JJ+1)
          END DO
        END DO
      END DO
!      
  999 RETURN 
!  
      END SUBROUTINE FCN1
