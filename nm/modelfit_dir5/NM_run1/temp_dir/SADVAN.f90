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
! CREATED ON  : DEC/1983
! LANGUAGE    : FORTRAN 90/95
! LAST UPDATE : APR/1990 - SECOND DERIVATIVES, DERIVATIVES W.R.T. TIME
!               APR/1999 - NOTE THAT TSTART IS NOT UPDATED. IS THIS A BUG?
!               JUL/2008 - COMMON BLOCKS REPLACED WITH MODULES
!               NOV/2008 - INTRODUCED HEADER INFORMATIONS AND RESTRUCTURED AS PER
!                          THE NONMEM STANDARDS
!               FEB/2009 - MOVED DATA STATEMENTS TO PRDATA.F90 FILE
!               APR/2009 - IQUIT ADDED TO HANDLE ERROR MESSAGES
!               FEB/2010 - CHANGED SIZES TO PRSIZES
!               JUL/2010 - INCORPORATED 7.2BETA5.2B CHANGES
!
!----------------------------- SADVAN.F90 -------------------------------------------
! SUBROUTINE SADVAN(ICALL,SAMODE)
!
! DESCRIPTION : Subroutine SADVAN - Supervisor of Advance
!
!               PREDPP contains a library of routines, called ADVAN routines, which
!               implement specific kinetic models. Exactly one ADVAN routine must be
!               selected for each NONMEM / PREDPP run. Its function is to "Advance"
!               the kinetic system from one state time point to the next
!
! ARGUMENTS   : ICALL,SAMODE
!               IN     - ICALL
!                        ICALL - Values are 0,1,2 or 3. ICALL values 0,1,2,3 from NONMEM
!                                are unchanged. ICALL values >=4 are converted to 2 for
!                                this routine.
!                        SAMODE - Flag for calling ADVAN
!               OUT    - NONE
!               IN OUT - NONE
!
! CALLED BY   : PRED  - This is PREDPP main program. Provides prediction, partial 
!                       derivatives of the statistical model with respect to ETA
!                       and EPSILON random variables and stores them in the G
!                       and H arguments of the PRED routine.
!               PREDI - Initialization-Finalization routine of PRED
!
! CALLS       : ADVAN - Set compartment models
!
! ALGORITHM   : - If (ICALL /= 2) implement kinetic equations and RETURN
!               - CALL DES and AES feature
!               - Start ON/OFF feature
!                 - Compress state vector, status vector, and set up mapping
!                 - Redo the mapping for compact arrays
!                 - Change compartment numbers for infusions
!                   - Zero out for mapped case
!                 - End ON/OFF feature.
!               - Zero out for normal case
!               - Save DTIME: Set FALSE in case of intermediate infusions
!               - Check if any infusions are in progress
!               - If (IF /= 0),set up rate, duration arrays and DD array for sorting
!                 - Loop over infusions
!                 - Constant rate or duration is modelled for this compartment
!                 - Model rate for this compartment
!                 - Loop over number of ETAs; Derivatives computed
!                 - Accumulate total rate for this compartment
!                 - Sort DD in order by duration
!                   - Move smallest duration to the top
!                   - Test if (remaining durations > DELTA) and quit the loop if true
!                 - Loop through sorted infusions checking for intermediate points
!                   - Terminate infusion
!                   - Advance to DT
!                   - Update remaining durations
!               - Finish intermediate points or no infusions in progress
!               - Update infusions
!                 - Check if all infusions ended
!                 - Else, update continuing infusions
!                 - Delete finished infusions
!               - General model, some compartment(s) OFF
!                 - Restore compartment numbers for infusions
!                 - Uncompress state vector, status vector, and restore 1-1 mapping
!
! MODULES USED: PRSIZES,PRDATA,NMPRD_INT,PRCM_INT,PRCOM_INT,PROCM_INT,PRCOM_REAL,
!               PROCM_REAL,NMPRD_CHAR,PRCM_LOG,PRCOM_LOG,PR_INTERFACE
!
! CONTAINS    : NONE
!
! LOCAL'S     : DEL,G2,GG,I,IF,IFIN,II,IMOVE,ITEMP,J,JC,JJ,K,KK,KP,STIME
!
!---------------------------- END OF HEADER -----------------------------------------
!
      SUBROUTINE SADVAN(ICALL,SAMODE) !7.2b52b
!      
      USE PRSIZES,      ONLY: ISIZE,DPSIZE
!      
      USE PRDATA,       ONLY: TWO
! INTEGER
      USE NMPRD_INT,    ONLY: IERPRD,IQUIT
      
      USE PRCM_INT,     ONLY: AA,IAI,IAJ,IAK,IPI,IPJ,IPK,ITI,ITK,LNCM1,LNCM2,MAP,MAPINV,&
                              NBRON,NIAI,NIAI1,NIPI,NIPI1,NIT,NIT1,PP,TT,XAA,XIAI,XIAJ, &
                              XIAK,XIPI,XIPJ,XIPK,XITI,XITK,XLNCM1,XLNCM2,XNC,XNCM1,    &
                              XNIAI,XNIAI1,XNIPI,XNIPI1,XNIT,XNIT1,XPP,XTT    
      USE PRCOM_INT,    ONLY: BETA,DCTR,DD,IBACK,IHEAD,INEXT,IP,IPOOL,IRR,ISPEC,MCOMP,  &
                              NC,NCM1,NPETAS,SV
      USE PROCM_INT,    ONLY: IDXETA
! REAL        
      USE PRCOM_REAL,   ONLY: ADTE,D2ADTE,D2DELT,DDELTA,DELTA,DT,I2AEA,I2DEA,I2REA,G3,  &
                              IA,IAA,IAEA,IDA,IDEA,IRA,IREA,R,R2E,RE,TSTART,XR,ZERO  
      USE PROCM_REAL,   ONLY: AMNT,DAETA,D2AETA,TSTATE
! CHARACTER
      USE NMPRD_CHAR,   ONLY: ETEXT
! LOGICAL
      USE PRCM_LOG,     ONLY: DOFINL,GENMOD,MAPPED,COMPAC
      USE PRCOM_LOG,    ONLY: DTIME,NOETAS,SECOND
! INTERFACE
      USE PR_INTERFACE, ONLY: ADVAN
!
      IMPLICIT NONE
!      
      INTEGER(KIND=ISIZE), INTENT(IN) :: ICALL,SAMODE !7.2b52b
!
      SAVE
!      
!------------------------------------------------------------------------------------
!     COMMON /NMPRD1/ IERPRD,NETEXT
!     COMMON /NMPRD2/ ETEXT(3)
!     INTEGER IERPRD,NETEXT
!     CHARACTER*132 ETEXT
!     COMMON /PRCOM0/ NP,NBP,YFORM
!     COMMON /PRCOM0/ MAXKF,IFORM
!     COMMON /PRCOM0/ IDC,IDO,MAXIC,ISV,IINST,ITURN
!     COMMON /PRCOM0/ JTIME,JCONT,JEVENT,JAMT,JRATE,JSS,JDELTA
!     COMMON /PRCOM0/ JCOMPT,JCOMPF,JERROR,SSC,KREC,JMORE,JDUM
!     COMMON /PRCOM1/ NOETAS,SECOND
!     COMMON /PRCOM2/ IBF,IRR,IS,ID,ITSC,IFR,ILAG
!     COMMON /PRCOM3/ ITRANS,IRGG,IREV,NPETAS,NPEPS
!     COMMON /PRCOM4/ G3,HH,DELTA,DT,DTE
!     COMMON /PRCOM4/ YMULT,ZERO,ONE,XR,XD,TSTART,DTSTAR
!     COMMON /PRCOM4/ DDELTA,D2DELT,ADTE,D2ADTE
!     COMMON /PRCOM5/ ISPEC,DCTR,BETA,DD
!     COMMON /PRCOM5/ IP,IPOOL,IHEAD,INEXT,IBACK,SV
!     COMMON /PRCOM6/ IA,IAA,IAEA,IRA,IREA,IDA,IDEA,R,RE
!     COMMON /PRCOM6/ RHO,RHOE,SDEL,SDELE,SSA,SSAE,SSR,SSRE
!     COMMON /PRCOM6/ SAMT,SDEL1
!     COMMON /PRCOM6/ I2AEA,I2REA,I2DEA,R2E,D2DTE,D2TSTA
!     COMMON /PRCOMN/ LOGUNT,NC
!     DOUBLE PRECISION DELTA,G3,HH
!     DOUBLE PRECISION SDELE,RHOE,SSAE,SSRE,YMULT,ZERO,XR,XD
!     DOUBLE PRECISION ONE,TSTART,DTSTAR(PE)
!     DOUBLE PRECISION DDELTA(PE),D2DELT(PE,PE),ADTE(PE),D2ADTE(PE,PE)
!     DOUBLE PRECISION IA(90),IAA(90),IAEA(90,PE),IRA(90),IDA(90)
!     DOUBLE PRECISION IREA(90,PE),IDEA(90,PE),R(PC),RE(PC,PE)
!     DOUBLE PRECISION I2REA(90,PE,PE),I2DEA(90,PE,PE),I2AEA(90,PE,PE)
!     DOUBLE PRECISION R2E(PC,PE,PE),D2DTE(PE,PE)
!     DOUBLE PRECISION D2TSTA(PE,PE)
!     DOUBLE PRECISION DT,DTE(PE),RHO,SDEL,SSA,SSR
!     DOUBLE PRECISION SAMT,SDEL1
!     DIMENSION SDELE(PE),RHOE(PE),SSAE(PE),SSRE(PE)
!     DIMENSION G3(PG+1,PE+1,PE+1),HH(PE,PE)
!     INTEGER LOGUNT,IRGG,IREV,ITRANS,NPETAS,NPEPS
!     INTEGER JCONT,JTIME,JEVENT,JAMT,JRATE,JSS,JDELTA
!     INTEGER JCOMPT,JCOMPF,JERROR
!     INTEGER NC,IDC,IDO,NP,NBP,SSC,KREC,JMORE,JDUM
!     INTEGER ISV(PC),IBF(PC),IRR(PC),SV(PC)
!     INTEGER IINST(PC),ITURN(PC),ITSC,IFR,ILAG(PC),IS(PC),ID(PC)
!     INTEGER MAXKF,IFORM(PG+1)
!     INTEGER YFORM,MAXIC
!     INTEGER BETA(90),IPOOL(90),IP,IHEAD,INEXT(90),IBACK(90)
!     INTEGER ISPEC,DD(90),DCTR
!     LOGICAL NOETAS,SECOND
!     COMMON /PRCMX1/ GENMOD,MAPPED,COMPAC
!     LOGICAL GENMOD,MAPPED,COMPAC
!     COMMON /PRCMX2/ XNC,XNCM1,MAP,MAPINV,NBRON,XNBRON
!     INTEGER XNC,XNCM1,MAP(PC),MAPINV(PC),NBRON,XNBRON
!     COMMON /PRCOME/ NRD,MCOMP,NCM1,IH
!     INTEGER NRD,MCOMP,NCM1,IH
!     COMMON /PRCMLS/ XLNCM1,XLNCM2,LNCM1,LNCM2
!     INTEGER XLNCM1,XLNCM2,LNCM1,LNCM2
!     COMMON /PRCOMT/ DTIME,LFLAG,DTDER
!     LOGICAL DTIME,LFLAG,DTDER
!     COMMON /PROCM4/ A,DAETA,D2AETA
!     DOUBLE PRECISION A,DAETA,D2AETA
!     DIMENSION A(PC),DAETA(PC,PE),D2AETA(PC,PE,PE)
!     COMMON /PROCM9/ TSTATE
!     DOUBLE PRECISION TSTATE
!     COMMON /PRCMDE/ NIAI,NIPI,NIT,XNIAI,XNIPI,XNIT
!     INTEGER NIAI,NIPI,NIT,XNIAI,XNIPI,XNIT
!     COMMON /PRCMDE/ IAI(PIR),IAJ(PIR),IAK(PIR)
!     COMMON /PRCMDE/ IPI(PIR),IPJ(PIR),IPK(PIR)
!     COMMON /PRCMDE/ ITI(PIR),ITK(PIR)
!     COMMON /PRCMDE/ XIAI(PIR),XIAJ(PIR),XIAK(PIR)
!     COMMON /PRCMDE/ XIPI(PIR),XIPJ(PIR),XIPK(PIR)
!     COMMON /PRCMDE/ XITI(PIR),XITK(PIR)
!     COMMON /PRCMDE/ IAC(PIR),IPC(PIR),ITC(PIR)
!     INTEGER IAI,IAJ,IAK,IPI,IPJ,IPK,ITI,ITK
!     INTEGER XIAI,XIAJ,XIAK,XIPI,XIPJ,XIPK,XITI,XITK
!     INTEGER IAC,IPC,ITC
!     COMMON /PRCMDE/ NIAI1,NIPI1,NIT1,XNIAI1,XNIPI1,XNIT1
!     COMMON /PRCMDE/ AA,PP,TT,XAA,XPP,XTT
!     INTEGER NIAI1,NIPI1,NIT1,XNIAI1,XNIPI1,XNIT1
!     INTEGER AA(PIR),PP(PIR),TT(PIR),XAA(PIR),XPP(PIR),XTT(PIR)
!     COMMON /PROCM5/ NACTIV,M(0:PE)
!     INTEGER NACTIV,M
!     COMMON /PRCM03/ DIDCAA,DIDDES,DIDAES,DOFINL
!     LOGICAL DIDCAA,DIDDES,DIDAES,DOFINL
!------------------------------------------------------------------------------------
!
! Local Variables    
!
      INTEGER(KIND=ISIZE) :: I,IF,IFIN,II,IMOVE,ITEMP,J,JC,JJ,K,KK,KP
!      
      REAL(KIND=DPSIZE)   :: DEL,G2,GG
!      
      LOGICAL :: STIME
!      
      G2(I,J,K)=G3(I,IDXETA(J)+1,IDXETA(K)+1) ! Statement function
      GG(I,J)=G3(I,IDXETA(J-1)+1,1)
!
! For second derivatives
! Counters for indices for the compact arrays (X Versions are unmapped)
! For ADVAN or SS
!
! Variables used to advance by DT
! For infusions which continue past DT:
! R(IC)         = Infusion rate into compartment IC
! RE(IC,K)      = Derivative of rate wrt ETA(K)
! If an infusion is terminating, ISPEC>0 and:
! IRA(ISPEC)    = Its infusion rate
! IREA(ISPEC,K) = Derivative of IRA wrt ETA(K)
! IDA(ISPEC)    = Its duration
! IDEA(ISPEC,K) = Derivative of IDEA wrt ETA(K)
! BETA(ISPEC)   = Its compartment number
!
! For implementation of general ADVANs' compartment ON/OFF feature
! GENMOD true for general ADVAN (set by PREDI)
! XNC,XNCM1,XNBRON are original values of NC,NCM1,NBRON (set by PREDI)
! NBRON tells how many compartments are now ON (not counting output)
! MAPPED is true when a mapping in effect (set by SADVAN, SSS)
! MAP, MAPINV: maps 'real' compartment numbers to 'reduced set' and V.V.
!
! LNCM1 = Number of DES-type compartments (XLNCM1 is original no.)
! LNCM2 = Number of AES-type compartments (XLNCM2 is original no.)
!
! Indices for the compact arrays (x versions are unmapped)
! IAC maps the contents of the compact array DAC
! IPC maps the contents of the compact array DPC
! ITC maps the contents of the compact array DTC
!
      IF (ICALL /= 2) THEN    
        IF (SAMODE == 0) CALL ADVAN(ICALL) !7.2b52b
        GO TO 999
      END IF
! ICALL=2 - Normal entry
! Call DES, Call AES feature
      IF (DOFINL) THEN
        IF (SAMODE == 0) CALL ADVAN(ICALL) !7.2b52b
        GO TO 999
      END IF      
!      
      IF (GENMOD) THEN ! ON/OFF feature
        IF (SECOND .AND. .NOT. COMPAC) THEN
          ETEXT(2)='COMPACT DES ARRAYS REQUIRED WITH LAPLACIAN METHOD.'
          IERPRD=2; GO TO 999
        END IF       
        IF (NBRON /= NCM1) THEN
! General model, some compartment(s) OFF
! Note that TSTART is not updated. Is this a BUG? 4/99
          IF (NBRON == 0) GO TO 999
          LNCM1=XLNCM1 ! Compress state vector, status vector, and set up mapping
          JJ=0
          DO II=1,NCM1
            IF (SV(II) == 0) THEN
              MAP(II)=0
              IF (II <= XLNCM1) LNCM1=LNCM1-1
            ELSE
              JJ=JJ+1
              MAP(II)=JJ
              MAPINV(JJ)=II
              IF (II /= JJ) THEN
                AMNT(JJ) = AMNT(II)
                IF (.NOT. NOETAS) THEN
                  DAETA(JJ,1:NPETAS) = DAETA(II,1:NPETAS)
                  IF (SECOND) THEN
                    DO K=1,NPETAS
                      DO J=K,NPETAS
                        D2AETA(JJ,J,K) = D2AETA(II,J,K)
                      END DO
                    END DO
                  END IF
                END IF
              END IF
              SV(JJ)=SV(II)
            END IF
          END DO
!          
          NC=NBRON+1;       NCM1=NBRON
          LNCM2=NCM1-LNCM1; MAPPED=.TRUE.
!
          IF (COMPAC) THEN ! Redo the mapping for compact arrays
            NIAI1=0; NIAI=0; NIT=0
            NIPI1=0; NIPI=0; NIT1=0
            DO I=1,XNIAI
              IF (MAP(XIAI(I)) == 0 .OR. MAP(XIAJ(I)) == 0) CYCLE
              IF (XIAK(I) /= 0 .AND. XIAK(I) <= MCOMP+1) THEN
                IF (MAP(XIAK(I)) == 0) CYCLE
              END IF
              NIAI = NIAI+1
              AA(NIAI)  = XAA(I)
              IAI(NIAI) = MAP(XIAI(I))
              IAJ(NIAI) = MAP(XIAJ(I))
              IAK(NIAI) = XIAK(I)
              IF (IAK(NIAI) == 0) THEN
                NIAI1=NIAI1+1
                CYCLE
              END IF
              IF (IAK(NIAI) <= MCOMP+1) IAK(NIAI) = MAP(XIAK(I))
            END DO
            DO I=1,XNIPI
              IF (MAP(XIPI(I)) == 0) CYCLE
              NIPI = NIPI+1
              PP(NIPI)  = XPP(I)
              IPI(NIPI) = MAP(XIPI(I))
              IPJ(NIPI) = XIPJ(I)
              IPK(NIPI) = XIPK(I)
              IF (IPK(NIPI) == 0) NIPI1 = NIPI1+1
            END DO
            DO I=1,XNIT
              IF (MAP(XITI(I)) == 0) CYCLE
              IF (XITK(I) /= 0 .AND. XITK(I) <= MCOMP+1) THEN
                IF (MAP(XITK(I)) == 0) CYCLE
              END IF
              NIT=NIT+1
              TT(NIT)=XTT(I)
              ITI(NIT)=MAP(XITI(I))
              ITK(NIT)=XITK(I)
              IF (ITK(NIT) == 0) THEN
                NIT1=NIT1+1
                CYCLE
              END IF
              IF (ITK(NIT) <= MCOMP+1) ITK(NIT)=MAP(XITK(I))
            END DO
          END IF
!
          IF (IHEAD /= 0) THEN ! Change compartment numbers for infusions
            IF=IHEAD  
            DO WHILE (IF /= 0)
              BETA(IF)=MAP(BETA(IF))
              IF=INEXT(IF)
            END DO
            DO JC=1,NC         ! Zero-out for mapped case
              R(JC)=ZERO
              IF (NOETAS) CYCLE
              RE(JC,1:NPETAS)=ZERO
              IF (SECOND) THEN
                DO K=1,NPETAS
                  DO J=K,NPETAS
                    R2E(JC,J,K)=ZERO
                  END DO
                END DO
              END IF
            END DO
            GO TO 1098
          END IF
        END IF
      END IF                                 ! End ON/OFF feature
!
      DO JC=1,NC                             ! Zero-out for normal case
        R(JC)=ZERO
        IF (NOETAS) CYCLE
        RE(JC,1:NPETAS)=ZERO
        IF (SECOND) THEN
          DO K=1,NPETAS
            DO J=K,NPETAS
              R2E(JC,J,K)=ZERO
            END DO
          END DO
        END IF
      END DO
      
 1098 DEL=DELTA
      STIME=DTIME ! Save DTIME: Set false in case of intermediate infusions
      IF=IHEAD    ! Check if any infusions are in progress
!      
      IF (IF /= 0) THEN
        DCTR=0 ! Yes - must set up rate, duration arrays and dd array for sorting
        DTIME=.FALSE.
        DO WHILE (IF /= 0)        ! Loop over infusions
          DCTR=DCTR+1
          DD(DCTR)=IF
          JC=BETA(IF)
          IF (IA(IF) /= XR) THEN  ! Constant rate or duration is modelled for this compartment
            IF (.NOT. NOETAS) THEN 
              RE(JC,1:NPETAS) = RE(JC,1:NPETAS)+IREA(IF,1:NPETAS)
              IF (SECOND) THEN
                DO K=1,NPETAS
                  DO J=K,NPETAS
                    R2E(JC,J,K) = R2E(JC,J,K)+I2REA(IF,J,K)
                  END DO
                END DO
              END IF
            END IF
          ELSE         ! Rate is modelled for this compartment
            IF (MAPPED) THEN
              KP=IRR(MAPINV(JC))
            ELSE
              KP=IRR(JC)
            END IF
            IF (GG(KP,1) <= ZERO) THEN
              ETEXT(2)='PK PARAMETER FOR RATE IS NON-POSITIVE'
              IERPRD=1; GO TO 4000
            END IF
            IRA(IF)=GG(KP,1)
            IDA(IF)=IAA(IF)/IRA(IF)
            IF (.NOT. NOETAS) THEN
              DO K=1,NPETAS ! Loop over number of ETAs; Derivatives computed
                IREA(IF,K) = GG(KP,K+1)
                IDEA(IF,K) = (IAEA(IF,K)-IREA(IF,K)*IDA(IF))/IRA(IF)
                RE(JC,K)   = RE(JC,K)+IREA(IF,K)
              END DO
              IF (SECOND) THEN
                DO K=1,NPETAS
                  DO J=K,NPETAS
                    I2REA(IF,J,K) = G2(KP,J,K)
                    I2DEA(IF,J,K) = (I2AEA(IF,J,K)-I2REA(IF,J,K)*IDA(IF)-IREA(IF,K)   &
                                   *IDEA(IF,J)-IDEA(IF,K)*IREA(IF,J))/IRA(IF)
                    R2E(JC,J,K)   = R2E(JC,J,K)+I2REA(IF,J,K)
                  END DO
                END DO
              END IF
            END IF
          END IF
          R(JC)=R(JC)+IRA(IF)   ! Accumulate total rate for this compartment
          IF=INEXT(IF)
        END DO 
!        
        IF (DCTR /= 1) THEN     ! Now sort DD in order by duration
          DO KK=2,DCTR          ! Move smallest duration to the top
            IMOVE=0
            DO II=KK,DCTR
              JJ=DCTR-II+KK
              IF (IDA(DD(JJ-1)) < IDA(DD(JJ))) CYCLE
              IMOVE=1
              ITEMP=DD(JJ)
              DD(JJ)=DD(JJ-1)
              DD(JJ-1)=ITEMP
            END DO
            IF (IMOVE == 0) EXIT              ! Finished if no moves (array is in sort)
            IF (IDA(DD(KK-1)) > DELTA) EXIT   ! No need to continue if remaining durations GT DELTA
          END DO
        END IF
!        
        IFIN=0
        DO II=1,DCTR    ! Now loop thru sorted infusions checking for intermediate points
          IF=DD(II)
          IF (IDA(IF) > DEL) EXIT
          JC=BETA(IF)   ! Infusion is terminating
          ISPEC=IF
          R(JC)=R(JC)-IRA(IF)
          IF (.NOT. NOETAS) THEN
            RE(JC,1:NPETAS)=RE(JC,1:NPETAS)-IREA(IF,1:NPETAS)
            IF (SECOND) THEN
              DO K=1,NPETAS
                DO J=K,NPETAS
                  R2E(JC,J,K)=R2E(JC,J,K)-I2REA(IF,J,K)
                END DO
              END DO
            END IF
          END IF
          DT=DEL
          IF (II < DCTR) DT=DMIN1(DT,IDA(DD(II+1)))
          DT=(DT+IDA(IF))/TWO
          IF (SAMODE == 0) CALL ADVAN(ICALL)     ! Advance to DT !7.2b52b
          IF (IQUIT == 1) GO TO 999
          IF (IERPRD > 0) GO TO 4000
          TSTART=TSTART+DT
          DEL=DEL-DT            ! Update remaing durations
          IDA(DD(II:DCTR))=IDA(DD(II:DCTR))-DT
          IFIN=II
        END DO 
      END IF
!   
      ISPEC=0 ! Finished intermediate points or no infusions in progress
      DTIME=STIME
      DT=DEL
!      
      IF (DTIME .AND. NPETAS /= 0) THEN !7.2B51B
        ADTE(1:NPETAS)=DDELTA(1:NPETAS)
        IF (SECOND) THEN
          DO K=1,NPETAS
            DO J=K,NPETAS
              D2ADTE(J,K)=D2DELT(J,K)
            END DO
          END DO
        END IF
      END IF
!      
      IF (SAMODE == 0) CALL ADVAN(ICALL) !7.2b52b
      IF (IQUIT == 1) GO TO 999
!
      IF (IERPRD <= 0 .AND. IHEAD /= 0) THEN ! Update infusions
        IF (DCTR <= IFIN) THEN               ! All infusions ended.
          IHEAD=0
          DO II=1,DCTR
            IPOOL(IP)=DD(II)
            IP=IP-1
          END DO    
        ELSE                                 ! Update continuing infusions
          IFIN=IFIN+1
          DO I=IFIN,DCTR
            IF=DD(I)
            IDA(IF)=IDA(IF)-DEL
            IF (DTIME) THEN
              IDEA(IF,1:NPETAS)=IDEA(IF,1:NPETAS)-DDELTA(1:NPETAS)
              IF (SECOND) THEN
                DO K=1,NPETAS
                  DO J=K,NPETAS
                    I2DEA(IF,J,K)=I2DEA(IF,J,K)-D2DELT(J,K)
                  END DO
                END DO
              END IF
            END IF
            IF (IA(IF) /= XR) CYCLE
            IAA(IF)=IAA(IF)-DELTA*IRA(IF)
            IF (.NOT. NOETAS) THEN
              FORALL (K=1:NPETAS) IAEA(IF,K)=IAEA(IF,K)-DELTA*IREA(IF,K)-DDELTA(K)*IRA(IF)
              IF (SECOND) THEN
                DO K=1,NPETAS
                  DO J=K,NPETAS
                    I2AEA(IF,J,K) = I2AEA(IF,J,K)-DELTA*I2REA(IF,J,K)-DDELTA(J)*IREA(IF,K) &
                                   -D2DELT(J,K)*IRA(IF)-DDELTA(K)*IREA(IF,J)
                  END DO
                END DO
              END IF
            END IF
          END DO
          IFIN=IFIN-1 
          IF (IFIN /= 0) THEN                ! Delete finished infusions
            DO II=1,IFIN
              IF=DD(II)
              IPOOL(IP)=IF
              IP=IP-1
              IF (IHEAD /= IF) THEN
                INEXT(IBACK(IF))=INEXT(IF)
                IF (INEXT(IF) /= 0) IBACK(INEXT(IF))=IBACK(IF)
                CYCLE
              END IF
              IHEAD=INEXT(IF)
            END DO
          END IF
        END IF  
      END IF  
!      
 4000 IF (MAPPED) THEN 
! General model, some compartment(s) OFF        
! Restore compartment numbers for infusions
        IF (IHEAD /= 0) THEN
          IF=IHEAD
          DO WHILE (IF /= 0)
            BETA(IF)=MAPINV(BETA(IF))
            IF=INEXT(IF)
          END DO
        END IF
        DO II=XNCM1,1,-1  ! Uncompress state vector, status vector, and restore 1-1 mapping
          JJ=MAP(II)
          IF (JJ == 0 .AND. II <= NBRON) THEN
            AMNT(II)=ZERO
            IF (.NOT. NOETAS) THEN
              DAETA(II,1:NPETAS)=ZERO
              IF (SECOND) THEN
                DO K=1,NPETAS
                  DO J=K,NPETAS
                    D2AETA(II,J,K)=ZERO
                  END DO
                END DO
              END IF
            END IF
            SV(II)=0
          ELSE IF (JJ /= 0 .AND. JJ /= II) THEN
            AMNT(II)=AMNT(JJ)
            IF (.NOT. NOETAS) THEN
              DAETA(II,1:NPETAS)=DAETA(JJ,1:NPETAS)
              IF (SECOND) THEN
                DO K=1,NPETAS
                  DO J=K,NPETAS
                    D2AETA(II,J,K)=D2AETA(JJ,J,K)
                  END DO
                END DO
              END IF
            END IF
            SV(II)=1
          END IF
          MAP(II)=II
          MAPINV(II)=II
        END DO
!        
        NC    = XNC
        NCM1  = XNCM1
        LNCM1 = XLNCM1
        LNCM2 = XLNCM2
        MAPPED= .FALSE.
!        
        IF (COMPAC) THEN
          NIAI1=XNIAI1; NIAI=XNIAI; NIT=XNIT
          NIPI1=XNIPI1; NIPI=XNIPI; NIT1=XNIT1 
!         
          AA(1:NIAI)  = XAA(1:NIAI)
          IAI(1:NIAI) = XIAI(1:NIAI)
          IAJ(1:NIAI) = XIAJ(1:NIAI)
          IAK(1:NIAI) = XIAK(1:NIAI)         
!         
          PP(1:NIPI)  = XPP(1:NIPI)
          IPI(1:NIPI) = XIPI(1:NIPI)
          IPJ(1:NIPI) = XIPJ(1:NIPI)
          IPK(1:NIPI) = XIPK(1:NIPI)
!        
          TT(1:XNIT)  = XTT(1:XNIT)
          ITI(1:XNIT) = XITI(1:XNIT)
          ITK(1:XNIT) = XITK(1:XNIT)
        END IF
      END IF
!      
      IF (DTIME) THEN    
        ADTE(1:NPETAS)=ZERO
        IF (SECOND) THEN
          DO K=1,NPETAS
            DO J=K,NPETAS
              D2ADTE(J,K)=ZERO
            END DO
          END DO
        END IF
      END IF
!      
      TSTATE=TSTART+DELTA      
!      
  999 RETURN 
!        
      END SUBROUTINE SADVAN
