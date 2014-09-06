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
! LAST UPDATE : FEB/1991 - NUMERIC DERIVATIVES ELIMINATED 
!               JUL/2008 - COMMON BLOCKS REPLACED WITH MODULES
!               NOV/2008 - INTRODUCED HEADER INFORMATIONS AND RESTRUCTURED AS PER
!                          THE NONMEM STANDARDS
!               JAN/2009 - ASSUMED SIZE ARRAY'S REPLACED WITH ASSUMED SHAPE
!               FEB/2009 - MOVED DATA STATEMENTS TO PRDATA.F90 FILE
!               APR/2009 - INTRODUCED ERROR CHECK FOR FILE OPERATION
!               FEB/2010 - CHANGED SIZES TO PRSIZES
!               FEB/2011 - INTEGRATED 7.2BETA5.8B MODIFICATIONS
!
!----------------------------- ADVAN6.F90 -------------------------------------------
!
! SUBROUTINE ADVAN(ICALL)
!
! DESCRIPTION : ADVAN6 and ADVAN8 are routines in PREDPP's library which implement the
!               general non-linear model. The general non-linear model is used for
!               systems in which a drug is distributed between compartments according
!               to first-order processes. ADVAN8 may be used in preference to ADVAN6
!               when the differential equations describing the processes are stiff.
!               ADVAN6 is a general routine for models specified by differential 
!               equations.
!
! ARGUMENTS   : ICALL
!               IN     - ICALL
!                        ICALL - Values are 0,1,2 or 3. ICALL values 0,1,2,3 from NONMEM
!                                are unchanged. ICALL values >= 4 are converted to 2 for
!                                this routine.
!               OUT    - NONE
!               IN OUT - NONE
!
! CALLED BY   : FCN2   - FCN2 copies Y to the state vector and calls ADVAN to advance the
!                        system (SS6 already set up infusion array and DT). It inputs the SS
!                        dose over the advance interval. It then computes the difference
!                        between the initial state vector and the post-advance state vector
!                        for multiple doses-interfaces with ADVAN.
!               SADVAN - Stands for Supervisor ADVAN.  This is an interface.
!                        PREDPP contains a library of routines, called ADVAN routines,
!                        which implement specific kinetic models. Exactly one ADVAN
!                        routine must be selected for each NONMEM / PREDPP run.
!                        Its function is to "ADVANCE" the kinetic system from one
!                        state time point to the next
!               SS6    - General routine for all models
!               SS9    - General routine for all models
!               SS13   - General routine for all models
!
! CALLS       : DES             - To solve diffrential equations
!               MODEL           - Model definition information
!               TOL             - The error which is controlled by way of the parameter TOL is 
!                                 an estimate of the local truncation error, that is, the error
!                                 committed on taking a single step with the method, starting 
!                                 with data regarded as exact. 
!               DVERK1          - Differential Equation Solver - Runge-Kutta-Verner fifth and sixth
!                                 order method
!               RUN_TIME_ERRORS - Set run-time error conditions.
!               FCN1            - Evaluate YPRIME(1),...,YPRIME(N). FCN1 calls DES to obtain DADT. It
!                                 then adds in the rate of any infusions that are in progress. 
!               ERRORMSGS       - Writes error messages to a file JUNIT and sets IQUIT to 1 indicating
!                                 that NONMEM has to quit for non-super problems. And for super problems
!                                 calculation continues with next sub problem. 
!
! ALGORITHM   : - IF (ICALL == 2) Normal Entry; Else
!                 - IF (ICALL /= 0), initialization call to DES; else
!                 - Initialize variables - Initialization call as ICALL=0
!                   - Set maximum values
!                   - Call MODEL for model definition information - Call MODEL
!                   - Obtain the no. of significant digits - Call TOL
!                   - Set up DVERK1's communication vector
!               - Normal entry
!                 - Solve diffrential equations - Call DES
!                 - Call with state vector at TSTART
!                 - For all ETAS, compute only once - Subroutine to compute norm of DA
!                 - If MAPPED is TRUE then, branch over Mapped-compact arrays or 
!                   Mapped-full storage.
!                 - If MAPPED is FALSE then, branch over Not Mapped-compact arrays or 
!                   Not Mapped-full storage.
!                 - For Not Mapped-full storage, compute norm of upper left (NC-1)X(NC-1)
!                   submatrix of DA
!                 - Branch over IRET
!                 - If IRET == 1
!                   - Set I2FLAG=1 if there will be partials w.r.t time due to term. infusion
!                   - HR(I,1)=infusion rate into A(I) after terminating inf. ends
!                   - HR(I,2)=infusion rate into A(I) before it ends
!                   - Save old values if needed for output compartment
!                   - SS6 sets MFLAG8=1 when the call is from ZSPOW
!                   - Set up now for first integration interval
!                   - If error in integration to IDA(ISPEC) or DT - call RUN_TIME_ERRORS
!                   - All finished if no terminating infusion
!                   - Compute NORM at end of terminating infusion - Set IRET=2
!                   - If MAPPED is TRUE then, branch over Mapped-compact arrays or 
!                     Mapped-full storage.
!                   - If MAPPED is FALSE then, branch over Not Mapped-compact arrays or 
!                     Not Mapped-full storage.
!                 - If IRET == 2
!                   - Branch if numeric derivatives are not to be obtained
!                   - Adjustment for end point - FCN1 handles on/off, adds both infusions
!                   - Calculate second set of derivatives
!                   - Setup for second integration interval
!                   - If error in integration to IDA(ISPEC) or DT - call RUN_TIME_ERRORS
!                   - Finish integrating to TSTART+DT
!                 - Go back for another group of ETAS if necessary
!                 - Must also subtract IDEA from DTINT before re-doing the integ.
!                 - For call from SS, must return all the first derivatives
!                 - Adjust derivatives at end point - FCN1 handles ON/OFF, adds infusion
!                 - Any value greater than NCM1 will do
!                 - If error in integration to IDA(ISPEC) or DT - call RUN_TIME_ERRORS
!                 - Calculate the output compartment
!                 - Exit point
!
! MODULES USED: PRSIZES,PRDATA,NMPRD_INT,PRCM_INT,PRCOM_INT,PRCM_REAL,PRCOM_REAL,
!               PROCM_REAL,NMPRD_CHAR,PRCOM_LOG,PRCM_LOG,NM_INTERFACE,PR_INTERFACE
!
! CONTAINS    : RUN_TIME_ERRORS
!
! LOCAL'S     : C,CTEMP,CTEMPC,DADT,DADT2,I,I2FLAG,ICOMP,IER,IND,IOUT,IRET,J,K,
!               KLOOP,L,NORM,NORM1,NORM2,NORMI,NPR,NW,NY,OLDA,OLDAE2,OLDAET,PE,
!               PPW,SAVSEC,TEMP,TL,W,X,XA,XEND,Y
!

!---------------------------- END OF HEADER -----------------------------------------
!
      SUBROUTINE ADVAN(ICALL)
!
      USE PRSIZES,      ONLY: ISIZE,DPSIZE,PE,PC,MAXFCN,PG,PIR
      USE PRDATA,       ONLY: P00001,THREE,TEN
! INTEGER
      USE NMPRD_INT,    ONLY: IERPRD
      USE PRCM_INT,     ONLY: AA,IAI,INRD,LST,LEND,SLEND,SLST,SPW,SLOOP,MFLAG8,NIAI1,   &
                              XNCM1,MAP,MAPINV
      USE PRCOM_INT,    ONLY: ADVID,BETA,CALCTR,IATT,IDNO,IH,IMAX,ISPEC,LINK,LOGUNT,    &
                              MAXCAL,MCOMP,NBP,NCM1,NPETAS,NRD,SV
! REAL
      USE PRCM_REAL,    ONLY: DAC,DPC,DTC
      USE PRCOM_REAL,   ONLY: H2ETA,D2ADTE,D2TINT,I2REA,I2DEA,R2E,D2TSTA,ADTE,DA,DET,   &
                              DP,DT,DTINT,DTSTAR,HETA,HR,IDA,IDEA,IRA,IREA,R,RE,TSTART, &
                              ZERO,G3
      USE PROCM_REAL,   ONLY: AMNT,DAETA,D2AETA
! CHARACTER
      USE NMPRD_CHAR,   ONLY: ETEXT
! LOGICAL
      USE PRCOM_LOG,    ONLY: DTIME,LFLAG,DTDER,NOETAS,SECOND
      USE PRCM_LOG,     ONLY: DIDCAA,DIDDES,DOFINL,MAPPED,COMPAC
! INTERFACE
      USE NM_INTERFACE, ONLY: ERRORMSGS
      USE PR_INTERFACE, ONLY: DES,DVERK1,FCN1,MODEL,TOL
!      
      IMPLICIT NONE
!
      INTEGER(KIND=ISIZE), INTENT(IN) :: ICALL
! 
      SAVE
!
!------------------------------------------------------------------------------------
!     INCLUDE '../SIZES'
!     COMMON /NMPRD1/ IERPRD,NETEXT
!     COMMON /NMPRD2/ ETEXT(3)
!     INTEGER IERPRD,NETEXT
!     CHARACTER*132 ETEXT
!     COMMON /PRCOM0/ NP,NBP,YFORM
!     COMMON /PRCOM0/ MAXKF,IFORM
!     COMMON /PRCOM0/ IDC,IDO,MAXIC,ISV,IINST,ITURN
!     COMMON /PRCOM0/ JTIME,JCONT,JEVENT,JAMT,JRATE,JSS,JDELTA
!     COMMON /PRCOM0/ JCOMPT,JCOMPF,JERROR,SSC,KREC,JMORE,JDUM
!     COMMON /PRCM02/ ITOTL,INRD
!     INTEGER ITOTL(PC),INRD(PM)
!     COMMON /PRCOM1/ NOETAS,SECOND
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
!     COMMON /PRCOM7/ ADVID,SSID
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
!     INTEGER ISV(PC),SV(PC)
!     INTEGER IINST(PC),ITURN(PC)
!     INTEGER ADVID,SSID,MAXKF,IFORM(PG+1),YFORM,MAXIC
!     INTEGER BETA(90),IPOOL(90),IP,IHEAD,INEXT(90),IBACK(90)
!     INTEGER ISPEC,DD(90),DCTR
!     LOGICAL NOETAS,SECOND
!     COMMON /PRCMX1/ GENMOD,MAPPED,COMPAC
!     LOGICAL GENMOD,MAPPED,COMPAC
!     COMMON /PRCMX2/ XNC,XNCM1,MAP,MAPINV,NBRON,XNBRON
!     INTEGER XNC,XNCM1,MAP(PC),MAPINV(PC),NBRON,XNBRON
!     DOUBLE PRECISION XA(PM)
!     EXTERNAL FCN1,DES
!     COMMON /PROCM4/ A,DAETA,D2AETA
!     DOUBLE PRECISION A,DAETA,D2AETA
!     DIMENSION A(PC),DAETA(PC,PE),D2AETA(PC,PE,PE)
! CHANGE FOR SECOND DERIVATIVES
!     COMMON /PRCOMD/ DA,DP,DET,HR,HETA,H2ETA
!     COMMON /PRCOME/ NRD,MCOMP,NCM1,IH
!     COMMON /PRCOML/ IDNO,IATT,LINK,ILINK,INUM
!     DOUBLE PRECISION Y !,DA,DP,HR,HETA,DET,H2ETA
!     DIMENSION Y(PPW)!,DA(PM,PM),DP(PM,PG),HR(PM,2),DET(PM)
!     DIMENSION HETA(PM,PE,2),H2ETA(PM,PE,PE,2)
!     INTEGER MCOMP,NCM1,IH
!     INTEGER NY,IND,IER,NPR ! ,NRD,IDNO,,IATT,LINK,ILINK,INUM
!     DIMENSION IATT(PM,9),LINK(PM,PC),ILINK(PM,PC),INUM(PM)
!     COMMON /PRCOMT/ DTIME,LFLAG,DTDER
!     LOGICAL DTIME,LFLAG,DTDER
!     COMMON /PRCOMF/ MAXCAL,CALCTR
!     INTEGER MAXCAL,CALCTR
! FOR LOOPS ON ETAS IN ADVAN6, ADVAN8, SS6
!     COMMON /PRCMET/ LST,LEND,SLEND(PE),SLST(PE),SPW,SLOOP
!     INTEGER LST,LEND,SLEND,SLST,SPW,SLOOP
! COUNTERS FOR INDICES FOR THE COMPACT ARRAYS (X VERSIONS ARE UNMAPPED)
!     COMMON /PRCMDE/ NIAI,NIPI,NIT,XNIAI,XNIPI,XNIT
!     INTEGER NIAI,NIPI,NIT,XNIAI,XNIPI,XNIT
! INDICES FOR THE COMPACT ARRAYS (X VERSIONS ARE UNMAPPED)
!     COMMON /PRCMDE/ IAI(PIR),IAJ(PIR),IAK(PIR)
!     COMMON /PRCMDE/ IPI(PIR),IPJ(PIR),IPK(PIR)
!     COMMON /PRCMDE/ ITI(PIR),ITK(PIR)
!     COMMON /PRCMDE/ XIAI(PIR),XIAJ(PIR),XIAK(PIR)
!     COMMON /PRCMDE/ XIPI(PIR),XIPJ(PIR),XIPK(PIR)
!     COMMON /PRCMDE/ XITI(PIR),XITK(PIR)
!     COMMON /PRCMDE/ IAC(PIR),IPC(PIR),ITC(PIR)
!     COMMON /PRCMDE/ NIAI1,NIPI1,NIT1,XNIAI1,XNIPI1,XNIT1
!     COMMON /PRCMDE/ AA,PP,TT,XAA,XPP,XTT
!     INTEGER NIAI1,NIPI1,NIT1,XNIAI1,XNIPI1,XNIT1
!     INTEGER AA(PIR),PP(PIR),TT(PIR),XAA(PIR),XPP(PIR),XTT(PIR)
!     INTEGER IAI,IAJ,IAK,IPI,IPJ,IPK,ITI,ITK
!     INTEGER XIAI,XIAJ,XIAK,XIPI,XIPJ,XIPK,XITI,XITK
!     INTEGER IAC,IPC,ITC
! THE COMPACT ARRAYS THEMSELVES
!     COMMON /PRCMDC/ DAC(PIR,1),DPC(PIR,1),DTC(PIR)
!     DOUBLE PRECISION DAC,DPC,DTC
!     COMMON /PRCOMG/ MITER,METH,IMAX,ISTFLG,INTFLG
!     INTEGER MITER,METH,IMAX,ISTFLG,INTFLG
!     COMMON /PRCOMX/ DTINT(PE),D2TINT(PE,PE)
!     DOUBLE PRECISION DTINT,D2TINT
!     COMMON /PRCMLX/ MFLAG1,MFLAG2,MFLAG3,MFLAG4,MFLAG5,MFLAG6
!     COMMON /PRCMLX/ MFLAG7,MFLAG8,SS3
!     INTEGER MFLAG1,MFLAG2,MFLAG3,MFLAG4,MFLAG5,MFLAG6
!     INTEGER MFLAG7,MFLAG8,SS3
!     COMMON /PRCM03/ DIDCAA,DIDDES,DIDAES,DOFINL
!     LOGICAL DIDCAA,DIDDES,DIDAES,DOFINL
!------------------------------------------------------------------------------------
!
! Local Variables
!
      INTEGER(KIND=ISIZE), PARAMETER :: PM=PC-1,PPW=2*PE*PM+PM
      INTEGER(KIND=ISIZE) :: I,I2FLAG,ICOMP,IER,IND,IOUT,IRET,J,K,KLOOP,L,NPR,NW,NY
!
      REAL(KIND=DPSIZE)   :: C,DADT,DADT2,NORM,NORM1,NORM2,NORMI,OLDA,OLDAE2,OLDAET, &
                             TEMP,TL,W,X,XA,XEND,Y
!
      CHARACTER(LEN=3)    :: CTEMP
      CHARACTER(LEN=12)   :: CTEMPC
!
      LOGICAL :: SAVSEC
!
      DIMENSION :: Y(PPW),C(24),OLDA(PM),XA(PM),NORMI(PM),DADT(PPW),DADT2(PPW),      &
                   W(PPW,9),OLDAET(PM,PE),OLDAE2(PM,PE,PE)
!
!
! Variables used to advance by DT; For infusions into primary compartment 
! which continue past DT:
!   R(IDC)       = Infusion rate into primary compartment
!   RE(IDC,K)    = Derivative of rate w.r.t ETA(K)
! If an infusion into primary compt. is terminating, ISPEC>0 and:
!  IRA(ISPEC)    = Its infusion rate
!  IREA(ISPEC,K) = Derivative of IRA w.r.t ETA(K)
!  IDA(ISPEC)    = Its duration
!  IDEA(ISPEC,K) = Derivative of IDEA w.r.t ETA(K)
!  BETA(ISPEC)   = Its compartment number.
!  TSTART        = Starting time of interval over which to advance
!
! For implementation of general ADVANS' compartment on/off feature
! GENMOD true for general ADVAN (set by PREDI)
! XNC,XNCM1,XNBRON are original values of NC,NCM1,NBRON (set by PREDI)
! NBRON tells how many compartments are now on (not counting output)
! MAPPED Is true when a mapping in effect (set by SADVAN, SSS)
! MAP, MAPINV: MAPS 'REAL' Compartment nos. to 'REDUCED SET' and V.V.
!
! Variables supplied by user's subroutine DES:
! DADT(I) = Derivative of A(I) w.r.t time T
! DA(I,K) = Derivative of DADT(I) w.r.t A(K)
! DP(I,J) = Derivative of DADT(I) w.r.t GG(J)
!
! DADT must be larger than PM for the adjustment when LFLAG is true
!
      IF (ICALL /= 2) THEN
        IF (ICALL == 0) THEN    ! Initialization call    
          ADVID=6; SPW=PPW
! Set maximum values
          DET(1:MCOMP)=ZERO  
          DADT(1:MCOMP)=ZERO
          FORALL(I=1:MCOMP,J=1:9) IATT(I,J)=0
          FORALL(I=1:MCOMP,J=1:PG) DP(I,J)=ZERO
          FORALL(I=1:MCOMP,J=1:MCOMP) DA(I,J)=ZERO
          FORALL(I=1:MCOMP,J=1:MCOMP+1) LINK(I,J)=0
          Y(1:PPW)=ZERO
! Call for MODEL definition information
          CALL MODEL(IDNO,NCM1,NPR,MCOMP,IATT,LINK) 
          IF (IERPRD <= 0) THEN
            WRITE (LOGUNT,125,ERR=502)
            NBP=NPR; IOUT=NCM1+1
            INRD(1:MCOMP)=0
            CALL TOL(INRD)             ! Obtain the no. of significant digits
            IF (IERPRD <= 0) THEN
              NRD=INRD(1)
              NW=PPW
              TL=TEN**(-NRD)
              C(1:9)=ZERO              ! Set up DVERK1'S communication vector
              C(1)=THREE
              C(2)=P00001
            END IF  
          END IF
        ELSE    ! Added 4/94. Allows initialization call to DES
          CALL DES(AMNT,G3,TSTART,DADT,PIR,DAC,DPC,DTC)             
        END IF  
        GO TO 999
      END IF  
!
! Normal Entry
      IF (DOFINL) THEN
        IF (COMPAC) THEN
          CALL DES(AMNT,G3,TSTART,DADT,PIR,DAC,DPC,DTC)
        ELSE
          CALL DES(AMNT,G3,TSTART,DADT,MCOMP,DA,DP,DET)
        END IF
        DIDDES=.FALSE.
        GO TO 999
      END IF
!
! Not a special call
      DIDCAA=.TRUE.
      DIDDES=.TRUE.
      NY=NCM1*(NPETAS+1)
!      
      IF (IMAX == -1) THEN
        MAXCAL=MAXFCN
        IF (MAXCAL == 0) MAXCAL=1000000  ! Default added 11/2007 in case old SIZES is used
      ELSE
        MAXCAL=IMAX
      END IF
!
! Caution: Assumes that PREDI intializes to zero originally
      IF (LFLAG .AND. (.NOT. NOETAS)) THEN
        DTINT(1:NPETAS)=DTSTAR(1:NPETAS)
      END IF
!
      NORM2=ZERO
      IRET=1    ! Call with state vector at TSTART
!
! It is the same for all ETAS, compute only once; Subroutine to compute norm of DA
      IF (MAPPED) THEN      ! Mapped
        DO I=XNCM1,1,-1
          IF (MAP(I) == 0) THEN
            XA(I)=ZERO
          ELSE
            XA(I)=AMNT(MAP(I))
          END IF
        END DO
!
        IF (COMPAC) THEN    ! Mapped, compact arrays
          CALL DES(XA,G3,TSTART,DADT,PIR,DAC,DPC,DTC)
          IF (IERPRD > 0) GO TO 999
          NORMI(1:NCM1)=ZERO
          DO K=1,NIAI1
            NORMI(IAI(K))=NORMI(IAI(K))+ABS(DAC(AA(K),1))
          END DO  
          NORM=NORMI(1)
          DO I=2,NCM1
            IF (NORM < NORMI(I)) NORM=NORMI(I)
          END DO
        ELSE                ! Mapped, full storage
          CALL DES(XA,G3,TSTART,DADT,MCOMP,DA,DP,DET)
          IF (IERPRD > 0) GO TO 999
          NORM=ZERO
          DO I=1,NCM1
            TEMP=ZERO
            DO J=1,NCM1
              TEMP=TEMP+ABS(DA(MAPINV(I),MAPINV(J)))
            END DO
            IF (NORM < TEMP) NORM=TEMP
          END DO
        END IF  
      ELSE                  ! Not Mapped
        IF (COMPAC) THEN    ! Not mapped, compact storage
          CALL DES(AMNT,G3,TSTART,DADT,PIR,DAC,DPC,DTC)
          IF (IERPRD > 0) GO TO 999
          NORMI(1:NCM1)=ZERO
          DO K=1,NIAI1
            NORMI(IAI(K))=NORMI(IAI(K))+ABS(DAC(AA(K),1))
          END DO  
          NORM=NORMI(1)
          DO I=2,NCM1
            IF (NORM < NORMI(I)) NORM=NORMI(I)
          END DO
        ELSE                ! Not mapped, full storage
          CALL DES(AMNT,G3,TSTART,DADT,MCOMP,DA,DP,DET)
          IF (IERPRD > 0) GO TO 999
! Compute norm of upper left (NC-1)X(NC-1) submatrix of DA
          NORM=ZERO
          DO I=1,NCM1
            TEMP=ZERO
            DO J=1,NCM1
              TEMP=TEMP+ABS(DA(I,J))
            END DO
            IF (NORM < TEMP) NORM=TEMP
          END DO
        END IF  
      END IF  
!
  111 SELECT CASE (IRET)
      CASE (1)
        GO TO 1020
      CASE (2)
        NORM2=NORM 
        GO TO 1410
      END SELECT
!
 1020 NORM1=NORM
      I2FLAG=0  ! Set I2FLAG=1 if there will be partials w.r.t time due to term. infusion
      DTDER=LFLAG .OR. DTIME
!
      IF (ISPEC /= 0 .AND. (.NOT. NOETAS)) THEN
        DO L=1,NPETAS
          IF (IDEA(ISPEC,L) /= ZERO) THEN 
            I2FLAG=1
            DTDER=.TRUE.
            EXIT
          END IF
        END DO
      END IF        
!
! HR(I,1)=infusion rate into A(I) after terminating inf. ends
! HR(I,2)=infusion rate into A(I) before it ends
!
      HR(1:NCM1,1)=R(1:NCM1)
      HR(1:NCM1,2)=R(1:NCM1)
!
      IF (.NOT. NOETAS) THEN
        FORALL(I=1:NCM1,L=1:NPETAS) HETA(I,L,1)=RE(I,L)
        FORALL(I=1:NCM1,L=1:NPETAS) HETA(I,L,2)=RE(I,L)
      END IF  
!
      IF (ISPEC /= 0) THEN
        ICOMP=BETA(ISPEC)
        HR(ICOMP,2)=HR(ICOMP,2)+IRA(ISPEC)
        IF (.NOT. NOETAS) THEN
          DO L=1,NPETAS
            HETA(ICOMP,L,2)=HETA(ICOMP,L,2)+IREA(ISPEC,L)
          END DO  
        END IF  
      END IF  
!
! Save old values if needed for output compartment
      IF (SV(IOUT) /= 0) THEN
        DO I=1,NCM1
          OLDA(I)=AMNT(I)
          IF (NOETAS) CYCLE
          DO L=1,NPETAS
            OLDAET(I,L)=DAETA(I,L)
          END DO
        END DO
      END IF  
!
      IF (.NOT. NOETAS) THEN
        IF (SECOND) THEN
          IF (ISPEC /= 0 .AND. I2FLAG /= 1) THEN
            OUTER: DO L=1,NPETAS
              DO J=L,NPETAS
                IF (I2DEA(ISPEC,J,L) /= ZERO) THEN
                   I2FLAG=1
                   DTDER=.TRUE.
                   EXIT OUTER
                END IF
              END DO
            END DO OUTER
          END IF  
!
          DO L=1,NPETAS
            DO J=L,NPETAS
              DO I=1,NCM1
                H2ETA(I,J,L,1)=R2E(I,J,L)
                H2ETA(I,J,L,2)=R2E(I,J,L)
              END DO
            END DO
          END DO
!          
          IF (ISPEC /= 0) THEN
            DO L=1,NPETAS
              DO J=L,NPETAS
                H2ETA(ICOMP,J,L,2)=H2ETA(ICOMP,J,L,2)+I2REA(ISPEC,J,L)
              END DO
            END DO
          END IF
!          
          IF (SV(IOUT) /= 0) THEN
            DO L=1,NPETAS
              DO J=L,NPETAS
                DO I=1,NCM1
                  OLDAE2(I,J,L)=D2AETA(I,J,L)
                END DO
              END DO
            END DO
          END IF
!          
          IF (LFLAG) THEN
            DO L=1,NPETAS
              DO J=L,NPETAS
                D2TINT(J,L)=D2TSTA(J,L)
              END DO
            END DO
          END IF
        END IF  
!
! SS6 sets MFLAG8=1 when the call is from ZSPOW
        IF (MFLAG8 /= 1) THEN
          IF (.NOT. SECOND) THEN
            LST=1; LEND=NPETAS
          ELSE
            KLOOP=SLOOP
            LEND=SLEND(KLOOP)
            LST=SLST(KLOOP)
          END IF  
        END IF  
      END IF
!
! Set up now for first integration interval
 1110 Y(1:NCM1)=AMNT(1:NCM1)
! 
      IF (.NOT. NOETAS) THEN
        K=0
        DO L=1,LEND
          K=K+NCM1
          DO I=1,NCM1
            Y(K+I)=DAETA(I,L)
          END DO  
        END DO
        IF (SECOND) THEN
          DO L=LST,LEND
            DO J=1,L
              K=K+NCM1
              DO I=1,NCM1
                Y(K+I)=D2AETA(I,L,J)
              END DO  
            END DO
          END DO
          NY=K+NCM1
        END IF
      END IF  
!
      IH=2; IND=2
      X=TSTART
      XEND=X+DT
      IF (ISPEC /= 0) XEND=X+IDA(ISPEC)
      IF (XEND > X) THEN
        CALCTR=0
        C(5)=NORM1
        CALL DVERK1(NY,FCN1,X,Y,XEND,TL,IND,C,NW,W,IER)
        IF (IERPRD > 0) THEN
          IF (IERPRD == 999) CALL RUN_TIME_ERRORS(2)
          GO TO 999
        END IF   
      END IF
      IF (IND <= -2) THEN 
         CALL RUN_TIME_ERRORS(1); GO TO 999
      END IF   
!
! All finished if no terminating infusion
      IF (ISPEC == 0) GO TO 1450
!
      IF (NORM2 == ZERO) THEN
        IRET=2                ! Compute NORM at end of terminating infusion
!        
        IF (MAPPED) THEN      ! Mapped
          DO I=XNCM1,1,-1
            IF (MAP(I) == 0) THEN
              XA(I)=ZERO
            ELSE
              XA(I)=Y(MAP(I))
            END IF
          END DO
!
          IF (COMPAC) THEN    ! Mapped compact storage
            CALL DES(XA,G3,X,DADT,PIR,DAC,DPC,DTC)
            IF (IERPRD > 0) GO TO 999
            NORMI(1:NCM1)=ZERO
            DO K=1,NIAI1
              NORMI(IAI(K))=NORMI(IAI(K))+ABS(DAC(AA(K),1))
            END DO  
            NORM=NORMI(1)
            DO I=2,NCM1
              IF (NORM < NORMI(I)) NORM=NORMI(I)
            END DO
          ELSE                ! Mapped full storage                  
            CALL DES(XA,G3,X,DADT,MCOMP,DA,DP,DET)
            IF (IERPRD > 0) GO TO 999
            NORM=ZERO
            DO I=1,NCM1
              TEMP=ZERO
              DO J=1,NCM1
                TEMP=TEMP+ABS(DA(MAPINV(I),MAPINV(J)))
              END DO
              IF (NORM < TEMP) NORM=TEMP
            END DO
          END IF
        ELSE                  ! Not Mapped
          IF (COMPAC) THEN    ! Not Mapped compact storage
            CALL DES(Y,G3,X,DADT,PIR,DAC,DPC,DTC)
            IF (IERPRD > 0) GO TO 999
            NORMI(1:NCM1)=ZERO
            DO K=1,NIAI1
              NORMI(IAI(K))=NORMI(IAI(K))+ABS(DAC(AA(K),1))
            END DO  
            NORM=NORMI(1)
            DO I=2,NCM1
              IF (NORM < NORMI(I)) NORM=NORMI(I)
            END DO
          ELSE                ! Not Mapped full storage
            CALL DES(Y,G3,X,DADT,MCOMP,DA,DP,DET)
            IF (IERPRD > 0) GO TO 999
            NORM=ZERO         ! Compute norm of upper left (NC-1)X(NC-1) submatrix of DA
            DO I=1,NCM1
              TEMP=ZERO
              DO J=1,NCM1
                TEMP=TEMP+ABS(DA(I,J))
              END DO
              IF (NORM < TEMP) NORM=TEMP
            END DO
          END IF  
        END IF
        GO TO 111
      END IF
!
 1410 CONTINUE
!
! Branch if numeric derivatives are not to be obtained
      IF (I2FLAG /= 0) THEN
! Adjustment for end point - FCN1 handles on/off, adds both infusions
        SAVSEC=SECOND
        SECOND=.FALSE.
        CALCTR=0
        CALL FCN1(Y,DADT,NY,XEND)
        IF (IERPRD > 0) THEN
          IF (IERPRD == 999) CALL RUN_TIME_ERRORS(2)
          GO TO 999
        END IF   
!
        FORALL(L=1:NPETAS) DTINT(L)=DTINT(L)+IDEA(ISPEC,L)
        K=0
        DO J=1,LEND
          K=K+NCM1
          DO I=1,NCM1
            Y(K+I)=Y(K+I)+DADT(I)*IDEA(ISPEC,J)
          END DO  
        END DO
!
        IF (SAVSEC) THEN     ! Second set of derivatives
          DO L=LST,LEND
            DO J=1,L
              D2TINT(L,J)=D2TINT(L,J)+I2DEA(ISPEC,L,J)
            END DO
          END DO
          CALCTR=0
          CALL FCN1(Y,DADT2,NY,XEND)
          IF (IERPRD > 0) THEN
            IF (IERPRD == 999) CALL RUN_TIME_ERRORS(2)
            GO TO 999
          END IF   
!
          DO L=LST,LEND
            DO J=1,L
              K=K+NCM1
              DO I=1,NCM1
                Y(K+I)= Y(K+I)+DADT2(J*NCM1+I)*IDEA(ISPEC,L)+DADT(I)*I2DEA(ISPEC,L,J) &
                       +DADT(L*NCM1+I)*IDEA(ISPEC,J)
              END DO         
            END DO
          END DO
        END IF  
        SECOND=SAVSEC
      END IF  
!
! Start of second integration interval
      X=TSTART+IDA(ISPEC)
      XEND=TSTART+DT
      IND=2; IH=1
      IF (XEND > X) THEN
        CALCTR=0
        C(5)=NORM2
        CALL DVERK1(NY,FCN1,X,Y,XEND,TL,IND,C,NW,W,IER)
        IF (IERPRD > 0) THEN
          IF (IERPRD == 999) CALL RUN_TIME_ERRORS(2)
          GO TO 999
        END IF
      END IF
      IF (IND <= -2) THEN 
         CALL RUN_TIME_ERRORS(1); GO TO 999
      END IF   
! Finished integrating to TSTART+DT
!
 1450 CONTINUE
!
      IF (.NOT. NOETAS) THEN
        K=NCM1*(LST-1)
        DO L=LST,LEND
          K=K+NCM1
          DO I=1,NCM1
            DAETA(I,L)=Y(K+I)
          END DO
        END DO
        IF (SECOND) THEN
          DO L=LST,LEND
            DO J=1,L
              K=K+NCM1
              DO I=1,NCM1
                D2AETA(I,L,J)=Y(K+I)
              END DO
            END DO
          END DO
! Go back for another group of ETAS if necessary
! Must also subtract IDEA from DTINT before re-doing the integ.
          IF (MFLAG8 == 0) THEN
            KLOOP=KLOOP-1
            IF (KLOOP >= 1) THEN
              IF (ISPEC /= 0) THEN
                FORALL (L=1:NPETAS) DTINT(L)=DTINT(L)-IDEA(ISPEC,L)
                DO L=LST,LEND
                  DO J=1,L
                    D2TINT(L,J)=D2TINT(L,J)-I2DEA(ISPEC,L,J)
                  END DO
                END DO
              END IF
              LEND=SLEND(KLOOP)
              LST=SLST(KLOOP)
              GO TO 1110
            END IF
          ELSE
! For call from SS, must return all the first derivatives
            IF (LST > 1) THEN
              K=0
              DO L=1,LST-1
                K=K+NCM1
                DO I=1,NCM1
                  DAETA(I,L)=Y(K+I)
                END DO
              END DO
            END IF
          END IF  
        END IF
      END IF  
!
      AMNT(1:NCM1)=Y(1:NCM1)
!
      IF (I2FLAG == 1 .OR. DTIME) THEN 
! Adjust derivatives at end point - FCN1 handles ON/OFF, adds infusion
        IF (MFLAG8 == 0) THEN
          LST=1;  LEND=NPETAS
        END IF
        SAVSEC=SECOND
        SECOND=.FALSE.
!
! Any value greater than NCM1 will do
        NY=2*NCM1
        Y(1:NCM1)=AMNT(1:NCM1)
        K=0
        DO L=1,NPETAS
          K=K+NCM1
          DO I=1,NCM1
            Y(K+I)=DAETA(I,L)
          END DO
        END DO
!
        CALCTR=0
        CALL FCN1(Y,DADT,NY,XEND)
        IF (IERPRD > 0) THEN
          IF (IERPRD == 999) CALL RUN_TIME_ERRORS(2)
          GO TO 999
        END IF   
!
        DO L=1,NPETAS
          IF (ISPEC > 0) THEN
            DO I=1,NCM1
              DAETA(I,L)=DAETA(I,L)+DADT(I)*(ADTE(L)-IDEA(ISPEC,L))
            END DO
          ELSE
            DO I=1,NCM1
              DAETA(I,L)=DAETA(I,L)+DADT(I)*ADTE(L)
            END DO
          END IF
        END DO
!
        IF (SAVSEC) THEN
          K=0
          DO L=1,NPETAS
            K=K+NCM1
            DO I=1,NCM1
              Y(K+I)=DAETA(I,L)
            END DO
          END DO
          IF (ISPEC /= 0) THEN
            FORALL(L=1:NPETAS) DTINT(L)=DTINT(L)-IDEA(ISPEC,L)+ADTE(L)
          ELSE
            FORALL(L=1:NPETAS) DTINT(L)=DTINT(L)+ADTE(L)
          END IF
          CALCTR=0
          CALL FCN1(Y,DADT2,NY,XEND)
          IF (IERPRD > 0) THEN
            IF (IERPRD == 999) CALL RUN_TIME_ERRORS(2)
            GO TO 999
          END IF   
!
          DO L=LST,LEND
            DO J=1,L
              DO I=1,NCM1
                IF (ISPEC > 0) THEN
                  D2AETA(I,L,J)= D2AETA(I,L,J)+DADT2(J*NCM1+I)*(ADTE(L)-IDEA(ISPEC,L))  &
                                +DADT(I)*(D2ADTE(L,J)-I2DEA(ISPEC,L,J))                 &
                                +DADT(L*NCM1+I)*(ADTE(J)-IDEA(ISPEC,J))
                ELSE
                  D2AETA(I,L,J)= D2AETA(I,L,J)+DADT2(J*NCM1+I)*ADTE(L)                  &
                                +DADT(I)*D2ADTE(L,J)+DADT(L*NCM1+I)*ADTE(J)
                END IF
              END DO
            END DO
          END DO
        END IF  
        SECOND=SAVSEC
      END IF  
!
! Calculate the output compartment
      IF (SV(IOUT) /= 0) THEN
        DO I=1,NCM1
          AMNT(IOUT)=AMNT(IOUT)+OLDA(I)-AMNT(I)+R(I)*DT
        END DO
!
        IF (.NOT. NOETAS) THEN
          DO L=1,NPETAS
            DO I=1,NCM1
              DAETA(IOUT,L)=DAETA(IOUT,L)+OLDAET(I,L)-DAETA(I,L)+RE(I,L)*DT+R(I)*ADTE(L)
            END DO
          END DO
          IF (SECOND) THEN
            DO L=1,NPETAS
              DO J=L,NPETAS
                DO I=1,NCM1
                  D2AETA(IOUT,J,L)=D2AETA(IOUT,J,L)+OLDAE2(I,J,L)-D2AETA(I,J,L)             &
                                   +R2E(I,J,L)*DT+RE(I,J)*ADTE(L)+RE(I,L)*ADTE(J)           &
                                   +R(I)*D2ADTE(J,L)
                END DO
              END DO
            END DO
          END IF
        END IF  
!
        IF (ISPEC /= 0) THEN
          AMNT(IOUT)=AMNT(IOUT)+IRA(ISPEC)*IDA(ISPEC)
          IF (.NOT. NOETAS) THEN 
            DO L=1,NPETAS
              DAETA(IOUT,L)= DAETA(IOUT,L)+IRA(ISPEC)*IDEA(ISPEC,L)+IDA(ISPEC)*IREA(ISPEC,L)
            END DO  
            IF (SECOND) THEN 
              DO L=1,NPETAS
                DO J=L,NPETAS
                  D2AETA(IOUT,J,L)= D2AETA(IOUT,J,L)+I2DEA(ISPEC,J,L)*IRA(ISPEC)            &
                                   +IDEA(ISPEC,J)*IREA(ISPEC,L)+IDEA(ISPEC,L)*IREA(ISPEC,J) &
                                   +IDA(ISPEC)*I2REA(ISPEC,J,L)
                END DO
              END DO
            END IF  
          END IF        
        END IF
      END IF  
!
! Exit point 
      IF (LFLAG .OR. I2FLAG /= 0 .OR. DTIME) THEN
        DTINT(1:NPETAS)=ZERO
        IF (SECOND) THEN
          DO L=1,NPETAS
            DO J=L,NPETAS
              D2TINT(J,L)=ZERO
            END DO
          END DO
        END IF
      END IF
!
  125 FORMAT (' GENERAL NONLINEAR KINETICS MODEL (ADVAN6)')
!
  999 RETURN
!
  502 CALL ERRORMSGS(502,FILENAME='OUTPUT')
!
      CONTAINS
!
! RUN-TIME error conditions; Error in integration to IDA(ISPEC) or DT
!
        SUBROUTINE RUN_TIME_ERRORS(ITEMP)
!
          INTEGER, INTENT(IN) :: ITEMP
!
          SELECT CASE(ITEMP)
          CASE(1)
            ETEXT(1)='NUMERICAL DIFFICULTIES WITH INTEGRATION ROUTINE.'
            WRITE (CTEMP,'(I3)') NRD
            ETEXT(2)='NO. OF REQUIRED SIGNIFICANT DIGITS IN SOLUTION VECTOR'
            ETEXT(3)='TO DIFFERENTIAL EQUATIONS, '//CTEMP// ', MAY BE TOO LARGE.'
          CASE(2)  
            ETEXT(2)='NUMERICAL DIFFICULTIES WITH INTEGRATION ROUTINE.'
            WRITE (CTEMPC,'(I12)') MAXCAL
            ETEXT(3)='MAXIMUM NO. OF EVALUATIONS OF DIFFERENTIAL EQUATIONS, '            &
            //CTEMPC//', EXCEEDED.'
          END SELECT 
!
          IERPRD=1
!
  999     RETURN   
!       
        END SUBROUTINE RUN_TIME_ERRORS
!
      END SUBROUTINE ADVAN
