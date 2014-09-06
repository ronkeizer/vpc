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
! CREATED ON  : APR/1984
! LANGUAGE    : FORTRAN 90/95
! LAST UPDATE : JUL/2008 - COMMON BLOCKS REPLACED WITH MODULES
!               NOV/2008 - INTRODUCED HEADER INFORMATIONS AND RESTRUCTURED AS PER
!                          THE NONMEM STANDARDS
!               JAN/2009 - ASSUMED SIZE ARRAY'S REPLACED WITH ASSUMED SHAPE
!               FEB/2009 - MOVED DATA STATEMENTS TO PRDATA.F90 FILE
!               APR/2009 - MOVED ERROR MESSAGES TO ERRORMSGS.F90 FILE
!                        - INTRODUCED ERROR CHECK FOR FILE OPERATION
!               JUN/2009 - ASSUMED SHAPE ARRAY'S REPLACED WITH ASSUMED SIZE FOR BETTER
!                          PERFORMANCE
!               FEB/2010 - CHANGED SIZES TO PRSIZES
!               FEB/2011 - INTEGRATED 7.2BETA5.8B MODIFICATIONS
!               AUG/2013 - INTEGRATED NONMEM7.3a7.1 CHANGES
!
!----------------------------- CHECK.F90 --------------------------------------------
!
! SUBROUTINE CHECK(DATREC)
!
! DESCRIPTION : Default data checker routine
!
! ARGUMENTS   : DATREC
!               IN     - DATREC
!                        DATREC - Contains data record of individual
!               OUT    - NONE
!               IN OUT - NONE
!
! CALLED BY   : PREDI - Initialization-Finalization routine of PRED
!
! CALLS       : PASS      - Read and/or to modify ("transgenerate") data records
!               ERRORMSGS - Writes error messages to a file JUNIT and sets IQUIT to 1 indicating
!                           that NONMEM has to quit for non-super problems. And for super problems
!                           calculation continues with next sub problem
!
! ALGORITHM   : - Initialize variables
!               - In case of output error, skip to label 502 to display right
!                 error message - Call ERRORMSGS
!               - Check array sizes from user subroutines, if error quit NONMEM -
!                 Set IQUIT=1 and Return
!               - Checks on INDXS values
!               - If error in INDXS quit NONMEM - Set IQUIT=1 and Return
!               - Initialize PASS
!               - Loop through data records
!                 - If(error in data records) terminate NONMEM - Set IQUIT=1 and return
!                 - Initialize for new individual
!                 - Check individual data record items
!                 - Check event record
!                 - Allow 100 as the default output compartment
!                 - Only permit CALL=10,11,12,13 with ADVAN9
!                 - Check for serious errors in observation event, dose event, all doses,
!                   reset and other events.
!                 - Branch on Event records to skip to Observation event or Dose
!                   event or Other event or Reset event
!                 - For dose and other event - check compartment for observation
!               - End of loop through data records
!
! MODULES USED: PRSIZES,PRDATA,NMPRD_INT,PRCM_INT,PRCOM_INT,NM_INTERFACE
!
! CONTAINS    : NONE
!
! LOCAL'S     : AMT,CKSV,CTEMP,DEL,EVENT,I,IC,IERR,JDEF,KC,KERR,KIND,MAXC,MAXERR,MODE,
!               MORE,PE,RATE,SS,T1,T2,TREC,XD,ISDEFOUT
!
!---------------------------- END OF HEADER -----------------------------------------
!
      SUBROUTINE CHECK(DATREC)
!      
      USE PRSIZES,      ONLY: ISIZE,DPSIZE,PE,PC,PG,PD
!
      USE PRDATA,       ONLY: SUBNAM
! INTEGER
      USE NMPRD_INT,    ONLY: IDXMDV,IPS,NWIND,IQUIT
      USE PRCM_INT,     ONLY: PRMC,PRME,PRMG,PRMT
      USE PRCOM_INT,    ONLY: ADVID,ID,IDC,IDO,IINST,IRR,IS,ISV,ITURN,JAMT,JCOMPF,JCOMPT, &
                              JCONT,JDELTA,JERROR,JEVENT,JMORE,JRATE,JSS,JTIME,KREC,      &
                              LOGUNT,NC,SSID
! INTERFACE
      USE NM_INTERFACE, ONLY: ERRORMSGS
!
      IMPLICIT NONE
!      
      REAL(KIND=DPSIZE), INTENT(IN) :: DATREC(*)     
!
      SAVE
!      
!------------------------------------------------------------------------------------
!     COMMON /ROCM29/ IPS
!     INTEGER IPS
!     INTEGER NWIND,MODE
!     COMMON /CM5/ IDUM1,IXPRED,IDAT,IDUM2(6)
!     COMMON /CM5/ IC0501(3),IC0502(21)
!     INTEGER IC0501,IC0502
!     COMMON /ROCM34/ NWIND
!     INTEGER LOGUNT,IDAT
!     INTEGER IDUM1,IXPRED,IDUM2
!     INTEGER MODE
!     DIMENSION DATREC(*)
!     DOUBLE PRECISION DATREC
!     COMMON /PRCM00/ PRMC(6),PRME(6),PRMG(6),PRMT(6)
!     INTEGER PRMC,PRME,PRMG,PRMT
!     COMMON /PRCOM0/ NP,NBP,YFORM
!     COMMON /PRCOM0/ MAXKF,IFORM
!     COMMON /PRCOM0/ IDC,IDO,MAXIC,ISV,IINST,ITURN
!     COMMON /PRCOM0/ JTIME,JCONT,JEVENT,JAMT,JRATE,JSS,JDELTA
!     COMMON /PRCOM0/ JCOMPT,JCOMPF,JERROR,SSC,KREC,JMORE,JDUM
!     COMMON /PRCOM2/ IBF,IRR,IS,ID,ITSC,IFR,ILAG
!     COMMON /PRCOM7/ ADVID,SSID
!     COMMON /PRCOMN/ LOGUNT,NC
!     INTEGER LOGUNT
!     INTEGER JCONT,JTIME,JEVENT,JAMT,JRATE,JSS,JDELTA
!     INTEGER JCOMPT,JCOMPF,JERROR
!     INTEGER NC,IDC,IDO,NP,NBP,SSC,KREC,JMORE,JDUM
!     INTEGER ISV(PC),IBF(PC),IRR(PC)
!     INTEGER IINST(PC),ITURN(PC),ITSC,IFR,ILAG(PC),IS(PC),ID(PC)
!     INTEGER ADVID,SSID,MAXKF,IFORM(PG+1),YFORM,MAXIC
!------------------------------------------------------------------------------------
!
! Local Variables
!
      INTEGER(KIND=ISIZE) :: MODE,JDEF,IERR,MAXERR,IC,EVENT,KERR,KC,MAXC,KIND,CKSV(PC)
!                             
! Too much trouble to pick up XD from PRCOM4 -- AJB 12/90
      REAL(KIND=DPSIZE)   :: CTEMP,T1,T2,AMT,SS,DEL,RATE,MORE,TREC(0:PD+1),XD
!
      LOGICAL :: ISDEFOUT
!     
      IERR=0; XD=-2.0; TREC(0)=0.0;  T1=0.0; MAXERR=50
      KERR=0; JDEF=PD+1; TREC(PD+1)=0.0; T2=0.0
!          
! First check array sizes from user subroutines
! This really belongs in PREDI, but it was getting too big
!      DO I=1,6
!        IF (PRMC(I) /= 0 .AND. PRMC(I) /= PC) THEN
!          WRITE (LOGUNT,8101,ERR=502) PC,'PC',SUBNAM(I),PRMC(I)
!          IERR=1
!        END IF
!        IF (PRME(I) /= 0 .AND. PRME(I) /= PE) THEN
!          WRITE (LOGUNT,8101,ERR=502) PE,'LVR',SUBNAM(I),PRME(I)
!          IERR=1
!        END IF
!        IF (PRMG(I) /= 0 .AND. PRMG(I) /= PG) THEN
!          WRITE (LOGUNT,8101,ERR=502) PG,'PG',SUBNAM(I),PRMG(I)
!          IERR=1
!        END IF
!!        IF (PRMT(I) /= 0 .AND. PRMT(I) /= LTH) THEN
!        IF (PRMT(I) /= 0 .AND. PRMT(I) /= F_LTH) THEN
!          WRITE (LOGUNT,8101,ERR=502) F_LTH,'LTH',SUBNAM(I),PRMT(I)
!          IERR=1
!        END IF
!      END DO
!!      
      IF (IERR > 0) THEN
        IQUIT=1; GO TO 999
      END IF
!      
! Checks on INDXS values
! Allow time data item to be missing 5/93
      IF ((JTIME == JEVENT) .OR. (JEVENT == 0)) THEN
        IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
        WRITE (LOGUNT,8001,ERR=502)
        IERR=IERR+1
      END IF  
!    
      IF (.NOT.(JDELTA == JDEF .OR. (JSS /= JDEF .AND. JSS /= JDELTA) .OR. &
         (JMORE /= JDEF .AND. JMORE /= JDELTA))) THEN 
        IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
        WRITE (LOGUNT,8002,ERR=502)
        IERR=IERR+1
      END IF
! 
      IF (JMORE /= JDEF .AND. JDELTA == JDEF) THEN
        IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
        WRITE (LOGUNT,8062,ERR=502)
        IERR=IERR+1
      END IF
!      
! Give up if errors in INDXS
      IF (IERR+KERR > 0) THEN
        IQUIT=1; GO TO 999
      END IF
!      
      MODE=0; KIND=0; KC=0; MAXC=4
!      
      CALL PASS(MODE)
      IF (IQUIT == 1) GO TO 999
      MODE=1; KREC=0
!      
! Loop thru data records
   23 KREC=KREC+1
      CALL PASS(MODE)
      IF (IQUIT == 1) GO TO 999
      IF (MODE == 0) THEN   ! End of loop thru DATRECS
        IF (IERR > 0) THEN
          WRITE (LOGUNT,8099,ERR=502)
          IQUIT=1 
        END IF  
        GO TO 999
      END IF
!      
      TREC(1:PD)=DATREC(1:PD)
      EVENT=TREC(JEVENT)
      AMT=TREC(JAMT)
      RATE=TREC(JRATE)
      MORE=TREC(JMORE)
      SS=TREC(JSS)
      DEL=TREC(JDELTA)
      IF (NWIND /= 2) THEN  ! Initialize for new individual
        KIND=KIND+1
        KC=0
        IF (NWIND /= 1 .OR. IPS /= 2) THEN 
          T2=0.0
          CKSV(1:NC)=ISV(1:NC)
        END IF
      END IF
! Check individual data record items
      IF (TREC(JCONT) /= 0.0) THEN 
        IF (TREC(JCONT) /= 1.0) THEN
          IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
          WRITE (LOGUNT,8003,ERR=502) KREC
          IERR=IERR+1
        ELSE
          KC=KC+1
          IF (KC > MAXC) THEN
            IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
            WRITE (LOGUNT,8032,ERR=502) KREC,MAXC
            IERR=IERR+1
          END IF
          IF ((IDXMDV == 0) .OR. (TREC(IDXMDV) /= 1)) THEN   
            IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
            WRITE (LOGUNT,8004,ERR=502) KREC
            IERR=IERR+1
          END IF
        END IF  
        GO TO 9010
      END IF  
!      
! Check event record
      KC=0;  KERR=0
      T1=T2; T2=TREC(JTIME)
!    
      IF (T2 < 0.0) THEN
        IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
        WRITE (LOGUNT,8038,ERR=502) KREC
        IERR=IERR+1
      ELSE
        IF (T2 <  T1 .AND. EVENT < 3) THEN 
          IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
          WRITE (LOGUNT,8005,ERR=502) KREC
          IERR=IERR+1
        END IF
      END IF
!    
      IF (AMT < 0.0) THEN   
        IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
        WRITE (LOGUNT,8006,ERR=502) KREC
        IERR=IERR+1
      END IF
  !
      IF (.NOT. (RATE == -1.0 .OR. RATE == -2.0 .OR. RATE >= 0.0)) THEN 
        IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
        WRITE (LOGUNT,8007,ERR=502) KREC
        IERR=IERR+1
      END IF
!      
      IF (.NOT.(SS == 0.0 .OR. SS == 1.0 .OR. SS == 2.0 .OR. SS == 3)) THEN
        IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
        WRITE (LOGUNT,8008,ERR=502) KREC
        IERR=IERR+1
      END IF
!    
      IF (DEL < 0.0) THEN  
        IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
        WRITE (LOGUNT,8009,ERR=502) KREC
        IERR=IERR+1
      END IF
!    
      IF (MORE /= AINT(MORE) .OR. MORE < 0) THEN
        IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
        WRITE (LOGUNT,8060,ERR=502) KREC
        KERR=KERR+1
      END IF
!   
      CTEMP=TREC(JCOMPT)  ! This is the CMT data item
!   
      IF (CTEMP /= AINT(CTEMP) .OR. ABS(CTEMP) > NC) THEN
!       IF (ABS(CTEMP) /= 100) THEN ! Allow 100 as the default output compartment  7/2003
        IF (.NOT. ISDEFOUT(INT(ABS(CTEMP)))) THEN    ! Allow 1000 or 100 as the default output CMT. 3/2013
          IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
          WRITE (LOGUNT,8010,ERR=502) KREC
          KERR=KERR+1
        END IF
      END IF
!         
      IC=CTEMP
! Allow 100 as the default output compartment (next 2 lines) 7/2003
!     IF (IC ==  100) IC=NC
!     IF (IC == -100) IC=-NC
      IF (ISDEFOUT(IC)) IC=NC  ! Allow 1000 or 100 as the default output CMT. 3/2013
      IF (ISDEFOUT(-IC)) IC=-NC
      IF (IC == 0) IC=IDC
!      
      CTEMP=TREC(JCOMPF)  ! This is the PCMT data item
!    
      IF (.NOT.(CTEMP == AINT(CTEMP) .AND. CTEMP <= NC .AND. CTEMP >= 0)) THEN    
!       IF (CTEMP /= 100) THEN ! Allow 100 as the default output compartment  7/2003
        IF (.NOT. ISDEFOUT(INT(CTEMP))) THEN  ! Allow 1000 or 100 as the default output CMT. 3/2013
          IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
          WRITE (LOGUNT,8011,ERR=502) KREC
          KERR=KERR+1
        END IF
      END IF
!      
      IF (TREC(JERROR) /= AINT(TREC(JERROR))) GO TO 140
      IF (TREC(JERROR) >= 0.0 .AND. TREC(JERROR) <= 3) GO TO 150
! Only permit CALL=10,11,12,13 with ADVAN9
      IF (ADVID /= 9) GO TO 140
      IF (TREC(JERROR) >= 10 .AND. TREC(JERROR) <= 13) GO TO 150
  140 IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
      WRITE (LOGUNT,8012,ERR=502) KREC
      IERR=IERR+1
 !     
  150 IF (.NOT.(TREC(JEVENT) == AINT(TREC(JEVENT)) .AND. (EVENT >= 0 .AND. EVENT <= 4))) THEN 
        IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
        WRITE (LOGUNT,8013,ERR=502) KREC
        KERR=KERR+1
      END IF
!   
! Check for serious errors
      IERR=IERR+KERR
!      
      IF (KERR > 0) GO TO 9010
!      
      SELECT CASE (EVENT+1)
      CASE (1)  ! Observation event
        IC=TREC(JCOMPT)
        IF (IC == 0) IC=IDO
        IF (IC <  0) IC=-IC
!       IF (IC == 100) IC=NC    ! Allow 100 as the default output compartment  7/2003
        IF (ISDEFOUT(IC)) IC=NC ! Allow 1000 or 100 as the default output CMT. 3/2013
!        
        IF (.NOT.(AMT == 0.0 .AND. RATE == 0.0 .AND. SS == 0.0 .AND. DEL == 0.0 .AND. &
             MORE == 0)) THEN
          IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
          WRITE (LOGUNT,8015,ERR=502) KREC
          IERR=IERR+1
        END IF
!        
        IF (CKSV(IC) /= 1) THEN 
          IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
          WRITE (LOGUNT,8037,ERR=502) KREC
          IERR=IERR+1
        END IF
!       
        IF (TREC(JCOMPT) < 0.0) THEN 
          IF (ITURN(IC) /= 1) THEN
            IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
            WRITE (LOGUNT,8030,ERR=502) KREC
            IERR=IERR+1
          ELSE
            IF (ISV(IC) /= 0 .OR. IINST(IC) /= 0) THEN  
              IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
              WRITE (LOGUNT,8014,ERR=502) KREC
              IERR=IERR+1
            END IF
            CKSV(IC)=0
          END IF  
        END IF
        GO TO 9010
      CASE (2,5)    ! Dose event; For special case - ADVAN9, no transient compartments
        IF (IC /= 0) THEN 
          IF ((IDXMDV == 0) .OR. (TREC(IDXMDV) /= 1.0)) THEN
            IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
            WRITE (LOGUNT,8017,ERR=502) KREC
            IERR=IERR+1
          END IF        
          IF (.NOT.(SS /= 0.0 .OR. MORE /= 0.0 .OR. DEL == 0.0)) THEN
            IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
            IF (JSS /= JDEF .AND. JMORE /= JDEF) WRITE (LOGUNT,8018,ERR=502) KREC
            IF (JSS == JDEF .AND. JMORE /= JDEF) WRITE (LOGUNT,8118,ERR=502) KREC
            IF (JSS /= JDEF .AND. JMORE == JDEF) WRITE (LOGUNT,8218,ERR=502) KREC
            IERR=IERR+1
          END IF
!      
          IF (IC <= 0) THEN
            IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
            WRITE (LOGUNT,8019,ERR=502) KREC
            IERR=IERR+1
            IC=-IC
          END IF
!          
! Added 2/88 for reset and dose
          IF (EVENT == 4) CKSV(1:NC)=ISV(1:NC)
          CKSV(IC)=1
         
! Added 9/87
          IF (ISV(IC) == 0 .AND. ITURN(IC) == 0) THEN
            IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
            WRITE (LOGUNT,8030,ERR=502) KREC
            IERR=IERR+1
          END IF
!         
          IF (RATE /= 0.0) THEN    
! Zero-order dose (same message as inst. dose 9/87)
            IF (IINST(IC) /= 1) THEN
              IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
              WRITE (LOGUNT,8021,ERR=502) KREC
              IERR=IERR+1
            END IF
            IF (RATE <= 0.0) THEN
              IF (RATE /= -1.0) THEN
                IF (RATE == -2.0) THEN  
                  IF (ID(IC) <= 0) THEN
                    IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
                    WRITE (LOGUNT,8034,ERR=502) KREC,IC
                    IERR=IERR+1
                  END IF        
                END IF   
              ELSE
                IF (IRR(IC) <= 0) THEN 
                  IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
                  WRITE (LOGUNT,8024,ERR=502) KREC,IC
                  IERR=IERR+1
                END IF 
              END IF
            END IF
            GO TO 390
          END IF
          IF (IINST(IC) == 1) GO TO 390 ! Instantaneous dose
        END IF
        IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)       
        WRITE (LOGUNT,8021,ERR=502) KREC
        IERR=IERR+1
        GO TO 390   ! All doses
      CASE (3)
        IF (IDXMDV == 0 .OR. TREC(IDXMDV) /= 1.0) THEN
          IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
          WRITE (LOGUNT,8028,ERR=502) KREC
          IERR=IERR+1
        END IF     
        IF (.NOT.(AMT == 0.0 .AND. RATE == 0.0 .AND. SS == 0.0 .AND. DEL == 0.0 .AND. MORE == 0)) THEN
          IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
          WRITE (LOGUNT,8029,ERR=502) KREC
          IERR=IERR+1
        END IF
        GO TO 525   ! Reset and other events
      CASE (4)
        IF ((IDXMDV == 0) .OR. (TREC(IDXMDV) /= 1.0)) THEN
          IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
          WRITE (LOGUNT,8058,ERR=502) KREC
          IERR=IERR+1
        END IF
        IF (.NOT.(AMT == 0.0 .AND. RATE == 0.0 .AND. SS == 0.0 .AND. DEL == 0.0 .AND. MORE == 0)) THEN
          IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
          WRITE (LOGUNT,8059,ERR=502) KREC
          IERR=IERR+1
        END IF
        CKSV(1:NC)=ISV(1:NC)
        GO TO 525
      CASE DEFAULT
         CALL ERRORMSGS(302) ! 'ERROR IN CHECK ROUTINE. VARIABLE "EVENT+1" IS NOT IN THE RANGE (1-5)'
         GO TO 999
      END SELECT
! 
! All doses
  390 IF (MORE /= 0 .AND. DEL == 0) THEN 
        IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
        WRITE (LOGUNT,8061,ERR=502) KREC
        IERR=IERR+1
      END IF
!   
      IF (SS == 0.0) THEN
        IF (AMT == 0.0) THEN
          IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
          WRITE (LOGUNT,8025,ERR=502) KREC
          IERR=IERR+1
        END IF
      ELSE  ! Steady state dose
        IF (CKSV(NC) /= 0) THEN 
          IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
          WRITE (LOGUNT,8040,ERR=502) KREC
          IERR=IERR+1
        END IF
!     
        IF (SSID == 0) THEN 
          IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
          WRITE (LOGUNT,8039,ERR=502) KREC
          IERR=IERR+1
        END IF
!        
        IF (AMT /= 0.0 .OR. DEL /= 0.0) THEN 
          IF (AMT <= 0.0 .OR. DEL <= 0.0) THEN
            IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
            WRITE (LOGUNT,8026,ERR=502) KREC
            IERR=IERR+1
          END IF
        ELSE
          IF (RATE == XD) THEN
            IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
            WRITE (LOGUNT,8027,ERR=502) KREC
            IERR=IERR+1
          END IF
        END IF
      END IF
      GO TO 600
! 
! Reset and other events
  525 IF (IC < 0) THEN
        IF (ITURN(-IC) /= 1) THEN
          IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
          WRITE (LOGUNT,8030,ERR=502) KREC
          IERR=IERR+1
        ELSE
          CKSV(-IC)=0
        END IF
      ELSE IF (IC > 0) THEN
        CKSV(IC)=1
      END IF
!  
! Dose and other event - check compartment for observation
  600 IC=TREC(JCOMPF)
      IF (IC == 0) IC=IDO
!     IF (IC == 100) IC=NC    ! Allow 100 as the default output compartment  7/2003
      IF (ISDEFOUT(IC)) IC=NC ! Allow 1000 or 100 as the default output CMT. 3/2013
!
!     IF ((CKSV(IC) /= 1) .AND. (TREC(JCOMPT) /= -IC) .AND. (TREC(JCOMPT) /= -100)) THEN 
! Allow 1000 or 100 as the default output CMT. 3/2013
      IF ((CKSV(IC) /= 1) .AND. (TREC(JCOMPT) /= -IC) .AND. (.NOT. ISDEFOUT(-INT(TREC(JCOMPT))))) THEN 
        IF (IERR+KERR == 0) WRITE (LOGUNT,8000,ERR=502)
        WRITE (LOGUNT,8037,ERR=502) KREC
        IERR=IERR+1
      END IF
!      
! End of DATREC checks
 9010 IF (IERR > MAXERR) THEN 
        WRITE (LOGUNT,8031,ERR=502) KREC
        WRITE (LOGUNT,8099,ERR=502) 
        IQUIT=1  
      END IF
!
      GO TO 23 ! End of loop thru data records
!     
 8000 FORMAT ('0DATA RECORD ERROR MESSAGE FROM PRED:')     
 8001 FORMAT ('0EVENT ID DATA ITEM INDEX MISSING')
 8002 FORMAT ('0STEADY STATE AND/OR ADDITIONAL DOSES DATA ITEM INDICES MISSING')
 8003 FORMAT ('0DATA REC',I5,': VALUE OF CONTINUATION DATA ITEM IS INAPPROPRIATE')
 8004 FORMAT ('0DATA REC',I5,': MDV DATA ITEM MUST BE 1 WHEN CONTINUATION DATA ITEM IS 1')
 8005 FORMAT ('0DATA REC',I5,': TIME DATA ITEM IS LESS THAN PREVIOUS TIME DATA ITEM')
 8006 FORMAT ('0DATA REC',I5,': DOSE AMOUNT DATA ITEM HAS INAPPROPRIATE VALUE')
 8007 FORMAT ('0DATA REC',I5,': RATE DATA ITEM HAS INAPPROPRIATE VALUE')
 8008 FORMAT ('0DATA REC',I5,': STEADY STATE DATA ITEM HAS INAPPROPRIATE VALUE')
 8009 FORMAT ('0DATA REC',I5,': INTERDOSE INTERVAL DATA ITEM HAS INAPPROPRIATE VALUE')
 8010 FORMAT ('0DATA REC',I5,': COMPARTMENT NUMBER IS INAPPROPRIATE FOR THIS MODEL')
 8011 FORMAT ('0DATA REC',I5,': PREDICTION COMPARTMENT NUMBER IS INAPPROPRIATE FOR THIS MODEL')
 8012 FORMAT ('0DATA REC',I5,': CALL DATA ITEM HAS INAPPROPRIATE VALUE')
 8013 FORMAT ('0DATA REC',I5,': EVENT IDENTIFICATION DATA ITEM HAS INAPPROPRIATE VALUE')
 8014 FORMAT ('0DATA REC',I5,': SPECIFIED COMPARTMENT MAY NOT BE TURNED OFF WITH'/' AN OBSERVATION RECORD')
 8015 FORMAT ('0DATA REC',I5,': OBSERVATION EVENT RECORD MAY NOT SPECIFY DOSING INFORMATION')
 8017 FORMAT ('0DATA REC',I5,': MDV DATA ITEM MUST BE 1 FOR LAST RECORD OF DOSE EVENT')
 8018 FORMAT ('0DATA REC',I5,': INTERDOSE INTERVAL DATA ITEM IS NON-ZERO,'/                 &
                              ' STEADY STATE AND ADDITIONAL DOSES DATA ITEMS ARE ZERO')
 8019 FORMAT ('0DATA REC',I5,': CAN NOT TURN OFF A COMPARTMENT RECEIVING A DOSE')
 8021 FORMAT ('0DATA REC',I5,': DOSE NOT ALLOWED IN THIS COMPARTMENT ')
 8024 FORMAT ('0DATA REC',I5,': PK PARAMETER FOR RATE IS NOT SUPPLIED BY PK FOR'/' COMPT. NO. ',I3)
 8025 FORMAT ('0DATA REC',I5,': DOSE AMOUNT IS ZERO')
 8026 FORMAT ('0DATA REC',I5,': DOSE RATE, DOSE AMOUNT, INTERDOSE INTERVAL DATA ITEMS'/     &
                              ' MUTUALLY INCOMPATIBLE FOR STEADY STATE RECORD')
 8027 FORMAT ('0DATA REC',I5,': DURATION MAY NOT BE MODELLED FOR STEADY STATE INFUSION')
 8028 FORMAT ('0DATA REC',I5,': MDV DATA ITEM MUST BE 1 FOR LAST RECORD OF OTHER EVENT')
 8029 FORMAT ('0DATA REC',I5,': OTHER EVENT RECORD MAY NOT SPECIFY DOSING INFORMATION')
 8030 FORMAT ('0DATA REC',I5,': SPECIFIED COMPARTMENT MAY NOT BE TURNED ON OR OFF')
 8031 FORMAT ('0DATA REC',I5,': MAXIMUM NUMBER OF ERRORS EXCEEDED - NO MORE DATA RECORDS'/  &
                              ' WILL BE CHECKED')
 8032 FORMAT ('0DATA REC',I5,': NUMBER OF CONTINUATION RECORDS FOR THE EVENT RECORD EXCEEDS', I3)
 8034 FORMAT ('0DATA REC',I5,': PK PARAMETER FOR DURATION IS NOT SUPPLIED BY PK FOR'/' COMPT. NO. ',I3)
 8037 FORMAT ('0DATA REC',I5,': COMPARTMENT ASSOCIATED WITH THE PREDICTION IS OFF')
 8038 FORMAT ('0DATA REC',I5,': NEGATIVE TIME DATA ITEM IS INAPPROPRIATE')
 8039 FORMAT ('0DATA REC',I5,': SS DOSE MAY NOT BE USED WITH NULL SSS ROUTINE')
 8040 FORMAT ('0DATA REC',I5,': DEFAULT OUTPUT COMPARTMENT MUST BE OFF WHEN SS DOSE IS GIVEN')
 8058 FORMAT ('0DATA REC',I5,': MDV DATA ITEM MUST BE 1 FOR LAST RECORD OF RESET EVENT')
 8059 FORMAT ('0DATA REC',I5,': RESET EVENT RECORD MAY NOT SPECIFY DOSING INFORMATION')
 8060 FORMAT ('0DATA REC',I5,': ADDITIONAL DOSES DATA ITEM MUST BE A NON-NEGATIVE INTEGER')
 8061 FORMAT ('0DATA REC',I5,': INTERDOSE INTERVAL DATA ITEM IS ZERO,'/                     &
                              ' ADDITIONAL DOSES DATA ITEM IS NON-ZERO')
 8062 FORMAT ('0INTERDOSE INTERVAL DATA ITEM INDEX MISSING')
 8099 FORMAT ('0RUN TERMINATED BECAUSE OF ERRORS IN DATA RECORDS')
 8101 FORMAT ('0INTERNAL ERROR IN PREDPP.'/' PREDPP WAS COMPILED WITH VALUE',I4,' FOR ',A,'.'/     &
              ' SUBROUTINE ',A,' WAS COMPILED WITH VALUE',I4,'.')
 8118 FORMAT ('0DATA REC',I5,': INTERDOSE INTERVAL DATA ITEM IS NON-ZERO,'/                 &
                              ' ADDITIONAL DOSES DATA ITEM IS ZERO')
 8218 FORMAT ('0DATA REC',I5,': INTERDOSE INTERVAL DATA ITEM IS NON-ZERO,'/                 &
                              ' STEADY STATE DATA ITEM IS ZERO') 
!      
  999 RETURN
!
  502 CALL ERRORMSGS(502,FILENAME='OUTPUT')
!
      END SUBROUTINE CHECK
!      
!-----------------------------HISTORY------------------------------------------------
! VERSION     : NONMEM VII
! AUTHOR      : ALISON J. BOECKMANN
! CREATED ON  : AUG/2013
! LANGUAGE    : FORTRAN 90/95
! LAST UPDATE : AUG/2013 - INTRODUCED HEADER INFORMATIONS AND RESTRUCTURED AS PER
!                          THE NONMEM STANDARDS
!
!---------------------------- ISDEFOUT.F90 ------------------------------------------
!
! FUNCTION ISDEFOUT(IC)
!
! DESCRIPTION : This function answers the question: is this compartment no. the default 
!               output compartment? Formerly, only 100 could signify the default output
!               compartment. With nm7.3, it is possible for No. of cmts. to be >= 100.
!               With nm7.3, 1000 may be used with both large and small models.
!               100 may be used with small models (No. of cmts. < 100), to provide upwards
!               compatability. Note that this subroutine is used by CHECK and is only 
!               involved with error messages. Subroutine PRED looks for 1000 and 100 and 
!               actually implements this feature. Note that NMTRAN now gives a warning 
!               when CMT=100 with a large model, and warns the user that 1000 should be 
!               used to signify the default ouput compartment.
!
! ARGUMENTS   : IC
!               IN     - IC
!                        IC - CMT or PCMT data item.
!               OUT    - NONE
!               IN OUT - NONE
!
! CALLED BY   : CHECK
!
! CALLS       : NONE
!
! ALGORITHM   : - Initialize ISDEFOUT to FALSE
!               - Set ISDEFOUT to TRUE if CMT or PCMT data item is 1000 or 
!                 if Number of primary Compartments is less than 99 & CMT 
!                 or PCMT data item is 100
!               - The result type is default logical.
!
! MODULES USED: PRSIZES,PRCOM_INT
!
! CONTAINS    : NONE
!
! LOCAL'S     : ISDEFOUT
!
!---------------------------- END OF HEADER -----------------------------------------
!
      FUNCTION ISDEFOUT(IC)
!
      USE PRSIZES,   ONLY: ISIZE
! INTEGER      
      USE PRCOM_INT, ONLY: NC
!      
      INTEGER(KIND=ISIZE), INTENT(IN) :: IC
!      
!------------------------------------------------------------------------------------
!
! Local Variables

      LOGICAL :: ISDEFOUT
!      
      ISDEFOUT=.FALSE.
      IF (IC == 1000) ISDEFOUT=.TRUE.
      IF (NC < 99 .AND. IC == 100) ISDEFOUT=.TRUE.
!      
  999 RETURN
!      
      END FUNCTION ISDEFOUT
