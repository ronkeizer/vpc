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
! AUTHOR      : ROBERT J. BAUER
! CREATED ON  : DEC/2008
! LANGUAGE    : FORTRAN 90/95
! LAST UPDATE : DEC/2008 - INTRODUCED HEADER INFORMATIONS AND RESTRUCTURED AS PER
!                          THE NONMEM STANDARDS 
!               FEB/2009 - BAYESIAN METHOD INCLUDED
!               MAR/2009 - COMMON BLOCKS OF BAYES MODEL REPLACED WITH MODULES
!               MAY/2009 - INTEGRATED BETA 5.5B2 MODIFICATIONS
!               FEB/2011 - INTEGRATED 7.2BETA5.8B MODIFICATIONS
!               AUG/2013 - INTEGRATED NONMEM7.3a7.1 CHANGES
!
!---------------------------- MUMODEL.F90 -------------------------------------------
!
! SUBROUTINE MUMODEL(THETAQ,MU)
!
! DESCRIPTION : MUMODEL calls MUMODEL2, which was constructed from source code in
!               FSUBS. MUMODEL2 only has code pertaining to MU1,MU2 etc, so that
!               the computational expense is not great. The MU() values are returned
!               given a set of THETA values THETAQ. Used for MU modeling. If the user
!               did not MU model an ETA parameter, MU() is returned 0.
!
! ARGUMENTS   : THETAQ,MU
!               IN     - THETAQ
!                        THETAQ - THETA values
!               OUT    - MU
!                        MU     - MU values
!               IN OUT - NONE
!
! CALLED BY   : PMUD_EVALUATE,MUMODEL3
!
! CALLS       : DAT1,MUMODEL2
!
! ALGORITHM   : - Initialize variables
!               - Evaluate MU's as averaged accross all records for that individual
!               - Loop over number of individual records
!               - Hold and handle number of contiguous data records - call DAT1
!               - Call MUMODEL2
!               - Loop over total dimension size of OMEG and update MU
!               - Set ICALL to ICALLP
!
! MODULES USED: FSIZES,NM_BAYES_INT,NMPRD_INT,PRCOM_INT,CMNM1_INT, PRCOM_REAL, 
!               NM_INTERFACE
!
! CONTAINS    : NONE
!
! LOCAL'S     : FU,GU,HU,I,ICALLP,IDEF,IREV,ISMALL,J,MUI,NEWIND,NTOT,NVNT,T1
!
!---------------------------- END OF HEADER -----------------------------------------
!
      SUBROUTINE MUMODEL(THETAQ,MU)
!      
      USE FSIZES,       ONLY: F_PC,F_PG,F_LVO,F_LVS
! INTEGER        
      USE NM_BAYES_INT, ONLY: MUFIRST
      USE NMPRD_INT,    ONLY: IQUIT
      USE PRCOM_INT,    ONLY: XNPETA,IRGG,CALLPK
      USE CMNM1_INT,    ONLY: ICALL
! REAL 
      USE PRCOM_REAL,   ONLY: G3
! INTERFACE
      USE NM_INTERFACE, ONLY: MUMODEL2      
!      
      INCLUDE 'TOTAL.INC'      
!
      REAL(KIND=DPSIZE), INTENT(IN)  :: THETAQ(*)
      REAL(KIND=DPSIZE), INTENT(OUT) :: MU(*)      
!
      SAVE
!
!------------------------------------------------------------------------------------
!      INTEGER MUFIRST
!      COMMON /MUFIRST_COMMON/ MUFIRST
!      DATA MUFIRST /1/
!      DATA JINDPREV /-1/
!     COMMON /JINDPREV_COMMON/ JINDPREV
!------------------------------------------------------------------------------------
!
! Local Variables
!
!      INTEGER(KIND=ISIZE), PARAMETER :: PE=LVR
!      REAL(KIND=DPSIZE) :: FU,HU(LVR,LVR),GU(LVR,LVR+1),MUI(MAXOMEG),NTOT(MAXOMEG),T1
!      INTEGER(KIND=ISIZE) :: IDEF(7,PC),I,IREV,ICALLP,ISMALL,J,NEWIND,NVNT 
!
      INTEGER(KIND=ISIZE) :: I,IREV,ICALLP,ISMALL,J,NEWIND,NVNT
!      
      REAL(KIND=DPSIZE) :: FU,T1
! 
      INTEGER(KIND=ISIZE), ALLOCATABLE :: IDEF(:,:)
!
      REAL(KIND=DPSIZE), ALLOCATABLE, DIMENSION(:)   :: MUI,NTOT     
      REAL(KIND=DPSIZE), ALLOCATABLE, DIMENSION(:,:) :: GU,HU
!
      IRGG=F_PG+1
      IF(.NOT.ALLOCATED(GU))    ALLOCATE(GU(F_LVO,F_LVO+1))
      IF(.NOT.ALLOCATED(HU))    ALLOCATE(HU(F_LVS,F_LVO+1))
      IF(.NOT.ALLOCATED(IDEF))  ALLOCATE(IDEF(7,F_PC))
      IF(.NOT. ALLOCATED(MUI))  ALLOCATE(MUI(F_MAXOMEG))
      IF(.NOT. ALLOCATED(NTOT)) ALLOCATE(NTOT(F_MAXOMEG))
!
!     For the PRED call to work properly, the ICALL of CMNM1_INT must be set. Then return ICALL to
!     its original value
!
      ICALLP=ICALL 
      ISMALL=-1000000000
!      
      MU(1:NOMEG)=0.0D+00
      NTOT(1:NOMEG)=0.0D+00
!      
      T1=ISMALL
      IF (NPMU /= 0) THEN
        IF (NRECIND > 0) THEN           ! Evaluate MU's as averaged accross all records for that individual
          DO I=1,NRECIND                ! Loop over number of individual records
            CALL DAT1(IRECIDX+I,0,0)    ! Holds and handles number of contiguous data records.
!            IF (JTIME >=1 AND JTIME <= PD) THEN
!              T2=VDATREC(JTIME)
!              DELTA=T2-T1
!              T1=T2
!              IF ( DELTA <=0 .AND. I/=1 AND CALLPK ==2) CYCLE
!            END IF	     
            ICALL=2; NEWIND=2
            IREV=1;  NVNT=1
            MUI(1:NOMEG)=ISMALL
            IF (MUFIRST == 1) THEN
              ICALL=0
              CALL MUMODEL2(THETAQ,MUI,ICALL,IDEF,NEWIND,VDATREC,VDATREC,IREV,NVNT,INDXS,&
                            FU,GU,HU,IRGG,G3,XNPETA)
              ICALL=2
              MUFIRST=0
              IF (IQUIT == 1) GO TO 998
            END IF
            CALL MUMODEL2(THETAQ,MUI,ICALL,IDEF,NEWIND,VDATREC,VDATREC,IREV,NVNT,INDXS,&
                          FU,GU,HU,IRGG,G3,XNPETA)
            IF (IQUIT == 1) GO TO 998
            DO J=1,NOMEG                ! Loop over total dimension size of OMEG.
              IF (MUI(J) /= ISMALL) THEN
                MU(J)=MU(J)+MUI(J)
                NTOT(J)=NTOT(J)+1.0D+00
              END IF
            END DO
            IF (CALLPK == 3) EXIT
          END DO
          DO I=1,NOMEG
            IF (NTOT(I) /= 0.0D+00) MU(I)=MU(I)/NTOT(I)
!           WRITE(IDUNIT,*) I,NTOT(I),MU(I)
          END DO
        END IF
      END IF
!      
  998 CONTINUE  
      ICALL=ICALLP 
!
  999 RETURN
!
      END SUBROUTINE MUMODEL
