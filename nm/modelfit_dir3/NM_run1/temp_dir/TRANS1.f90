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
! CREATED ON  : MAY/1985
! LANGUAGE    : FORTRAN 90/95
! LAST UPDATE : JUL/2008 - COMMON BLOCKS REPLACED WITH MODULES
!               NOV/2008 - INTRODUCED HEADER INFORMATIONS AND RESTRUCTURED AS PER
!                          THE NONMEM STANDARDS
!               FEB/2011 - INTEGRATED 7.2BETA5.8B MODIFICATIONS
!
!----------------------------- TRANS1.F90 -------------------------------------------
! SUBROUTINE TRANS(ITRANS,IRGG,GG,NPETAS)
!
! DESCRIPTION : PREDPP contains a library of routines, called TRANSLATOR routines,
!               which perform re-parameterization of the basic PK parameters computed
!               by the PK routine. Exactly one TRANS routine must be selected for
!               each NONMEM/PREDPP run
!               The TRANS subroutine translates (or transforms) the values for a set
!               of basic PK parameters modeled in PK to a set of values for the
!               internal parameters required by the ADVAN. The PREDPP Library has a
!               number of TRANS subroutines, representing different possible 
!               parameterizations in PK, from which the user may choose. If a suitable 
!               translator is not found in the Library, the user may write his own.
!
! ARGUMENTS   : ITRANS,IRGG,GG,NPETAS
!               IN     - IRGG,NPETAS
!                        IRGG   - Used to set the maximum number of rows in GG
!                        NPETAS - The number of population ETA's in the problem. Certain
!                                 variables are available in GLOBAL MODULES. Their use is
!                                 optional. Not used.
!
!                                 Variables in read/write MODULE: IERPRD NETEXT ETEXT
!                                 Variables in read-only COMMON: NOETAS, SECOND, LOGICAL,
!                                 NOETAS,SECOND. When SECOND is true, PREDPP requires
!                                 second-partial derivatives of ETAs.
!               OUT    - NONE
!               IN OUT - ITRANS,GG
!                        ITRANS - 0 = Model Initialization
!                                 1 = TRANS has been called for initialization at the
!                                     beginning of a NONMEM problem; one such call per
!                                     problem. ITRANS must be reset by TRANS to a number
!                                     in the range 1-8999. This number appears on NONMEM
!                                     output, allowing the user to identify the TRANS
!                                     routine being used.
!                                 2 = Normal Entry. On input the GG array has stored in 
!                                     it the values computed by PK (except that were any 
!                                     PK parameter modeled in its logarithm form in PK, 
!                                     PREDPP would have  already exponentiated its 
!                                     typical/subject-specific value and multiplied 
!                                     its eta derivatives by its exponentiated typical/
!                                     subject-specific value). On output the GG array 
!                                     should have stored in it the values that would be 
!                                     computed by PK were the internal parameters of the 
!                                     ADVAN modeled directly in PK (and none in their 
!                                     logarithmic form).
!                                 4 = TRANS has been called during the Simulation Step.
!                                     Only the first column of the GG array need be
!                                     computed as with ITRANS=2. Other columns need not
!                                     be computed.
!                        GG     - The array of PK parameters and their ETA derivatives
!                                 after creation by subroutine PK and optional 
!                                 exponentiation by PRED. Not used.
! 
!                                 REFERENCES: Guide VI, section III.M (p. 38)
! 
! CALLED BY   : PRED  - This is PREDPP main program. Provides prediction, partial 
!                       derivatives of the statistical model with respect to ETA
!                       and EPSILON random variables and stores them in the G
!                       and H arguments of the PRED routine.
!               PREDI - Initialization-Finalization routine of PRED
!
! CALLS       : NONE
!
! ALGORITHM   : - Set ITRANS to 9999 and return if no user defined routine is present
!
! MODULES USED: PRSIZES,PRDIMS
!
! CONTAINS    : NONE
!
! LOCAL'S     : PE
!
!---------------------------- END OF HEADER -----------------------------------------
!
      SUBROUTINE TRANS(ITRANS,IRGG,GG,NPETAS) 
!      
      USE PRSIZES, ONLY: ISIZE,DPSIZE
      USE PRDIMS,  ONLY: GTRD
!
      IMPLICIT NONE
!     
      INTEGER(KIND=ISIZE), INTENT(IN)     :: IRGG,NPETAS 
      INTEGER(KIND=ISIZE), INTENT(IN OUT) :: ITRANS
      REAL(KIND=DPSIZE),   INTENT(IN OUT) :: GG(IRGG,GTRD+1,GTRD+1) 
!
      SAVE
!      
!------------------------------------------------------------------------------------
! Subroutine TRANS - Default translator ID=9999
!
! ITRANS=0;  Model initialization. 
! ITRANS=2;  Normal entry.
!
! Local Variables
!
      ITRANS=9999
!      
  999 RETURN
!      
      END SUBROUTINE TRANS
