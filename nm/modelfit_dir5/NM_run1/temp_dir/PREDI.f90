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
! CREATED ON  : OCT/1987
! LANGUAGE    : FORTRAN 90/95
! LAST UPDATE : OCT/1987 - FOR MULTIPLE DOSE RECORDS
!                        - NP STAYED 0 WHEN NO ADDITIONAL PARMS - BUG FIX AJB
!               MAY/1993 - PASS JDEF TO PRED IN CASE OF NO TIME DATA ITEM 
!                        - ALLOW NO DOSE COMPARTMENT WITH ADVAN9
!               APR/1994 - INITIALIZATION AT ICALL=1: CALL DES ALLOWS LIBRARY ROUTINES
!                          TO READ NEW INSTRUCTIONS IF THE DES ROUTINE DOES NOT RESET
!                          IDEF(1), ASSUME MAXIMUM NUMBER OF THETAS.
!               FEB/1995 - CALL ROUTINE TO CHECK DATA RECORDS FOR CONSISTENCY ALSO
!                          CHECKS ARRAY SIZES IN SUBROUTINES 
!               SEP/1997 - TELL NONMEM "RECURSIVE" PRED
!               JUL/2002 - CONTROLS FORMAT CHANGES. IUSED WAS NOT SET IN SUPERPROB
!                          WHEN .NOT.PF
!                        - IUSED WAS NOT SET IN SUPERPROB WHEN .NOT.PF 
!               OCT/2007 - BUG WHEN NO DOSABLE COMPARTMENTS.
!                        - CHANGED FROM "K == 0" TO "KCOMP == 0" 
!               JUL/2008 - COMMON BLOCKS REPLACED WITH MODULES
!               NOV/2008 - INTRODUCED HEADER INFORMATIONS AND RESTRUCTURED AS PER
!                          THE NONMEM STANDARDS
!               JAN/2009 - ASSUMED SIZE ARRAY'S REPLACED WITH ASSUMED SHAPE
!               FEB/2009 - MOVED DATA STATEMENTS TO PRDATA.F90 FILE
!               APR/2009 - MOVED ERROR MESSAGES TO ERRORMSGS.F90 FILE AND
!                        - IQUIT ADDED TO HANDLE ERROR MESSAGE
!                        - INTRODUCED ERROR CHECK FOR FILE OPERATION
!               JUN/2009 - INTEGRATED BETA 5.5B4 MODIFICATIONS
!               FEB/2010 - CHANGED SIZES TO PRSIZES
!               JUL/2010 - INCORPORATED 7.2BETA5.2B CHANGES
!               FEB/2011 - INTEGRATED 7.2BETA5.8B MODIFICATIONS
!               AUG/2012 - INTEGRATED NONMEM7.3ALPHA6.3 CHANGES
!               AUG/2013 - INTEGRATED NONMEM7.3a7.1 CHANGES
!
!----------------------------- PREDI.F90 --------------------------------------------
! SUBROUTINE PREDI(ICALL,NEWIND,THETA,INDXS,F,H,IDEF)
!
! DESCRIPTION : Initialization/finalization routine for PRED. This routine makes initial
!               calls to subroutines, initializes PREDPP, and prints the PREDPP initial
!               pages in the NONMEM output.
!
!               If compact arrays are used in DES, it creates the reverse mapping
!               information. Later, it makes final calls to subroutines.
!
! ARGUMENTS   : ICALL,NEWIND,THETA,INDXS,F,H,IDEF
!               IN     - ICALL,NEWIND,THETA
!                        ICALL  - 0 = Model initialization call
!                                 1 = Problem initialization call
!                                 3 = Termination entry call. Call user routine for
!                                     transgeneration, etc.
!                        NEWIND - 0 = First record of the data set. THETA value may
!                                     differ from value at last call with this record.
!                                 1 = First record of the data set, THETA value does
!                                     not differ from value at last call with this
!                                     record, and PRED is nonrecursive. First record
!                                     of a subsequent individual record.
!                                 2 = The record is the second or subsequent record of
!                                     an individual record.
!                        THETA  - A one-dimensional NONMEM THETA vector in which
!                                 the values of the THETA's are passed.
!               OUT    - H
!                        H      - An array of partial derivatives of F w.r.t epsilons.
!                                 H(i,1) is the derivative of F with respect to EPS(i).
!                                 H(i,j+1) is the partial derivative of H(i,1) with respect
!                                 to ETA(j). These mixed second derivatives are needed only
!                                 when the INTERACTION option is used to estimate parameters.
!                                 Set to zeros.
!               IN OUT - F,INDXS,IDEF
!                        F      - When ICALL=4, the value of the simulated observation is
!                                 associated with the data record. Alternatively, F can
!                                 be ignored, and the DV item in the data record can be
!                                 directly set to the value of the simulated observation.
!                                 With odd-type data, F is ignored; PRED should directly
!                                 set the DV item to the value of the simulated observation.
!                        INDXS  - The values specified in the $INDEX record of the
!                                 NM-TRAN control stream.
!                        IDEF   - Indices of PK params are in IDEF array
!
! CALLED BY   : PRED - This is PREDPP main program. Provides prediction, partial 
!                      derivatives of the statistical model with respect to ETA
!                      and EPSILON random variables and stores them in the G
!                      and H arguments of the PRED routine.
!
! CALLS       : CHECK     - Default data checker routine
!               ERROR     - Models intra-individual error in observed values.
!               INFN      - Make initialization and finalization computations.
!               PK        - Obtain values for basic and additional PK parameters
!               SADVAN    - Stands for Supervisor ADVAN. This is an interface.
!                           PREDPP contains a library of routines, called ADVAN routines,
!                           which implement specific kinetic models. Exactly one ADVAN
!                           routine must be selected for each NONMEM / PREDPP run.
!                           Its function is to "ADVANCE" the kinetic system from one
!                           state time point to the next
!               SSS       - Supervisor of steady state routines
!               TRANS     - Translates (or transforms) the values for a set of basic PK
!                           parameters modeled. If a suitable translator is not found
!                           in the Library, the user may write his own.
!               ERRORMSGS - Writes error messages to a file JUNIT and sets IQUIT to 1
!                           indicating that NONMEM has to quit for non-super problems.
!                           For super problems calculation continues with next sub problem.
!
! ALGORITHM   : - Generate labels in DIGIT2 array
!               - Branch over value of K
!               - If K=1, (ICALL=0)
!                 - Model initialization - initialize variables
!                 - Check for bad values in sizes
!                   - If errors, exit NONMEM - Set IQUIT=1 and return
!                 - Init. format statements for writes of complete arrays
!                 - Initial call to user initialization/termination routine - CALL INFN
!                   - If ERROR, return
!                 - Check for user supplied INFN routine and call accordingly
!                 - Call appropriate ADVAN Model - Call SADVAN
!                   - If ERROR, return
!                 - Check and digest the model subroutines output
!                 - Allow no basic params with DES
!                 - Define the individual compartments
!                 - Check for error in compartment definitions
!                 - Allow no dose compartment with ADVAN9
!                 - Define the output compartment
!                 - Set variables for ON/OFF feature, call DES, cal AES
!                 - Call Supervisor Steady state routine - CALL SSS
!                 - Check if link must be analyzed
!                 - Look for links from compartment I to compartment K
!                   - Save K for later entries.
!                 - Make initial call to translator - Call TRANS
!                 - Echo print the model definition information
!                 - Special checks for ADVAN9
!               - If K=2, ICALL=1.
!                 - Problem initialization 
!               - If K=3,4, ICALL=1=2
!                 - Termination entry
!                 - Call user routine for transgeneration, etc.
!                 - If ERROR, return
!               - If ICALL=1, Problem initialization execution starts from here avoiding
!                 above steps
!                 - Set up information obtained from NONMEM
!                 - Check print normal initialization messages in superprobs
!                 - Initialize H(1,1) to zero.
!               - Initialize format statements for writes of complete arrays
!                 - Set up for call to ERROR
!                 - Do Error checking - CALL ERROR
!                 - Set up for call to PK and CALL PK
!               - Allow user fixup of data
!               - Set up for call to PK
!               - Initialize call flag position to -1 (new and old IDEFS)
!               - Get and check indices of PK params from IDEF array
!               - If IDEF(1) /= -9, fix up location of new "CALL PK" flag unless PK changed it
!                 - Check largest number of PK parameter specified
!                 - Get scale and bio-availablity indices, rate indices.
!                 - Get duration indices, output fraction, time scale, call PK flag,
!                   time lag indices
!               - Else if IDEF(1) = -9,
!                 - Set up table for PK parameters
!                 - Get call PK flag
!                 - Get start,end of model time parameters
!                 - Get scale and dose-related parameters
!               - Set up table for PK parameters
!               - Check for basic PK parameters
!               - Check for unused PK params                 
!               - Set up dummy PK parameter for any S,F,FZ,T,FR which is identically 1
!               - Now initialize the dummy parameter
!               - Count number of pk params which are modelled as logs and save indices
!               - Initialize infusion counters
!               - Set up indxs values - 0 is default
!               - Pass JDEF to PRED in case of no time data item
!               - Initialize call variables
!               - Write messages to the output file
!               - Start ERROR subroutine descriptive material
!               - Stop if any errors so far
!               - Initialize arrays
!               - Allows library routines to read new instructions
!               - If the DES routine does not reset IDEF(1), assume max. no. of THETAS
!               - DES was called, maybe AES too
!               - Report on AES first if there exist equilibrium compartments
!               - New library or generated subroutine sets IDEFD(2) = 0 (compressed)
!                 or IDEFD(2)=1 (full) or IDEFD(2)=2 (library subr; retain old IDEFD(2)=0 info)
!                 old one leaves it 1, which defaults to full.
!               - Reinitialize full arrays in case the DES code changed
!               - Compact storage in DES
!               - Perform additional housekeeping
!                 - Allow library subroutines to re-position and re-read FLIB
!               - Call routine to check data records for consistency
!               - Also checks array sizes in subroutines -  Call CHECK
!               - Return
!
! MODULES USED: PRSIZES,NMDATA,PRDATA,NM_BAYES_INT,NMPRD_INT,PRCM_INT,PRCOM_INT,
!               PRMOD_INT,PROCM_INT,NMPRD_REAL,PRCM_REAL,PRCOM_REAL,PROCM_REAL,
!               PRMOD_CHAR,PROCM_CHAR,PRCM_LOG,PRCOM_LOG,NM_INTERFACE,PR_INTERFACE
!
! CONTAINS    : NONE
!
! LOCAL'S     : COTHER,I,IINFN,ISIDEF,ISTOP,ISTOPT,IUSED,J,JD,JDEF,JDO,JJ,K,KCOMP,
!               LINE,M,MAXI,MAXJ,MAXK,MM,MMSQ,NAMEFL,NBP1,NEP1,PE,SIMTXT,ST1FL,
!               STR,STR1,TLIST,TRUNTH
!
!---------------------------- END OF HEADER -----------------------------------------
!
      SUBROUTINE PREDI(ICALL,NEWIND,THETA,INDXS,F,H,IDEF)
!
      USE PRSIZES,      ONLY: DPSIZE,ISIZE,PAL,PC,PCT,PD,PG,PIR,MAXIC
      USE VERSION,      ONLY: LEV
      USE NMDATA,       ONLY: IUN
      USE PRDATA,       ONLY: TEXT,LETT,DASH,STAR,TDIGIT,CNOYES,COFFON,OLDFMT
! INTEGER
      USE NM_BAYES_INT, ONLY: TOLTOPRED
      USE NMPRD_INT,    ONLY: IERPRD,IPRNV,IPROB,IPS,IQUIT,IRECRSIV,ISETNTH,NITSUP,  &
                              NOEPS,NTHETA,NWEPS,NWETA,NWIND,NWTHT,NETAZ,NEPSZ
      USE PRCM_INT,     ONLY: AA,IAI,IAJ,IAK,INRD,IPI,IPJ,IPK,ITI,ITK,ITOTL,LNCM1,   &
                              LNCM2,MAP,MAPINV,NBRON,NIAI,NIAI1,NIPI,NIPI1,NIT,NIT1, &
                              PP,PRMC,PRME,PRMG,PRMT,TT,XAA,XIAI,XIAJ,XIAK,XIPI,XIPJ,&
                              XIPK,XITI,XITK,XLNCM1,XNBRON,XNC,XNCM1,XNIAI,XNIAI1,   &
                              XNIPI,XNIPI1,XNIT,XNIT1,XPP,XTT
      USE PRCOM_INT,    ONLY: ADVID,CALLA9,CALLER,CALLID,CALLPK,CPYTHE,ERPROB,IATT,  &
                              IBF,ID,IDC,IDNO,IDO,IERRA,IFORM,IFR,IHEAD,IINST,ILAG,  &
                              ILIB,ILINK,IMTBEG,IMTEND,INUM,IP,IPKA,IPKA0,IPOOL,IREV,&
                              IRGG,IRR,IS,ISV,ITRANS,ITSC,ITURN,IXUSER,JAMT,JCOMPF,  &
                              JCOMPT,JCONT,JDELTA,JDUM,JERROR,JEVENT,JMORE,JRATE,JSS,&
                              JTIME,KAES,KDES,KTOL,LINK,LOGUNT,MAXKF,MCNTR,MCOMP,    &
                              MMAX,MTNO,NBP,NC,NCM1,NFPR,NOPROB,NP,NPEPS,NPETAS,NRD, &
                              PKPROB,SSID,XNPETA,YFORM
      USE PRMOD_INT,    ONLY: I_SS,ICALLD,IDEFA,IDEFD,ISSMOD
      USE PROCM_INT,    ONLY: NEVENT,PNEWIF,IDXETA !7.1.1
      USE PRDIMS,       ONLY: GPRD,HPRD,GERD,HERD,GPKD
! REAL
      USE NMPRD_REAL,   ONLY: ETA,EPS,PASSRC
      USE PRCM_REAL,    ONLY: DAC,DPC,DTC
      USE PRCOM_REAL,   ONLY: ADTE,D2ADTE,D2TINT,DA,DET,DP,DTINT,G3,HH,ONE,TSTART,XD,&
                              XR,YMULT,ZERO,DDELTA,DTSTAR,D2DELT,D2TSTA !7.1.1
      USE PROCM_REAL,   ONLY: AMNT,D2AETA,D2DOST,DAETA,DDOST,DOSREC,DOSTIM,EVTREC
! CHARACTER
      USE PRMOD_CHAR,   ONLY: NAME
      USE PROCM_CHAR,   ONLY: FMT
! LOGICAL
      USE PRCM_LOG,     ONLY: CALL9,CALLE,CALLP,COMPAC,DIDAES,DIDCAA,DIDDES,DOFINL,  &
                              GENMOD,MAPPED,TIMDEF
      USE PRCOM_LOG,    ONLY: DTIME,NEWWAY,NOETAS,PF,XNOETA
! INTERFACE
      USE NM_INTERFACE, ONLY: ERRORMSGS
      USE PR_INTERFACE, ONLY: CHECK,ERROR,INFN,PK,SADVAN,TRANS
!
      IMPLICIT NONE
!
      INTEGER(KIND=ISIZE), INTENT(IN)     :: ICALL,NEWIND
      INTEGER(KIND=ISIZE), INTENT(IN OUT) :: INDXS(*),IDEF(7*PC)
!
      REAL(KIND=DPSIZE),   INTENT(IN)     :: THETA(*)
      REAL(KIND=DPSIZE),   INTENT(OUT)    :: H(HPRD,*) !7.2
      REAL(KIND=DPSIZE),   INTENT(IN OUT) :: F
!
      SAVE
!
!------------------------------------------------------------------------
!     INTEGER ICALL,NEWIND,INDXS
!     COMMON /ROCM29/ IPS
!     INTEGER IPS
!     COMMON /CM1/ NTHETA,ISETNTH
!     INTEGER NTHETA,ISETNTH
!     COMMON /CM1/ RC0101(LTH),IC0101(LTH),RC0102(LTH),IC0102(LTH+1)
!     REAL RC0101,RC0102
!     INTEGER IC0101,IC0102
!     COMMON /CM2/ NETAS,NEPS,IFLAG
!     COMMON /CM2/ IC0202(LVR,LVR),IC0203,IC0204(LVR,LVR),IC0205(LVR,2)
!     INTEGER IC0202,IC0203,IC0204,IC0205
!     COMMON /ROCM14/ NPROB,IPROB,S1SC,S2SC,S1NIT,S2NIT,S1IT,S2IT
!     COMMON /ROCM28/ IPRNV
!     COMMON /ROCM28/ IDUM28
!     INTEGER IDUM28
!     COMMON /ROCM34/ NWIND
!     COMMON /ROCM35/ NWTHT,NWETA,NWEPS
!     INTEGER NWTHT,NWETA,NWEPS
!     COMMON /NMPRD1/ IERPRD,NETEXT
!     INTEGER IERPRD,NETEXT
!     COMMON /NMPRD7/ ETA(PE),EPS(PE)
!     DOUBLE PRECISION ETA,EPS
!     COMMON /NMPRD8/ IRECRSIV
!     INTEGER IRECRSIV
!     COMMON /NMPRD9/ PASSRC(20)
!     REAL PASSRC
!     INTEGER NETAS,NEPS,IFLAG,LOGUNT,IRGG,IREV,ITRANS
!     INTEGER NPROB,IPROB,S1SC,S2SC,S1NIT,S2NIT,S1IT,S2IT,IPRNV,NWIND 
!     INTEGER NEWIND
!     DIMENSION THETA(*),DATREC(*),INDXS(*),H(PE,*)
!     INTEGER ICALL,INDXS,NEWIND
!     DOUBLE PRECISION F,H,THETA
!     DOUBLE PRECISION DATREC
!     COMMON /PRCM00/ MC(6),ME(6),MG(6),MT(6)
!     COMMON /PRCOM0/ NP,NBP,YFORM
!     COMMON /PRCOM0/ MAXKF,IFORM
!     COMMON /PRCOM0/ IDC,IDO,MAXIC,ISV,IINST,ITURN
!     COMMON /PRCOM0/ JTIME,JCONT,JEVENT,JAMT,JRATE,JSS,JDELTA
!     COMMON /PRCOM0/ JCOMPT,JCOMPF,JERROR,SSC,KREC,JMORE,JDUM
!     COMMON /PRCM02/ ITOTL,INRD
!     COMMON /PROCM7/ EVTREC(5,PD+1),N
!     DOUBLE PRECISION EVTREC
!     COMMON /PROCM8/ FMT
!     CHARACTER*80 FMT(3)
!     COMMON /PRCOM1/ NOETAS,SECOND
!     COMMON /PRCOM2/ IBF,IRR,IS,ID,ITSC,IFR,ILAG
!     COMMON /PRCOM3/ ITRANS,IRGG,IREV,NPETAS,NPEPS
!     COMMON /PRCOM4/ G3,HH,DELTA,DT,DTE
!     COMMON /PRCOM4/ YMULT,ZERO,ONE,XR,XD,TSTART,DTSTAR
!     COMMON /PRCOM4/ DDELTA,D2DELT,ADTE,D2ADTE
!     COMMON /PRCOM5/ ISPEC,DCTR,BETA,DD
!     COMMON /PRCOM5/ IP,IPOOL,IHEAD,INEXT,IBACK,SV
!     COMMON /PRCOM7/ ADVID,SSID
!     COMMON /PRCOME/ NRD,MCOMP,NCM1,IH
!     COMMON /PRCOML/ IDNO,IATT,LINK,ILINK,INUM
!     COMMON /PRCOMS/ ILIB,ISUB,KDES,KTOL,KAES,NOPROB,PKPROB,ERPROB,NFPR
!     COMMON /PRCOMI/ IXUSER,IDEF,XNPETA,XNOETA
!     COMMON /PRNAME/ NAME
!     CHARACTER*8 NAME(PC)
!     COMMON /PRCOMN/ LOGUNT,NC
!     COMMON /PRCOMJ/ MTNO,IMTBEG,IMTEND,IMTGG,IMT,MTCNTR,MTPTR
!     INTEGER MTNO,IMTBEG,IMTEND,IMTGG(PCT),IMT(PCT),MTCNTR,MTPTR
!     INTEGER IXUSER,IDEF(7*PC),XNPETA
!     LOGICAL XNOETA
!     INTEGER ILIB,ISUB,KDES,KTOL,KAES,NOPROB,ERPROB,PKPROB,NFPR
!     INTEGER IDNO,IATT,LINK,ILINK,INUM ! NRD,MCOMP,NCM1,IH,
!     DIMENSION IATT(PM,9),LINK(PM,PC),ILINK(PM,PC),INUM(PM)
!     INTEGER IDEF(7,PC)
!     EQUIVALENCE (IDEF(1),IDEF(1,1))
!     LOGICAL ISIDEF
!     DOUBLE PRECISION DELTA,G3,HH
!     DOUBLE PRECISION YMULT,ZERO,XR,XD
!     DOUBLE PRECISION ONE,TSTART,DTSTAR(PE)
!     DOUBLE PRECISION DDELTA(PE),D2DELT(PE,PE),ADTE(PE),D2ADTE(PE,PE)
!     DOUBLE PRECISION DT,DTE(PE)
!     DIMENSION G3(PG+1,PE+1,PE+1),HH(PE,PE)
!     INTEGER MC,ME,MG,MT
!     INTEGER LOGUNT,IRGG,IREV,ITRANS
!     INTEGER N ! NPETAS,NPEPS,
!     INTEGER JCONT,JTIME,JEVENT,JAMT,JRATE,JSS,JDELTA
!     INTEGER JCOMPT,JCOMPF,JERROR
!     INTEGER NC,IDC,IDO,NP,NBP,SSC,KREC,JMORE,JDUM
!     INTEGER ISV(PC),IBF(PC),IRR(PC),SV(PC)
!     INTEGER IINST(PC),ITURN(PC),ITSC,IFR,ILAG(PC),IS(PC),ID(PC)
!     INTEGER ITOTL(PC),INRD(PM)
!     INTEGER ADVID,SSID,MAXKF,IFORM(PG+1),YFORM,MAXIC
!     INTEGER BETA(90),IPOOL(90),IP,IHEAD,INEXT(90),IBACK(90)
!     INTEGER ISPEC,DD(90),DCTR
!     LOGICAL NOETAS,SECOND
!     COMMON /PRCMX1/ GENMOD,MAPPED,COMPAC
!     LOGICAL GENMOD,MAPPED,COMPAC
!     COMMON /PRCMX2/ XNC,XNCM1,MAP,MAPINV,NBRON,XNBRON
!     INTEGER XNC,XNCM1,MAP(PC),MAPINV(PC),NBRON,XNBRON
!     COMMON /PRCOMM/ MMAX,MCNTR
!     INTEGER MMAX,MCNTR
!     COMMON /PRCMLS/ XLNCM1,XLNCM2,LNCM1,LNCM2
!     INTEGER XLNCM1,XLNCM2,LNCM1,LNCM2
!     COMMON /PRCOMT/ DTIME,LFLAG,DTDER
!     LOGICAL DTIME,LFLAG,DTDER
!     COMMON /PROCM4/ A,DAETA,D2AETA
!     DOUBLE PRECISION A,DAETA,D2AETA
!     DIMENSION A(PC),DAETA(PC,PE),D2AETA(PC,PE,PE)
!     COMMON /PRCOMU/ NEWWAY
!     LOGICAL NEWWAY
!     COMMON /PRCOMA/ IERRA,IPKA0,IPKA
!     INTEGER IERRA,IPKA0,IPKA
!     COMMON /PROCM2/ DOSTIM,DDOST,D2DOST
!     DOUBLE PRECISION DOSTIM
!     DOUBLE PRECISION DDOST(PE),D2DOST(PE,PE)
!     COMMON /PROCM3/ DOSREC
!     DOUBLE PRECISION DOSREC(PD)
!     COMMON /PROCM1/ PNEWIF
!     INTEGER PNEWIF
!     COMMON /PRCOMX/ DTINT(PE),D2TINT(PE,PE)
!     DOUBLE PRECISION DTINT,D2TINT
!     COMMON /PRCOMC/ CALLER,CALLPK,CALLID,CALLA9,CPYTHE
!     INTEGER CALLER,CALLPK,CALLID,CALLA9,CPYTHE
!     COMMON /PRDDE1/ICALLD,IDEFD(2),IDEFA(2)
!     INTEGER ICALLD,IDEFD,IDEFA
!     COMMON /PRCM01/ CALLP,CALLE,CALL9,TIMDEF
!     LOGICAL CALLP,CALLE,CALL9,TIMDEF
!     COMMON /PRCM03/ DIDCAA,DIDDES,DIDAES,DOFINL
!     LOGICAL DIDCAA,DIDDES,DIDAES,DOFINL
!     COMMON /PRDPK4/ I_SS,ISSNOW,ISSMOD
!     INTEGER I_SS,ISSNOW,ISSMOD
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
!     COMMON /PRCMDC/ DAC(PIR,1),DPC(PIR,1),DTC(PIR)
!     DOUBLE PRECISION DAC,DPC,DTC
!     COMMON /PRCOMD/ DA,DP,DET,HR,HETA,H2ETA
!     DOUBLE PRECISION DA(PM,PM),DP(PM,PG),HR(PM,2),DET(PM)
!     DOUBLE PRECISION HETA(PM,PE,2),H2ETA(PM,PE,PE,2)
!     COMMON /PRCOMP/ PF
!     LOGICAL PF
!     INTEGER TOLTOPRED
!     COMMON /TOLTOPRED_COMMON/ TOLTOPRED
!------------------------------------------------------------------------------------
!
! Local Variables
!
      INTEGER(KIND=ISIZE), PARAMETER :: PM=PC-1
      INTEGER(KIND=ISIZE), PARAMETER :: PALZ=MAX(PAL,PCT+20)

      INTEGER(KIND=ISIZE) :: I,IINFN,ISTOP,ISTOPT,J,JD,JDEF,JDO,JJ,K,KCOMP,M,MM,MAXI,&
                             MAXJ,MAXK,MMSQ,NAMEFL,NBP1,NEP1,ST1FL,TRUNTH,IUSED(PG)
!
      CHARACTER(LEN=3)    :: TLIST(PC),COTHER(2)   ! Internal arrays for printing TEXT
      CHARACTER(LEN=72)   :: SIMTXT,STR1,STR
      CHARACTER(LEN=133)  :: LINE
!
      LOGICAL :: ISIDEF
!
      TDIGIT(1) = '  *'
      DO I=1,999
        TDIGIT(I+1)=' '           ! Generates 1,2...10,11,...99 labels in DIGIT2 array
        WRITE (STR(1:3),'(I3)') I
        TDIGIT(I+1)=STR
      END DO
!
! Modifications: 7/86 9/86 9/87 3/88 9/90; Always call PK, ERROR
!    
! For implementation of general ADVANS' compartment ON/OFF feature
! GENMOD true for general ADVAN (set by PREDI)
! XNC, XNCM1, XNBRON are original values of NC, NCM1, NBRON (set by PREDI)
! NBRON tells how many compartments are now ON (not counting output)
! MAPPED is true when a mapping in effect (set by SADVAN, SSS)
! MAP, MAPINV: Maps 'real' compartment nos. to 'reduced set' and V.V.     
!      
! LNCM1 = Number of DES-type compartments (XLNCM1 is original no.)
! LNCM2 = Number of AES-type compartments (XLNCM1 is original no.)        
!
      NEWWAY=.TRUE. ! Bioavailability fraction applies to all doses
      LOGUNT=IUN(6)
      IF (TOLTOPRED /= 0) LOGUNT=2050
      ICALLD=ICALL
      K=ICALL+1
!      
      SELECT CASE (K)
      CASE (1)     ! ICALL=0  Model initialization
        ILIB=0; ZERO=0.0D0; PRMC(1:6)=0; NOPROB=-1; ISTOPT=0; D2AETA=ZERO; DDELTA=ZERO 
        KDES=0; XR=-1.0D0;  PRME(1:6)=0; ERPROB=-1; NAMEFL=0; DAETA=ZERO;  DTSTAR=ZERO
        KAES=0; XD=-2.0D0;  PRMG(1:6)=0; PKPROB=-1; CPYTHE=0; D2DELT=ZERO; ISTOP=0
        KTOL=0; ONE=1.0D0;  PRMT(1:6)=0; NFPR=-1;   IDXETA=0; D2TSTA=ZERO; MCOMP=PM
!
        TSTART=ZERO; ETA=ZERO;  DDOST=ZERO;  COMPAC=.TRUE.; MMAX=PALZ; IREV=5
        DOSTIM=ZERO; EPS=ZERO;  DOSREC=ZERO; DTIME=.FALSE.; PF=.TRUE.; NAME=' '
        D2DOST=ZERO; AMNT=ZERO; IRGG=PG+1;   IXUSER=12;     MCNTR=0
!
! DDOST(1:F_LVR)=ZERO; ETA(1:F_LVR)=ZERO; DOSREC(1:PD)=0.0; EPS(1:F_LVR)=ZERO; MAXIC=90
!
        WRITE (LOGUNT,30,ERR=502) TRIM(LEV)
!
        IF (PC > 999) THEN        ! Check for bad values in SIZES
          WRITE (LOGUNT,238,ERR=502) '    PC',PC,999
          WRITE (LOGUNT,237,ERR=502)
          IQUIT=1; GO TO 999
        END IF
!        
        IF (PG-PCT > 999) THEN
          WRITE (LOGUNT,238,ERR=502) 'PG-PCT',PG-PCT,999
          WRITE (LOGUNT,237,ERR=502)
          IQUIT=1; GO TO 999
        END IF
!
        WRITE (FMT(1),993) NWETA ! Init. format statements for writes of complete arrays
        WRITE (FMT(2),993) NWEPS
        WRITE (FMT(3),993) NWTHT
!        
        IINFN=ICALL              ! Initial call to user initialization/termination routine
        CALL INFN(IINFN,THETA,PASSRC,INDXS(IXUSER),NWIND)
        IF (IQUIT == 1 .OR. IERPRD > 0) GO TO 999
        IF (IINFN > 0 .AND. IINFN <= 8999) WRITE (LOGUNT,241,ERR=502) IINFN        
!
        ADVID=0;  IDNO=0; ISSMOD=-1; NBP=0
        XLNCM1=0; NCM1=0; I_SS=-1
!        
        CALL SADVAN(ICALL,0)  !7.2b52b
        IF (IERPRD > 0 .OR. IQUIT == 1) GO TO 999
!        
        IF (ADVID < 5 .OR. ADVID == 10 .OR. ADVID == 11 .OR. ADVID == 12) THEN  
          NCM1=NC-1     ! For analytic ADVANs
          XNC=NC        ! SS6 with ADVAN10 uses XNCM1
          XNCM1=NCM1
          MAPPED=.FALSE.
          GENMOD=.FALSE.
          NAMEFL=1
          IF (ADVID == 12) GENMOD=.TRUE.! ADVAN12 uses compressed arrays when DEPOT is off
        ELSE                            ! Finished with ADVANs
          WRITE (LOGUNT,125,ERR=502) IDNO,NBP   ! Check and digest the model subroutines output
          IF (NCM1 <= 0) THEN
            WRITE (LOGUNT,8001,ERR=502) NCM1 
            WRITE (LOGUNT,8099,ERR=502)
            IQUIT=1; GO TO 999
          END IF
          IF (NCM1 > MCOMP) THEN
            WRITE (LOGUNT,8002,ERR=502) NCM1
            WRITE (LOGUNT,8099,ERR=502)
            IQUIT=1; GO TO 999
          END IF
          NC=NCM1+1
! Allow no basic params with DES
          IF ((NBP <= 0 .OR. NBP >= IRGG) .AND. (NBP /= 0 .OR.      &
              (ADVID /= 6 .AND. ADVID /= 8 .AND. ADVID /= 9 .AND. ADVID /= 13))) THEN   ! ajb 3/2009              
            IF (NBP <= 0) THEN
              WRITE (LOGUNT,8015,ERR=502) 
            ELSE
              WRITE (LOGUNT,8005,ERR=502) PG
            END IF
            WRITE (LOGUNT,8099,ERR=502)
            IQUIT=1; GO TO 999
          END IF
!         
          DO I=1,NCM1   ! Define the individual compartments
            ISV(I)=IATT(I,1)
            ITURN(I)=IATT(I,2)
            IINST(I)=IATT(I,3)
            ITOTL(I)=IATT(I,9)
            IF (IATT(I,4) == 1) IDO=I
            IF (IATT(I,5) == 1) IDC=I
            IF (NAME(I) /= ' ') NAMEFL=1
          END DO
! Check for error in compartment definitions; Allow no dose compartment with ADVAN9
          IF ((IDC <= 0 .OR. IDC > NCM1) .AND. (IDC /= 0 .OR. ADVID /= 9)) THEN
            WRITE (LOGUNT,8003,ERR=502) IDC
            WRITE (LOGUNT,8099,ERR=502)
            IQUIT=1; GO TO 999
          END IF
!        
          IF (IDO <= 0 .OR. IDO > NC) THEN
            WRITE (LOGUNT,8004,ERR=502) IDC
            WRITE (LOGUNT,8099,ERR=502)
            IQUIT=1; GO TO 999
          END IF
!     
          IF (IDC /= 0 .AND. IINST(IDC) /= 1) THEN
            WRITE (LOGUNT,8003,ERR=502) IDC
            WRITE (LOGUNT,8099,ERR=502)
            IQUIT=1; GO TO 999
          END IF
!    
          ISV(NC)=0     ! Define the output compartment
          IINST(NC)=0
          ITURN(NC)=1
          ITOTL(NC)=1
          NAME(NC)=TEXT
          GENMOD=.TRUE. ! For on/off feature
          XNC=NC
          XNCM1=NCM1
          MAPPED=.FALSE.
          DIDCAA=.FALSE.! For call DES, call AES feature
          DOFINL=.FALSE.
          DIDDES=.FALSE.
          DIDAES=.FALSE.
        END IF 
!        
        SSID=0          ! Finished with ADVANs
        CALL SSS
!
        IF (IERPRD > 0 .OR. IQUIT == 1) GO TO 999
!        
! Check if link must be analyzed
        IF ((SSID == 5 .OR. SSID == 7 .OR. ADVID == 5 .OR. ADVID == 7)) THEN
          WRITE (LOGUNT,8007,ERR=502) (I,I=1,NC)
          WRITE (LOGUNT,8008,ERR=502) ! 99 in format specification should really be PC
          TDIGIT(1)=DASH
          ST1FL=0
          DO I=1,NCM1
            DO K=1,NC
              TLIST(K)=TDIGIT(LINK(I,K)+1)
              IF (I == K) TLIST(K)=STAR
              IF (LINK(I,K) == 0 .AND. I /= K) ST1FL=1
            END DO
            WRITE (LOGUNT,8009,ERR=502) I,(TLIST(K),K=1,NC) ! 99 in format specification should really be PC
          END DO
          WRITE (LOGUNT,8011)
          IF (ST1FL == 1) WRITE (LOGUNT,8012,ERR=502)
          ISTOP=0
          DO I=1,NCM1
            INUM(I)=0
            DO K=1,NC           ! Look for links from compartment I to compartment K
              IF (LINK(I,K) == 0) CYCLE
              IF (I == K) THEN  ! JJ is the GG row with the rate constant K(I,K)
                WRITE (LOGUNT,8010,ERR=502) I  
                ISTOP=1; CYCLE
              END IF
              JJ=LINK(I,K)
              IF (JJ > NBP) THEN
                WRITE (LOGUNT,8006,ERR=502) JJ
                ISTOP=1; CYCLE
              END IF    
              INUM(I)=INUM(I)+1 ! Save K for later entries
              ILINK(I,INUM(I))=K
            END DO
          END DO
          IF (ISTOP == 1) THEN
            WRITE (LOGUNT,8099,ERR=502)
            IQUIT=1; GO TO 999
          END IF
        END IF
!
        ITRANS=0        ! Initial call to translator
        CALL TRANS(ITRANS,IRGG,G3,NPETAS)
        IF (IERPRD > 0) GO TO 999
        IF (ITRANS < 9000) WRITE (LOGUNT,240,ERR=502) ITRANS
        TDIGIT(1)=STAR  ! Echo print the model definition information
!        
        IF (ADVID /= 9 .OR. .NOT. OLDFMT) THEN
          WRITE (LOGUNT,2100,ERR=502)
        ELSE
          WRITE (LOGUNT,2101,ERR=502)
        END IF
!        
        NBRON=0;   COTHER(1)=' '
        MAP(NC)=0; COTHER(2)=' '
!        
        DO I=1,NC
          IF (I <= MCOMP) THEN  ! For on/off feature
            IF (ISV(I) == 1) NBRON=NBRON+1
            MAPINV(I)=I
            MAP(I)=I
          END IF
          JD=1                  ! End on/off feature
          IF (I == IDC) JD=2
          JDO=1
          IF (I == IDO) JDO=2
          IF (ADVID == 9) THEN
            COTHER(2)=CNOYES(ITOTL(I)+1)
            COTHER(1)=CNOYES(1)
            IF (I <= MCOMP) COTHER(1)=CNOYES(IATT(I,8)+1)
          END IF
          IF (NAMEFL == 1 .OR. I == NC) THEN ! Always use name array for output compartment
            WRITE (LINE,219) I,NAME(I),COFFON(ISV(I)+1),CNOYES(ITURN(I)+1),            &
                             CNOYES(IINST(I)+1),CNOYES(JD),CNOYES(JDO),COTHER(1),COTHER(2)
          ELSE
            WRITE (LINE,220) I,(IATT(I,J),J=6,7),COFFON(ISV(I)+1),CNOYES(ITURN(I)+1),  &
                             CNOYES(IINST(I)+1),CNOYES(JD),CNOYES(JDO),COTHER(1),COTHER(2)
          END IF
          IF (ADVID == 9 .AND. OLDFMT) THEN
            WRITE (LOGUNT,'(A96)',ERR=502) LINE
          ELSE
            WRITE (LOGUNT,'(A74)',ERR=502) LINE
          END IF
        END DO
!        
        IF (ADVID == 9 .AND. .NOT. OLDFMT) THEN
          WRITE (LOGUNT,2102,ERR=502)
          DO I=1,NC
            COTHER(2)=CNOYES(ITOTL(I)+1)
            COTHER(1)=CNOYES(1)
            IF (I <= MCOMP) COTHER(1)=CNOYES(IATT(I,8)+1)
            IF (NAMEFL == 1 .OR. I == NC) THEN
              WRITE (LINE,219) I,NAME(I),COTHER(1),COTHER(2)
            ELSE
              WRITE (LINE,220) I,(IATT(I,J),J=6,7),COTHER(1),COTHER(2)
            END IF
            WRITE (LOGUNT,'(A41)',ERR=502) LINE
          END DO
        END IF
! Special checks for ADVAN9
        IF (ADVID == 9 .AND. LNCM2 > 0) THEN 
          DO I=LNCM1+1,NCM1
            IF (IINST(I) == 1) THEN
              WRITE (LOGUNT,8902,ERR=502)
              WRITE (LOGUNT,8099,ERR=502)
              IQUIT=1; GO TO 999
            END IF
          END DO
        END IF        
        XNBRON=NBRON 
! End of special checks for ADVAN9
!
        IF (SSID == 6) THEN
          WRITE (LOGUNT,2501,ERR=502)
        ELSE
          IF (SSID == 5 .AND. ADVID /= 5) WRITE (LOGUNT,2502,ERR=502)
        END IF
! 
        IF (SSID == 7 .AND. ADVID /= 7) WRITE (LOGUNT,2503,ERR=502)       
! 
        IF ((SSID /= 0 .AND. SSID /= 6 .AND. SSID /= ADVID) .AND.  &
            (SSID /= 5 .OR. (ADVID /= 6 .AND. ADVID /= 8)) .AND.   &
            (SSID /= 7 .OR. (ADVID /= 6 .AND. ADVID /= 8))) THEN       
          WRITE (LOGUNT,2301,ERR=502)
          WRITE (LOGUNT,8098,ERR=502)
          IQUIT=1; GO TO 999
        END IF
!
        IF (ISSMOD > 0) WRITE (LOGUNT,3484,ERR=502) ISSMOD  ! Added 10/2007
!        
        IF (ADVID == 9 .OR. ADVID == 13) THEN                      ! ajb 3/2009        
          DO J=1,NCM1
            IF (INRD(J) == 0) THEN  
              WRITE (LOGUNT,997,ERR=502) (INRD(I),I=1,J-1)
              GO TO 999 
            END IF  
          END DO
          J=NCM1+1
          WRITE (LOGUNT,997,ERR=502) (INRD(I),I=1,J-1)
          GO TO 999
        END IF     
!        
        IF (ADVID == 6 .OR. ADVID == 8 .OR. ADVID == 10 .OR. SSID == 6 .OR.        &
            SSID == 10) WRITE (LOGUNT,998,ERR=502) NRD
        GO TO 999
      CASE (2)
        GO TO 1000
      CASE (3,4)      ! ICALL=3  Termination entry
        IINFN=ICALL   ! Call user routine for transgeneration, etc.
        CALL INFN(IINFN,THETA,PASSRC,INDXS(IXUSER),NWIND)
        GO TO 999     ! IF (IERPRD > 0) GO TO 999
      CASE DEFAULT    
        CALL ERRORMSGS(308)   ! ' ERROR IN PREDI ROUTINE. VARIABLE "K" IS NOT IN THE RANGE (1-4)'
        GO TO 999
      END SELECT
!      
! ICALL=1  Problem initialization
 1000 IRECRSIV=1 ! Tell NONMEM "recursive" PRED  9/97
                 ! Set up information obtained from NONMEM
                 ! Whether or not to print normal initialization messages in superprobs.
      IF (NITSUP(1) > 1 .OR. NITSUP(2) > 1) THEN
        PF=IPRNV(1) == 1       ! Modified IPRNV to IPRNV(1)
      ELSE
        PF=.TRUE.
      END IF
!     
      IF (IPS /= 2) THEN
        NPETAS=NETAZ
        NPEPS=0
        IF (NOEPS /= 9999) NPEPS=NEPSZ
        NPEPS=NEPSZ     
      ELSE
        NPETAS=0
        NPEPS=NETAZ
        H(1,1)=ZERO            ! Initialize H(1,1) to zero. It will never change.
      END IF
!      
      NEP1=NPETAS+1
      NOETAS=NPETAS == 0
      XNOETA=NOETAS
      XNPETA=NPETAS
! Initialize format statements for writes of complete arrays
      WRITE (FMT(1),993) NWETA    
      WRITE (FMT(2),993) NWEPS
      WRITE (FMT(3),993) NWTHT
!      
      HH(1:NPEPS,1:NPETAS+1)=ZERO ! Set up for call to ERROR
      IDEF(1)=0
      IDEF(2)=-1                  ! Initialize call flag position to -1
      IDEF(3)=-1                  ! Don't know if ERROR uses A explicitly
      YMULT=ONE
      PNEWIF=NEWIND
!      
      CALL ERROR(ICALL,IDEF,THETA,IREV,EVTREC,NEVENT,INDXS(IXUSER),F,AMNT,HH)
!      
      IF (IERPRD > 0) GO TO 999
!      
      IF (IDEF(1) /= 0 .AND. IDEF(1) /= 1) THEN
        WRITE (LOGUNT,4010,ERR=502) 'IDEF(1)',IDEF(1)
        WRITE (LOGUNT,8097,ERR=502)
        IQUIT=1; GO TO 999
      END IF
!   
      IF (IDEF(2) < -1 .OR. IDEF(2) > 2) THEN
        WRITE (LOGUNT,4010,ERR=502) 'IDEF(2)',IDEF(2)
        WRITE (LOGUNT,8097,ERR=502)
        IQUIT=1; GO TO 999
      END IF
!      
      YFORM=IDEF(1)
      CALLER=IDEF(2)+2
! Does ERROR refer explicitly to A? IDEF(3)=0 if the answer is no.
      IF (IDEF(3) /= -1 .AND. IDEF(3) /= 0 .AND. IDEF(3) /= 1) THEN   
        WRITE (LOGUNT,4010,ERR=502) 'IDEF(3)',IDEF(3)
        WRITE (LOGUNT,8097,ERR=502)
        IQUIT=1; GO TO 999
      END IF
!  
      IERRA=IDEF(3)
      IINFN=ICALL   ! Allow user fixup of data
      CALL INFN(IINFN,THETA,PASSRC,INDXS(IXUSER),NWIND)
      IF (IQUIT == 1 .OR. IERPRD > 0) GO TO 999
!
      G3(1:IRGG,1,1)=ZERO   ! Set up for call to PK
      IDEF(1:7*PC)=0
      IDEF(1+7)=-1        ! Initialize call flag position to -1 (new and old IDEFS)
      IDEF(1+(3-1)*7)=-1  ! Don't know if PK initializes A0
      IDEF(1+(4-1)*7)=-1  ! Don't know if PK uses A explicitly
      K=0
!      
      DO I=1,NC
       IF (IINST(I) /= 0) K=K+1
      END DO 
! 
      IDEF(NC+3*K+3)=-1
      KCOMP=K   ! Added 10/2007. BUG when no dosable compartments.
      I_SS=-1
      PNEWIF=NEWIND
      CALL PK(ICALL,IDEF,THETA,IREV,EVTREC,NEVENT,INDXS(IXUSER),IRGG,G3,NPETAS)
      IF (IERPRD > 0 .OR. IQUIT == 1) GO TO 999
      IF (PF) WRITE (LOGUNT,1200,ERR=502)
!
      NP=NBP    ! Get and check indices of PK params from IDEF array
      ISIDEF=.TRUE.
!      
      IF (IDEF(1) /= -9) THEN   
! Format is that of old IDEF
! Fix up location of new "CALL PK" flag unless PK changed it not if model has 1 
! CMT: IDEF(2,1) coincides with IDEF(8)
        IF (NC /= 2) THEN
          IF (IDEF(1+7) == -1) IDEF(1+7)=0
        END IF
        K=0
        IUSED=ZERO
        DO I=1,PG
          IF (IDEF(I) > K) K=IDEF(I)
!          IUSED(I)=0
        END DO
        IF (K > PG) THEN    ! Check largest number of PK parameter specified
          WRITE (LOGUNT,1210,ERR=502)
          WRITE (LOGUNT,1430,ERR=502) (IDEF(I),I=1,PG)
          WRITE (LOGUNT,8096,ERR=502)
          IQUIT=1; GO TO 999
        END IF   
        K=NC+1
        NP=NBP              ! BUG fix 10/87 AJB; NP stayed 0 when no additional parms
        DO I=1,NC           ! Get scale and bio-availablity indices
          IS(I)=IDEF(I)
          IF (IDEF(I) > 0) IUSED(IDEF(I))=1
          IF (IDEF(I) > NP) NP=IDEF(I)
          IBF(I)=0
          IF (IINST(I) == 0) CYCLE
          IBF(I)=IDEF(K)
          IF (IDEF(K) > 0) IUSED(IDEF(K))=1
          IF (IDEF(K) > NP) NP=IDEF(K)
          K=K+1
        END DO
        DO I=1,NC           ! Get rate indices
          IRR(I)=0
          IF (IINST(I) == 0) CYCLE
          IRR(I)=IDEF(K)
          IF (IDEF(K) > 0) IUSED(IDEF(K))=1
          IF (IDEF(K) > NP) NP=IDEF(K)
          K=K+1
        END DO
        DO I=1,NC           ! Get duration indices
          ID(I)=0
          IF (IINST(I) == 0) CYCLE
          ID(I)=IDEF(K)
          IF (IDEF(K) > 0) IUSED(IDEF(K))=1
          IF (IDEF(K) > NP) NP=IDEF(K)
          K=K+1
        END DO
        IFR=IDEF(K)         ! Get output fraction
        IF (IDEF(K) > 0) IUSED(IDEF(K))=1
        IF (IDEF(K) > NP) NP=IDEF(K)
        K=K+1
        ITSC=IDEF(K)        ! Get time scale
        IF (IDEF(K) > 0) IUSED(IDEF(K))=1
        IF (IDEF(K) > NP) NP=IDEF(K)
        CALLPK=IDEF(K+1)    ! Get call PK flag
        K=K+2
        DO I=1,NC           ! Get time lag indices
          ILAG(I)=0
          IF (IINST(I) == 0) CYCLE
          ILAG(I)=IDEF(K)
          IF (IDEF(K) > 0) IUSED(IDEF(K))=1
          IF (IDEF(K) > NP) NP=IDEF(K)
          K=K+1
        END DO
      ELSE                  ! Set up table for PK parameters
        ISIDEF=.FALSE.      ! Format is that of new IDEF
! Fix up location of old "CALL PK" flag unless PK changed it not if model has 1 
! CMT: IDEF(2,1) coincides with IDEF(8)
        IF (NC /= 2) THEN
          IF (IDEF(NC+3*K+3) == -1) IDEF(NC+3*K+3)=0
        END IF
        CALLPK=IDEF(1+7)   ! Get call PK flag
! Does PK initialize A0? IDEF(1,3)=0 if the answer is no.
        IF (IDEF(1+7*2) /= 0 .AND. IDEF(1+7*2) /= 1 .AND. IDEF(1+7*2) /= -1) THEN
          WRITE (LOGUNT,4990,ERR=502) 1,3,IDEF(1+7*2)
          WRITE (LOGUNT,4880,ERR=502)       
          ISTOP=1
        ELSE
          IPKA0=IDEF(1+7*2)
        END IF
! Does PK explicitly to A? IDEF(1,4)=0 if the answer is no.
        IF (IDEF(1+7*3) /= 0 .AND. IDEF(1+7*3) /= 1 .AND. IDEF(1+7*3) /= -1) THEN
          WRITE (LOGUNT,4990,ERR=502) 1,4,IDEF(1+7*3)
          WRITE (LOGUNT,4882,ERR=502)          
          ISTOP=1
        ELSE
          IPKA=IDEF(1+7*3)
        END IF
        DO I=5,PC ! THETAO of row 1 should be zero
          IF (IDEF(1+7*(I-1)) == 0) CYCLE
          WRITE (LOGUNT,4990,ERR=502) 1,I,IDEF(1+7*(I-1))       
          WRITE (LOGUNT,4991,ERR=502)
          ISTOP=1
        END DO
        IUSED=ZERO
!        IUSED(1:PG)=0
        IF (IDEF(2) > PG .OR. IDEF(2) < 0) THEN ! Get output fraction
          WRITE (LOGUNT,4990,ERR=502) 2,1,IDEF(2)
          WRITE (LOGUNT,4992,ERR=502) 'OUTPUT FRACTION'
          ISTOP=1
        ELSE
          IFR=IDEF(2)
          IF (IFR > 0) IUSED(IFR)=1
          IF (IFR > NP) NP=IFR
        END IF
        IF (IDEF(2+7) > PG .OR. IDEF(2+7) < 0) THEN ! Get time scale
          WRITE (LOGUNT,4990,ERR=502) 2,2,IDEF(2+7)
          WRITE (LOGUNT,4992,ERR=502) 'TIME SCALE'
          ISTOP=1
        ELSE
          ITSC=IDEF(2+7)
          IF (ITSC > 0) IUSED(ITSC)=1
          IF (ITSC > NP) NP=ITSC
        END IF
!        
        K=4 ! Get start,end of model time parameters
        MTNO=0
!        
        IF (IDEF(2+7*2) /= 0) THEN
          K=5
          IF (IDEF(2+7*2) > PG .OR. IDEF(2+7*2) < 0) THEN
            WRITE (LOGUNT,4990,ERR=502) 2,3,IDEF(2+7*2)
            WRITE (LOGUNT,4992,ERR=502) 'FIRST MODEL TIME PARAMETER'
            ISTOP=1
          ELSE
            IMTBEG=IDEF(2+7*2)
          END IF  
          IF (IDEF(2+7*3) > PG .OR. IDEF(2+7*3) < 0) THEN
            WRITE (LOGUNT,4990,ERR=502) 2,4,IDEF(2+7*3)
            WRITE (LOGUNT,4992,ERR=502) 'LAST MODEL TIME PARAMETER'
            ISTOP=1
          ELSE
            IMTEND=IDEF(2+7*3)
            IF (IMTEND > NP) NP=IMTEND
            MTNO=IMTEND-IMTBEG+1
          END IF
          IF (IMTBEG > 0 .AND. IMTEND == 0) THEN
            WRITE (LOGUNT,4997,ERR=502)
            ISTOP=1
          END IF
          IF (IMTBEG == 0 .AND. IMTEND > 0) THEN
            WRITE (LOGUNT,4997,ERR=502)
            ISTOP=1
          END IF
          IF (IMTBEG > IMTEND) THEN
            WRITE (LOGUNT,4998,ERR=502)
            ISTOP=1
          END IF
        END IF
        DO I=K,PC ! THETAO of row 2 should be zero
          IF (IDEF(2+7*(I-1)) /= 0) THEN
            WRITE (LOGUNT,4990,ERR=502) 2,I,IDEF(2+7*(I-1))
            WRITE (LOGUNT,4991,ERR=502)
            ISTOP=1
          END IF
        END DO
        DO I=1,NC ! Get scale and dose-related parameters
          IS(I)=0;  IBF(I)=0; ILAG(I)=0
          ID(I)=0;  IRR(I)=0
          DO J=3,7
            IF (IDEF(J+7*(I-1)) == 0) CYCLE
! Special case: No dosable compartments; this is CALLFL
! Changed from "K == 0" to "KCOMP == 0" 10/2007
            IF (IDEF(J+7*(I-1)) == -1 .AND. KCOMP == 0 .AND. NC+3 == J) CYCLE
            IF (IDEF(J+7*(I-1)) > PG .OR. IDEF(J+7*(I-1)) < 0) THEN
              WRITE (LOGUNT,4990,ERR=502) J,I,IDEF(J+7*(I-1))
              WRITE (LOGUNT,4994,ERR=502) I,LETT(J-2)
              ISTOP=1; CYCLE
            END IF
            IF (J > 3 .AND. IINST(I) == 0) THEN
              WRITE (LOGUNT,4990,ERR=502) J,I,IDEF(J+7*(I-1))
              WRITE (LOGUNT,4993,ERR=502) I,LETT(J-2)
              ISTOP=1; CYCLE
            END IF
            IUSED(IDEF(J+7*(I-1)))=1
            IF (IDEF(J+7*(I-1)) > NP) NP=IDEF(J+7*(I-1))
            IF (J == 3) THEN
              IS(I)=IDEF(J+7*(I-1))
            ELSE IF (J == 4) THEN
              IBF(I)=IDEF(J+7*(I-1))
            ELSE IF (J == 5) THEN
              IRR(I)=IDEF(J+7*(I-1))
            ELSE IF (J == 6) THEN
              ID(I)=IDEF(J+7*(I-1))
            ELSE
              ILAG(I)=IDEF(J+7*(I-1))
            END IF
          END DO
        END DO
        DO I=NC+1,PC ! THETAO of IDEF should be zero
          DO J=3,7
            IF (IDEF(J+7*(I-1)) == 0) CYCLE
            WRITE (LOGUNT,4990,ERR=502) J,I,IDEF(J+7*(I-1))
            WRITE (LOGUNT,4995,ERR=502) I         
            ISTOP=1
          END DO
        END DO
      END IF
!      
      ST1FL=0 ! Set up table for PK parameters
! 
      IF (ISTOP /= 0 .OR. PF) THEN 
        IF (ISTOP == 1) WRITE (LOGUNT,'(A)',ERR=502) ' '
        WRITE (LOGUNT,1440,ERR=502)
        DO I=1,NC
          TLIST(2:5)=DASH
          TLIST(1)=TDIGIT(IS(I)+1)
          IF (IS(I) == 0) ST1FL=1
          IF (IINST(I) /= 0) THEN
            TLIST(2)=TDIGIT(IBF(I)+1)
            IF (IBF(I) == 0) ST1FL=1
            TLIST(3)=TDIGIT(IRR(I)+1)
            IF (IRR(I) == 0) ST1FL=1
            TLIST(4)=TDIGIT(ID(I)+1)
            IF (ID(I) == 0) ST1FL=1
            TLIST(5)=TDIGIT(ILAG(I)+1)
            IF (ILAG(I) == 0) ST1FL=1
          END IF
          WRITE (LOGUNT,1451,ERR=502) I,(TLIST(J),J=1,5)
        END DO
        WRITE (LOGUNT,1452,ERR=502)
        IF (ST1FL == 1) WRITE (LOGUNT,1453,ERR=502)
        IF (IFR > 0) WRITE (LOGUNT,1456,ERR=502) IFR
        IF (ITSC > 0) WRITE (LOGUNT,1460,ERR=502) ITSC
        IF (MTNO > 0) WRITE (LOGUNT,1457,ERR=502) IMTBEG,IMTEND
      END IF    
! Check for basic PK parameters; IUSED was not set in superprob when .NOT.PF 
      IF (MTNO > 0) THEN
! Assume all are used because gaps are ok. (NMTRAN warned about them).
        IF (IMTBEG <= IMTEND) IUSED(IMTBEG:IMTEND)=1
      END IF
!
      DO I=1,NBP
        IF (IUSED(I) == 0) CYCLE
        WRITE (LOGUNT,1402,ERR=502) I
        ISTOP=1
      END DO
!
      NBP1=NBP+1    ! Check for unused PK params
!      
      DO I=NBP1,NP
        IF (IUSED(I) == 1) CYCLE
        WRITE (LOGUNT,1410,ERR=502) I       
        ISTOP=1
      END DO
!
      IF (ISTOP == 1 .AND. ISIDEF) WRITE (LOGUNT,1430,ERR=502) (IDEF(I),I=1,K)
!
      DO I=1,NC     ! Set up dummy PK parameter for any S,F,FZ,T,FR Which is identically 1
        IF (IS(I) == 0) IS(I)=IRGG
        IF (IINST(I) == 1 .AND. IBF(I) == 0) IBF(I)=IRGG
      END DO
!      
      IF (ITSC == 0) ITSC=IRGG
      IF (IFR == 0)  IFR=IRGG
!      G3(IRGG,1,1)=ONE ! Now initialize the dummy parameter
!      G3(IRGG,2:NEP1,1)=ZERO
      G3(IRGG,1:NEP1,1:NEP1)=ZERO   ! AJB 02FEB10 
      G3(IRGG,1,1)=ONE ! Now initialize the dummy parameter
      MAXKF=0          ! Count number of PK params which are modelled as LOGS and SAVE indices
!      
      DO I=1,NP
        IF (G3(I,1,1) == ZERO) CYCLE
        MAXKF=MAXKF+1
        IFORM(MAXKF)=I
      END DO
! 
      IF (PF) THEN
        IF (MAXKF > 0)  WRITE (LOGUNT,1660,ERR=502) (IFORM(I),I=1,MAXKF)        
        IF (YFORM == 1) WRITE (LOGUNT,1010,ERR=502)
      END IF
!
      FORALL (I=1:MAXIC) IPOOL(I)=I ! Initialize infusion counters
      IHEAD=0; IP=0
!
      JDEF=PD+1     ! SET UP INDXS values - 0 is default
      JDUM=JDEF     ! Pass JDEF to PRED in case of no time data item 5/93
      EVTREC(1:5,JDEF)=0.0
      JEVENT=INDXS(1)
      JTIME=INDXS(2)
      IF (JTIME == 0) JTIME=JDEF
      JAMT=INDXS(3)
      IF (JAMT == 0) JAMT=JDEF
      JRATE=INDXS(4)
      IF (JRATE == 0) JRATE=JDEF
      JSS=INDXS(5)
      IF (JSS == 0) JSS=JDEF
      JDELTA=INDXS(6)
      IF (JDELTA == 0) JDELTA=JDEF
      JCOMPT=INDXS(7)
      IF (JCOMPT == 0) JCOMPT=JDEF
      JCOMPF=INDXS(8)
      IF (JCOMPF == 0) JCOMPF=JDEF
      JERROR=INDXS(9)
      IF (JERROR == 0) JERROR=JDEF
      JCONT=INDXS(10)
      IF (JCONT == 0) JCONT=JDEF
      JMORE=INDXS(11)
      IF (JMORE == 0) JMORE=JDEF
!      
      IF (PF) THEN
        WRITE (LOGUNT,666,ERR=502)
        WRITE (LOGUNT,60,ERR=502) JEVENT
        IF (JTIME  /= JDEF) WRITE (LOGUNT,61,ERR=502) JTIME
        IF (JAMT   /= JDEF) WRITE (LOGUNT,62,ERR=502) JAMT
        IF (JRATE  /= JDEF) WRITE (LOGUNT,63,ERR=502) JRATE
        IF (JSS    /= JDEF) WRITE (LOGUNT,64,ERR=502) JSS
        IF (JDELTA /= JDEF) WRITE (LOGUNT,65,ERR=502) JDELTA
        IF (JMORE  /= JDEF) WRITE (LOGUNT,70,ERR=502) JMORE
        IF (JCOMPT /= JDEF) WRITE (LOGUNT,66,ERR=502) JCOMPT
        IF (JCOMPF /= JDEF) WRITE (LOGUNT,67,ERR=502) JCOMPF
        IF (JERROR /= JDEF) WRITE (LOGUNT,68,ERR=502) JERROR
        IF (JCONT  /= JDEF) WRITE (LOGUNT,69,ERR=502) JCONT      
        WRITE (LOGUNT,667,ERR=502)
      END IF
!      
      TIMDEF=JTIME /= JDEF
!
      IF (JTIME == JDEF) THEN
        IF (ADVID /= 9) THEN
          WRITE (LOGUNT,'(A)',ERR=502) ' TIME DATA ITEM IS REQUIRED WITH THIS ADVAN.'
          ISTOPT=1; GO TO 5000
        END IF
        IF (XLNCM1 /= 0) THEN
          WRITE (LOGUNT,'(A)',ERR=502) ' TIME DATA ITEM IS REQUIRED WITH NON-EQUILIBRIUM COMPARTMENTS.'
          ISTOPT=1; GO TO 5000
        END IF
      END IF
!
      CALL9=.FALSE. ! Initialize call variables
      CALLE=.FALSE.
      CALLP=.FALSE.
!      
      IF (CALLPK < -2 .OR. CALLPK > 1) THEN
        ISTOP=1
        WRITE (LOGUNT,1488,ERR=502) CALLPK
      ELSE
        CALLID=0
        IF (CALLPK == -2) THEN
          CALLID=1
          CALLPK=-1
        END IF
        CALLPK=CALLPK+2
        IF (PF) THEN
          IF (CALLPK == 1) WRITE (LOGUNT,1489,ERR=502)
          IF (CALLPK == 2) WRITE (LOGUNT,1490,ERR=502)
          IF (CALLPK == 3) WRITE (LOGUNT,1492,ERR=502)
          IF (CALLID == 0) THEN ! Write messages to the output file
            WRITE (LOGUNT,'(A)',ERR=502) ' PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.'
          ELSE IF (MTNO == 0) THEN
            WRITE (LOGUNT,'(A)',ERR=502) ' PK SUBROUTINE CALLED AT NONEVENT (ADDITIONAL AND LAGGED) DOSE TIMES.'
          ELSE IF (OLDFMT) THEN
            WRITE (LOGUNT,'(A)',ERR=502) ' PK SUBROUTINE CALLED AT NONEVENT (ADDITIONAL AND LAGGED) DOSE TIMES &
                                           &AND AT MODEL TIMES.'
          ELSE
            WRITE (LOGUNT,'(A/A)',ERR=502) ' PK SUBROUTINE CALLED AT NONEVENT (ADDITIONAL AND LAGGED) DOSE TIMES &
                                             &AND AT MODEL TIMES.'
          END IF
        END IF
      END IF
!      
      IF (IPKA0 == 1) THEN
        IF (PF) WRITE (LOGUNT,'(A,A)',ERR=502)'0PK SUBROUTINE INDICATES THAT COMPARTMENT AMOUNTS ARE INITIALIZED.'
      END IF
!      
      IF (IPKA0 == -1) THEN
        IF (PF) WRITE (LOGUNT,'(A,A/A,A)',ERR=502)'0BY DEFAULT, PK SUBROUTINE MAY INITIALIZE  &
                                                   &COMPARTMENT AMOUNTS. (IF NOT INITIALIZED, &
                                                   &PK SHOULD SET IDEF(1,3)=0 TO AVOID        &
                                                   &UNNECESSARY RUN TIME.)'
        IPKA0=1
      END IF
!
      IF (IPKA == 1) THEN
        IF (PF) WRITE (LOGUNT,'(A,A)',ERR=502)'0PK SUBROUTINE INDICATES THAT DERIVATIVES OF COMPARTMENT AMOUNTS ARE USED.'
      END IF
! 
      IF (IPKA == -1) THEN
        IF (PF) WRITE (LOGUNT,'(A,A/A,A)',ERR=502)'0BY DEFAULT, PK SUBROUTINE MAY USE DERIVATIVES &
                                                   &OF COMPARTMENT AMOUNTS. (IF NOT USED, PK      &
                                                   &SHOULD SET IDEF(1,4)=0 TO AVOID UNNECESSARY RUN TIME.)'
        IPKA=1
      END IF
!     
      IF (I_SS /= -1) WRITE (LOGUNT,3485,ERR=502)      ! Added 10/2007
! 
      IF (PF) THEN  ! Start ERROR subroutine descriptive material
        IF (CALLER == 1) WRITE (LOGUNT,1014,ERR=502)
        SIMTXT='0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH'//' EVERY EVENT RECORD.'
        IF (CALLER == 2) WRITE (LOGUNT,1013,ERR=502) SIMTXT
        IF (CALLER == 3) WRITE (LOGUNT,1011,ERR=502) SIMTXT
        IF (CALLER == 4) WRITE (LOGUNT,1012,ERR=502) SIMTXT
      END IF
!      
      IF (CALLER >= 4) THEN
        DO I=1,NPEPS
          IF (HH(I,1) /= ZERO) GO TO 5000
        END DO
        WRITE (LOGUNT,4012,ERR=502)  
        ISTOP=ISTOP+2
      END IF
!  
 5000 IF (IERRA == 1) THEN
        IF (PF) WRITE (LOGUNT,'(A,A)',ERR=502)'0ERROR SUBROUTINE INDICATES THAT DERIVATIVES OF COMPARTMENT AMOUNTS ARE USED.'
      END IF
!      
      IF (IERRA == -1) THEN
        IF (PF) WRITE (LOGUNT,'(A,A/A,A)',ERR=502)'0BY DEFAULT, ERROR SUBROUTINE MAY USE        &
                                                   &DERIVATIVES OF COMPARTMENT AMOUNTS. (IF NOT &
                                                   &USED, ERROR SHOULD SET IDEF(3)=0 TO AVOID   &
                                                   &UNNECESSARY RUN TIME.)'
        IERRA=1
      END IF
!
      IF (ISTOP > 0 .OR. ISTOPT > 0) THEN ! Stop if any errors so far
        IF (ISTOP == 1)  WRITE (LOGUNT,8096,ERR=502)
        IF (ISTOP == 2)  WRITE (LOGUNT,8097,ERR=502)
        IF (ISTOP == 3)  WRITE (LOGUNT,8094,ERR=502)
        IF (ISTOPT == 1) WRITE (LOGUNT,8095,ERR=502)
        IQUIT=1; GO TO 999
      END IF
!
      IF (.NOT. NOETAS) THEN ! Initialize arrays
        DTINT(1:NPETAS)=ZERO
        ADTE(1:NPETAS)=ZERO
        D2TINT(1:NPETAS,1:NPETAS)=ZERO
        D2ADTE(1:NPETAS,1:NPETAS)=ZERO   
      END IF
! Added 4/94. Initialization at ICALL=1: call DES allows library routines to read new 
! instructions if the DES routine does not reset IDEF(1), assume max. no. of THETAs
                   
      IDEFA(1)=-9; KDES=0
      IDEFA(2)=-1; KAES=0
      IDEFD(1)=-9; CPYTHE=0 
      IDEFD(2)=1;  TRUNTH=NTHETA
!      
      IF (ISETNTH == 9999) TRUNTH=0
!
      IF (ADVID == 6 .OR. ADVID == 8 .OR. ADVID == 9 .OR. ADVID == 13) THEN       ! ajb 3/2009      
        CALL SADVAN(ICALL,0)  !7.2b52b
        IF (IERPRD > 0 .OR. IQUIT == 1) GO TO 999
      ELSE
        IF (SSID == 6) THEN
          CALL SSS
          IF (IERPRD > 0 .OR. IQUIT == 1) GO TO 999
        ELSE
          GO TO 2000
        END IF
      END IF
!
      CALLA9=0 ! DES was called. Maybe AES too. Report on AES first if there exist equilibrium compts.
!
      IF (ADVID == 9 .AND. LNCM2 /= 0) THEN
! New library or generated subroutine sets IDEFA(2) = -1 (every event) or IDEFA(2)=1 (once per IR)
        CALLA9=IDEFA(2)
        IF ((IDEFA(1) < 0 .AND. IDEFA(1) /= -9) .OR. IDEFA(1) > TRUNTH) THEN
          WRITE (LOGUNT,3481,ERR=502) 'AES','IDEFA(1)',IDEFA(1),'AES'
          IQUIT=1; GO TO 999
        END IF
        CPYTHE=IDEFA(1)
        IF (IDEFA(1) == -9) CPYTHE=TRUNTH
        IF (JTIME == JDEF) THEN   ! If no time data item ...
          IF (CALLA9 /= -1 .AND. CALLA9 /= 1) THEN
            WRITE (LOGUNT,3481,ERR=502) 'AES','IDEFA(2)',IDEFA(2),'AES'           
            IQUIT=1; GO TO 999
          END IF
          IF (PF) THEN
            IF (CALLA9 == -1) WRITE (LOGUNT,3482,ERR=502)
            IF (CALLA9 == 1)  WRITE (LOGUNT,3483,ERR=502)    
          END IF
        END IF                    ! End not time data item ...
        IF (LNCM1 == 0) GO TO 2000
      END IF
! End of ADVAN9 with equilibrium compts.
      IF ((IDEFD(1) < 0 .AND. IDEFD(1) /= -9) .OR. IDEFD(1) > TRUNTH) THEN   
        WRITE (LOGUNT,3481,ERR=502) 'DES','IDEFD(1)',IDEFD(1),'DES'
        IQUIT=1; GO TO 999
      END IF
!
      IF (CPYTHE /= TRUNTH) THEN  ! If AES set CPYTHE to NTHETA, leave it as is
        IF (IDEFD(1) == -9) THEN
          CPYTHE=TRUNTH
        ELSE
          CPYTHE=MAX(IDEFA(1),IDEFD(1))
        END IF
      END IF
! New library or generated subroutine sets IDEFD(2) = 0 (compressed) or IDEFD(2)=1 (full)
! or IDEFD(2)=2 (library subr; retain old IDEFD(2)=0 info) old one leaves it 1, 
! which defaults to full.
      IF (IDEFD(2) == 2) THEN
        COMPAC=.TRUE.
        IF (PF) WRITE (LOGUNT,6002,ERR=502)
        GO TO 2000
      END IF
!      
      IF (IDEFD(2) == 1) THEN     ! Reinitialize full arrays in case the DES code changed
        COMPAC=.FALSE.
        DET(1:MCOMP)=ZERO
        DA(1:MCOMP,1:MCOMP)=ZERO
        DP(1:MCOMP,1:PG)=ZERO
        IF (PF) WRITE (LOGUNT,6001,ERR=502)
        GO TO 2000
      END IF
!
      COMPAC=.TRUE.               ! Compact storage in DES
      MM=MAX(100,MCOMP+PG+10)
      MMSQ=MM*MM
!      
      NIAI=0; MAXI=0; NIAI1=0
      MAXJ=0; MAXK=0
!      
      DO I=1,PIR
        IF (DAC(I,1) == 0) EXIT
        IAI(I)=INT(DAC(I,1))/MMSQ
        MAXI=MAX(MAXI,IAI(I))
        IAJ(I)=(INT(DAC(I,1))-MMSQ*IAI(I))/MM
        MAXJ=MAX(MAXJ,IAJ(I))
        IAK(I)=DAC(I,1)-MM*IAJ(I)-MMSQ*IAI(I)
        MAXK=MAX(MAXK,IAK(I))
        IF (IAK(I) == 0) NIAI1=NIAI1+1
        NIAI=NIAI+1
      END DO
!  
      M=0
! 
      DO I=1,MAXI
        DO J=0,MAXJ
          DO NEVENT=1,NIAI
            IF (IAK(NEVENT) == 0 .AND. IAI(NEVENT) == I .AND. IAJ(NEVENT) == J) THEN
              M=M+1
              AA(M)=NEVENT
              DAC(M,1)=IAI(NEVENT)*MMSQ+IAJ(NEVENT)*MM+IAK(NEVENT)
            END IF
          END DO
        END DO
      END DO
! 
      DO I=1,MAXI
        DO J=0,MAXJ
          DO K=1,MAXK
            DO NEVENT=1,NIAI
              IF (IAK(NEVENT) == K .AND. IAI(NEVENT) == I .AND. IAJ(NEVENT) == J) THEN
                M=M+1
                AA(M)=NEVENT
                DAC(M,1)=IAI(NEVENT)*MMSQ+IAJ(NEVENT)*MM+IAK(NEVENT)
              END IF
            END DO
          END DO
        END DO
      END DO
!      
      FORALL (I=1:NIAI)
        IAI(AA(I))=INT(DAC(AA(I),1))/MMSQ
        IAJ(AA(I))=(INT(DAC(AA(I),1))-MMSQ*IAI(AA(I)))/MM
        IAK(AA(I))=DAC(AA(I),1)-MM*IAJ(AA(I))-MMSQ*IAI(AA(I))
        DAC(AA(I),1)=0.0
      END FORALL
!      
      NIPI=0; MAXI=0; NIPI1=0
      MAXJ=0; MAXK=0
!      
      DO I=1,PIR
        IF (DPC(I,1) == 0) EXIT
        IPI(I)=INT(DPC(I,1))/MMSQ
        MAXI=MAX(MAXI,IPI(I))
        IPJ(I)=(INT(DPC(I,1))-MMSQ*IPI(I))/MM
        MAXJ=MAX(MAXJ,IPJ(I))
        IPK(I)=DPC(I,1)-MM*IPJ(I)-MMSQ*IPI(I)
        MAXK=MAX(MAXK,IPK(I))
        IF (IPK(I) == 0) NIPI1=NIPI1+1
        NIPI=NIPI+1
      END DO
! 
      M=0
! 
      DO I=1,MAXI
        DO J=0,MAXJ
          DO NEVENT=1,NIPI
            IF (IPK(NEVENT) == 0 .AND. IPI(NEVENT) == I .AND. IPJ(NEVENT) == J) THEN
              M=M+1
              PP(M)=NEVENT
              DPC(M,1)=IPI(NEVENT)*MMSQ+IPJ(NEVENT)*MM+IPK(NEVENT)
            END IF
          END DO
        END DO
      END DO
! 
      DO I=1,MAXI
        DO J=0,MAXJ
          DO K=1,MAXK
            DO NEVENT=1,NIPI
              IF (IPK(NEVENT) == K .AND. IPI(NEVENT) == I .AND. IPJ(NEVENT) == J) THEN
                M=M+1
                PP(M)=NEVENT
                DPC(M,1)=IPI(NEVENT)*MMSQ+IPJ(NEVENT)*MM+IPK(NEVENT)
              END IF
            END DO
          END DO
        END DO
      END DO
!   
      FORALL (I=1:NIPI)
        IPI(PP(I))=INT(DPC(PP(I),1))/MMSQ
        IPJ(PP(I))=(INT(DPC(PP(I),1))-MMSQ*IPI(PP(I)))/MM
        IPK(PP(I))=DPC(PP(I),1)-MM*IPJ(PP(I))-MMSQ*IPI(PP(I))
        DPC(PP(I),1)=0.0
      END FORALL 
!      
      NIT=0; NIT1=0; MAXI=0; MAXK=0
!      
      DO I=1,PIR
        IF (DTC(I) == 0) EXIT
        ITI(I)=INT(DTC(I))/MMSQ
        MAXI=MAX(MAXI,ITI(I))
        ITK(I)=DTC(I)-MMSQ*ITI(I)
        MAXK=MAX(MAXK,ITK(I))
        IF (ITK(I) == 0) NIT1=NIT1+1
        NIT=NIT+1
      END DO
! 
      M=0
! 
      DO I=1,MAXI
        DO NEVENT=1,NIT
          IF (ITK(NEVENT) == 0 .AND. ITI(NEVENT) == I) THEN
            M=M+1
            TT(M)=NEVENT
            DTC(M)=ITI(NEVENT)*MMSQ+ITK(NEVENT)
          END IF
        END DO
      END DO
!     
      DO I=1,MAXI
        DO K=1,MAXK
          DO NEVENT=1,NIT
            IF (ITK(NEVENT) == K .AND. ITI(NEVENT) == I) THEN
              M=M+1
              TT(M)=NEVENT
              DTC(M)=ITI(NEVENT)*MMSQ+ITK(NEVENT)
            END IF
          END DO
        END DO
      END DO
!      
      FORALL (I=1:NIT)
        ITI(TT(I))=INT(DTC(TT(I)))/MMSQ
        ITK(TT(I))=DTC(TT(I))-MMSQ*ITI(TT(I))
        DTC(TT(I))=0.0
      END FORALL
!
      XNIAI=NIAI ! Save original for on/off
      XNIAI1=NIAI1
      XNIPI=NIPI
      XNIPI1=NIPI1
      XNIT=NIT
      XNIT1=NIT1
!    
      XAA(1:NIAI)=AA(1:NIAI)
      XIAK(1:NIAI)=IAK(1:NIAI)
      XIAI(1:NIAI)=IAI(1:NIAI)
      XIAJ(1:NIAI)=IAJ(1:NIAI)
!        
      XPP(1:NIPI)=PP(1:NIPI)
      XIPK(1:NIPI)=IPK(1:NIPI)
      XIPI(1:NIPI)=IPI(1:NIPI)
      XIPJ(1:NIPI)=IPJ(1:NIPI)
!      
      XTT(1:NIT)=TT(1:NIT)
      XITK(1:NIT)=ITK(1:NIT)
      XITI(1:NIT)=ITI(1:NIT)
!      
      IF (PF) WRITE (LOGUNT,6003,ERR=502)
! 
! Additional housekeeping
! Allow library subroutines to re-position and re-read FLIB
!
 2000 IF (NOPROB /= -1 .AND. NOPROB < IPROB) GO TO 1000
!
! Call routine to check data records for consistency also checks array sizes in subroutines
! This is in check because PREDI is getting too big.
!
      CALL CHECK(PASSRC)
      IF (IQUIT == 1) GO TO 999
!    
   30 FORMAT ('1DOUBLE PRECISION PREDPP VERSION ',A,/' ')
   60 FORMAT ('   EVENT ID DATA ITEM IS DATA ITEM NO.:    ',I3)
   61 FORMAT ('   TIME DATA ITEM IS DATA ITEM NO.:        ',I3)
   62 FORMAT ('   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.: ',I3)
   63 FORMAT ('   DOSE RATE DATA ITEM IS DATA ITEM NO.:   ',I3)
   64 FORMAT ('   STEADY STATE DATA ITEM IS DATA ITEM NO.:',I3)
   65 FORMAT ('   INTERVAL DATA ITEM IS DATA ITEM NO.:    ',I3)
   66 FORMAT ('   COMPT. NO. DATA ITEM IS DATA ITEM NO.:  ',I3)
   67 FORMAT ('   PRED. COMPT. DATA ITEM IS DATA ITEM NO.:',I3)
   68 FORMAT ('   CALL DATA ITEM IS DATA ITEM NO.:        ',I3)
   69 FORMAT ('   CONTINUATION DATA ITEM IS DATA ITEM NO.:',I3)
   70 FORMAT ('   ADDL. DOSES DATA ITEM IS DATA ITEM NO.: ',I3)
  125 FORMAT ('0MODEL SUBROUTINE USER-SUPPLIED - ID NO.',I5/&
              '0MAXIMUM NO. OF BASIC PK PARAMETERS:',I4)
  219 FORMAT (2X,I3,9X,A8,5X,7(A3,8X))
  220 FORMAT (2X,I3,9X,2A4,5X,7(A3,8X))
  237 FORMAT ('0RUN TERMINATED BECAUSE OF ERRORS IN FILE SIZES.')
  238 FORMAT ('0',A6,'=',I4,'. THE MAXIMUM VALUE OF THIS CONSTANT IS ', I4,'.')
  240 FORMAT ('0TRANS SUBROUTINE USER-SUPPLIED - ID NO.',I5)
  241 FORMAT (' INFN SUBROUTINE USER-SUPPLIED - ID NO.',I5,/' ')
  666 FORMAT ('0DATA ITEM INDICES USED BY PRED ARE:')
  667 FORMAT (' ')
  993 FORMAT ('(',I3,'E15.7)')
  997 FORMAT ('0NRD VALUE(S) FROM SUBROUTINE TOL:',9I4)
  998 FORMAT ('0NRD VALUE FROM SUBROUTINE TOL:',I4)
 1010 FORMAT ('0ERROR IN LOG Y IS MODELED')
 1011 FORMAT (A/' OTHERWISE, ERROR SUBROUTINE CALLED ONCE PER INDIVIDUAL RECORD.')
 1012 FORMAT (A/' OTHERWISE, ERROR SUBROUTINE CALLED ONCE IN THIS PROBLEM.')
 1013 FORMAT (A/' OTHERWISE, ERROR SUBROUTINE CALLED ONLY WITH OBSERVATION EVENTS.')
 1014 FORMAT ('0ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.')
 1200 FORMAT ('1')
 1210 FORMAT ('ROW IN GG OF A PK PARAMETER IS GREATER THAN "PG"')
 1402 FORMAT ('0ERROR IN PK: BASIC PK PARAMETER',I3,' MAY NOT BE USED')
 1410 FORMAT ('0ERROR IN PK: PK PARAMETER NOT USED:',I3)
 1430 FORMAT ('0INCORRECT IDEF VECTOR:',20I3)
 1440 FORMAT (' ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG'&
             /' COMPT. NO.                             INDICES',&
             /'              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER ',&
              ' ABSORB', /'                         FRACTION    RATE        DURATION    LAG  ')
 1451 FORMAT (2X,I3,5X,5(5X,A3,4X))
 1452 FORMAT ('             - PARAMETER IS NOT ALLOWED FOR THIS MODEL')
 1453 FORMAT ('             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;', /&
              '               WILL DEFAULT TO ONE IF APPLICABLE')
 1456 FORMAT ('0OUTPUT FRACTION PARAMETER ASSIGNED TO ROW NO.:',I3)
 1457 FORMAT ('0FIRST MODEL TIME PARAMETER ASSIGNED TO ROW NO.:',I3, /' LAST  MODEL &
               &TIME PARAMETER ASSIGNED TO ROW NO.:',I3)
 1460 FORMAT ('0TIME SCALE PARAMETER ASSIGNED TO ROW NO.:',I3)
 1488 FORMAT ('0PK SUBROUTINE HAS RETURNED AN INAPPROPRIATE VALUE IN IDEF FOR'/&
               ' CALLS TO PK SUBROUTINE:',I3)
 1489 FORMAT ('0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.')
 1490 FORMAT ('0PK SUBROUTINE CALLED ONLY WITH NEW INDIVIDUAL OR NEW TIME.')
 1492 FORMAT ('0PK SUBROUTINE CALLED ONCE PER INDIVIDUAL RECORD.')
 1660 FORMAT ('0PK PARAMETERS WHICH ARE MODELLED AS LOG: ',30I3)
 2100 FORMAT ('0COMPARTMENT ATTRIBUTES '/' COMPT. NO.   FUNCTION   INITIAL    &
               &ON/OFF      DOSE      DEFAULT    DEFAULT'/'                    &
               &     STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.')
 2101 FORMAT ('0COMPARTMENT ATTRIBUTES '/' COMPT. NO.   FUNCTION   INITIAL    &
               &ON/OFF      DOSE      DEFAULT    DEFAULT    EQUILIB    &
               &EXCLUDE'/ '                         STATUS     ALLOWED    &
               &ALLOWED    FOR DOSE   FOR OBS.    RIUM      FROM TOTAL')
 2102 FORMAT ('0COMPT. NO.   FUNCTION   EQUILIB    EXCLUDE'/&
               '                          RIUM      FROM TOTAL')
 2301 FORMAT ('0ROUTINE SS IS INCOMPATIBLE WITH ROUTINE ADVAN')    
 2501 FORMAT ('0GENERAL STEADY STATE (SS) ROUTINE IS BEING USED')
 2502 FORMAT ('0LINEAR KINETICS STEADY STATE (SS5) ROUTINE IS BEING USED')
 2503 FORMAT ('0LINEAR KINETICS STEADY STATE (SS7) ROUTINE IS BEING USED')       
 3481 FORMAT ('0',A3,' SUBROUTINE HAS RETURNED AN INAPPROPRIATE VALUE FOR ',A,I3/&
              '0RUN TERMINATED BECAUSE OF ERROR IN ',A3,' SUBROUTINE')
 3482 FORMAT ('0ADVAN9 CALLED WITH EVERY EVENT RECORD.')
 3483 FORMAT ('0ADVAN9 CALLED ONCE PER INDIVIDUAL RECORD.')
 3484 FORMAT ('0INITIAL STEADY STATE (I_SS) SET BY MODEL TO ',I3)
 3485 FORMAT ('0INITIAL STEADY STATE (I_SS) SET BY PK SUBROUTINE.')
 4010 FORMAT ('0ERROR SUBROUTINE HAS RETURNED AN INAPPROPRIATE VALUE FOR ',A,I3)
 4012 FORMAT ('0ERROR IN ERROR SUBROUTINE: NO VALUE RETURNED FOR HH')
 4880 FORMAT (' COMPARTMENT INITIALIZATION FLAG MUST BE 0 OR 1')
 4882 FORMAT (' COMPARTMENT USE FLAG MUST BE 0 OR 1')
 4990 FORMAT ('0ERROR IN PK: INAPPROPRIATE VALUE IN IDEF(', I3,',',I3,'): ',I6)
 4991 FORMAT (' THIS VALUE SHOULD ALWAYS BE ZERO.')     
 4992 FORMAT (' ROW IN GG MUST BE 1 THRU "PG" FOR ',A)    
 4993 FORMAT (' PK MAY NOT MODEL COMPARTMENT ',I3,'''S ',A/ ' BECAUSE DOSES TO THIS &
                &COMPARTMENT ARE NOT ALLOWED.')
 4994 FORMAT (' ROW IN GG MUST BE 1 THRU "PG" FOR COMPARTMENT ', I3,'''S ',A)
 4995 FORMAT (' COMPARTMENT',I3,' IS NOT DEFINED IN THE MODEL.')
 4997 FORMAT (' ERROR IN PK: BOTH FIRST AND LAST MODEL TIME PARAMETERS MUST BE ASSIGNED ROWS IN GG.')
 4998 FORMAT (' ERROR IN PK. ROW IN GG FOR LAST MODEL TIME PARAMETER IS LESS THAN'/&
              ' ROW IN GG FOR FIRST MODEL TIME PARAMETER.')
 6001 FORMAT ('0DES SUBROUTINE USES FULL STORAGE MODE.')
 6002 FORMAT ('0DES SUBROUTINE USES COMPACT STORAGE MODE (IDENTICAL TO PREVIOUS PROBLEM).')
 6003 FORMAT ('0DES SUBROUTINE USES COMPACT STORAGE MODE.')      
 8001 FORMAT ('0NUMBER OF USER-DEFINED COMPARTMENTS,',I4,', IS INAPPROPRIATE')    
 8002 FORMAT ('0NUMBER OF USER-DEFINED COMPARTMENTS,',I4,', MUST BE LESS THAN "PC"')
 8003 FORMAT ('0DEFAULT COMPARTMENT FOR DOSES,',I4,', IS INAPPROPRIATE')
 8004 FORMAT ('0DEFAULT COMPARTMENT FOR OBSERV.,',I4, ', IS INAPPROPRIATE')
 8005 FORMAT ('0MAXIMUM NO. OF BASIC PK PARAMETERS MAY NOT BE GREATER THAN "PG" (',I4,')')
 8006 FORMAT ('0NON-BASIC PK PARAMETER',I3,' MAY NOT BE USED IN LINK ARRAY')
 8007 FORMAT ('0RATE CONSTANT PARAMETERS - ASSIGNMENT OF ROWS IN GG' /10X,'  TO COMPT.'/2X,'FROM  ',999I5)
 8008 FORMAT (2X,'COMPT.')
 8009 FORMAT (2X,I3,3X,999(2X,A3))
 8010 FORMAT (' LINK FROM COMPARTMENT',I3,' TO ITSELF IS NOT POSSIBLE')
 8011 FORMAT ('             * LINK FROM A COMPARTMENT TO ITSELF IS NOT POSSIBLE')
 8012 FORMAT ('             - LINK BETWEEN THESE COMPARTMENTS IS NOT DEFINED FOR THIS MODEL')    
 8015 FORMAT ('0MAXIMUM NO. OF BASIC PK PARAMETERS IS INAPPROPRIATE')
 8096 FORMAT ('0RUN TERMINATED BECAUSE OF ERRORS IN PK SUBROUTINE')
 8097 FORMAT ('0RUN TERMINATED BECAUSE OF ERRORS IN ERROR SUBROUTINE')
 8098 FORMAT ('0RUN TERMINATED BECAUSE OF ERRORS IN SUBROUTINE COMBINATION')
 8099 FORMAT ('0RUN TERMINATED BECAUSE OF ERRORS IN MODEL SUBROUTINE')    
 8902 FORMAT ('0EQUILIBRIUM COMPARTMENTS MAY NOT BE ALLOWED TO RECEIVE DOSES')      
 8094 FORMAT ('0RUN TERMINATED BECAUSE OF ERRORS IN PK AND ERROR SUBROUTINES')
 8095 FORMAT ('0RUN TERMINATED BECAUSE TIME DATA ITEM IS MISSING')                   
!      
  999 RETURN
!     
  502 CALL ERRORMSGS(502,FILENAME='OUTPUT')
!
      END SUBROUTINE PREDI
!