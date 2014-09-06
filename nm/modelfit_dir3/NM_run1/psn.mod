$SIZES      NO=500 LIM6=500
$PROBLEM    Repeated Time To Event data,Constant Hazard
$INPUT      ID TIME DV DOSE ICL IV IKA TYPE SMAX SMXH THR CAV CAVH CON
            CNT=DROP CNT2=DROP CNT3=DROP HC=DROP HC2=DROP HC3=DROP FE
            EVID
;ETA1 ETA2 ETA3 ETA4
$DATA       data.csv IGNORE=@
$SUBROUTINE ADVAN=6 TOL=9
$MODEL      COMP=(HAZARD)
$PK 
  
  CL  = ICL
  V   = IV
  KA  = IKA

  BASE= THETA(1)*EXP(ETA(1))

$DES 
  CP  =DOSE*KA/V/(-KA+CL/V)*(EXP(-KA*T)-EXP(-CL/V*T))   
  DADT(1)=BASE        ;hazard

$ERROR 
  CONC =DOSE*KA/V/(-KA+CL/V)*(EXP(-KA*TIME)-EXP(-CL/V*TIME)) 
;----------RTTE Model------------------------------
  IF(NEWIND.NE.2) OLDCHZ=0  ;reset the cumulative hazard
  CHZ = A(1)-OLDCHZ         ;cumulative hazard 
                            ;  from previous time point
                            ;  in data set
  OLDCHZ = A(1)             ;rename old cumulative hazard
  SUR = EXP(-CHZ)           ;survival probability

  HAZNOW=BASE               ; rate of event
                            ;  each time pt
                            ;  NB: update with each new model

  IF(DV.EQ.0)   Y=SUR         ;censored event (prob of survival)
  IF(DV.NE.0)   Y=SUR*HAZNOW  ;prob density function of event

  IF(ICALL.EQ.4) THEN ; for simulation
   CALL RANDOM (2,R)
   DV=0
   RTTE = 0
   IF(TIME.EQ.480) RTTE = 1 ; for the censored observation at 480 min
   IF(R.GT.SUR) THEN
      DV=1
      RTTE = 1
   ENDIF
  ENDIF

$THETA  (0,0.025) ; BASE
$OMEGA  0.09  ;   BSV-BASE
$SIMULATION (5988566) (39978 UNIFORM) ONLYSIM NOPREDICTION SUB=25
;$ESTIM MAXEVAL=9990 METHOD=1 LAPLACE LIKE PRINT=1 MSFO=msfb57 SIGL=9 NSIG=3

;$COV PRINT=E
$TABLE      ID TIME SUR DOSE EVID NOPRINT ONEHEADER FILE=simtab57

