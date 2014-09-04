;Sim_start : add to simulation model   
$SIZES NO=500 LIM6=500
;Sim_end

$PROBLEM Constant Hazard

$INPUT ID TIME DV DOSE ICL IV IKA 
       TYPE SMAX SMXH THR CAV CAVH CON   
       CNT=DROP CNT2=DROP CNT3=DROP 
       HC=DROP HC2=DROP HC3=DROP FE EVID
       ;ETA1 ETA2 ETA3 ETA4 

$DATA data.csv IGNORE=@  
;Sim_start : remove from simulation model   
;ACCEPT=(FE.EQ.1) ACCEPT=(TYPE.EQ.1)
;Sim_end

$SUBR ADVAN=6 TOL=9
$MODEL COMP=(HAZARD)

$PK

  CL  = ICL
  V   = IV
  KA  = IKA

  BASE = THETA(1)*EXP(ETA(1)) ;the ETA is a placeholder here

$DES
  CP  =DOSE*KA/V/(-KA+CL/V)*(EXP(-KA*T)-EXP(-CL/V*T))   
  DADT(1)=BASE    ;hazard   

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

$THETA  (0.00385814) ; BASE
$OMEGA  0  FIX

$SIMULATION (5988566) (39978 UNIFORM) NOPREDICTION SUB=100 ONLYSIM

$TABLE ID TIME SUR DOSE EVID NOPRINT ONEHEADER FILE=simtab51
