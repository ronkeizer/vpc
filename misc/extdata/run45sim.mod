$PROBLEM As 45 but for simulation 
$INPUT ID TIME ODV DOSE ICL IV IKA 
       TYPE SMAX DV=SMXH THR CAV CAVH CON
       ;CNT CNT2 CNT3 HC HC2 HC3 
       ;ETA1 ETA2 ETA3 ETA4

$DATA data.csv IGNORE=@ ACCEPT=(THR.GT.0)
$MSFI msfb45 NPOPETAS=1

$PRED
  ;Baseline values
  B1   =THETA(1)
  B2   =THETA(2)
  B3   =THETA(3)

  ;Logits for Y>=1, Y>=2, Y>=3
  LGE1 =B1+ETA(1)
  LGE2 =B1+B2+ETA(1)
  LGE3 =B1+B2+B3+ETA(1)

  ;Probabilities for Y>=2, Y>=3
  PGE1   =EXP(LGE1)/(1+EXP(LGE1))
  PGE2   =EXP(LGE2)/(1+EXP(LGE2))
  PGE3   =EXP(LGE3)/(1+EXP(LGE3))

  ;Probabilities for Y=0, Y=1, Y=2, Y=3
  P0   =(1-PGE1) 
  P1   =(PGE1-PGE2)
  P2   =(PGE2-PGE3)
  P3   = PGE3   
 
  ;Select appropriate P(Y=m)
  IF(DV.EQ.0) Y=P0
  IF(DV.EQ.1) Y=P1
  IF(DV.EQ.2) Y=P2
  IF(DV.EQ.3) Y=P3

  ;Simulations
  IF (ICALL.EQ.4) THEN
   CALL RANDOM (2,R)
   DV=0
   IF(R.LE.PGE1) DV=1
   IF(R.LE.PGE2) DV=2
   IF(R.LE.PGE3) DV=3
  ENDIF


$SIM (12345) (678910 UNI) ONLYSIM NSUB=100

$TABLE ID TIME NOPRINT ONEHEADER FILE=simtab45
