      PROGRAM CONFINT

C     Program to calculate confidence intervals for abundance estimates,
C     assuming a lognormal distribution

C     Programmer:  S.T. Buckland, SASS,  17/5/91

      DATA Z1,Z2/1.96,1.645/

      PRINT *,' Program to calculate confidence intervals for',
     > ' abundance estimates,'
      PRINT *,' assuming a lognormal distribution'
      PRINT *
      PRINT *,' Enter abundance estimate'
      READ(5,*) P
      PRINT *,P
      PRINT *,' Enter standard error (enter 0.0 if you have cv instead)'
      READ(5,*) SEP

      IF(SEP.LT.1.0E-8) THEN
         PRINT *,' Enter coeff. of variation.  If cv is given',
     >    ' as %, divide by 100 first'
         READ(5,*) CV
         PRINT *,CV

         IF(CV.GT.1.5) THEN
            PRINT *,' *** WARNING ***'
            PRINT *,' Either estimate is abnormally imprecise or cv',
     >      ' was entered as a %'
         ENDIF

      ELSE
         PRINT *,SEP
         CV=SEP/P
      ENDIF

      C1=SQRT(LOG(1.0+CV**2))
      C=EXP(Z1*C1)
      PL=P/C
      PU=P*C

      PRINT *,' 95% confidence limits: ',PL,PU

      K=0
      N=PU
 10   N=N/10

      IF(N.GT.0) THEN
         K=K+1
         GOTO 10
      ENDIF

      IF(K.GT.2) THEN
         KK=10**(K-2)
         NL=0.5+PL/KK
         NU=0.5+PU/KK
         NL=KK*NL
         NU=KK*NU
      ELSE
         NL=PL+0.5
         NU=PU+0.5
      ENDIF

      PRINT *,' 95% rounded confidence limits: ',NL,NU

      C=EXP(Z2*C1)
      PL=P/C

      PRINT *,' Lower 5% limit: ',PL

      STOP
      END
