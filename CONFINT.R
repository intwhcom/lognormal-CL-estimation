#      PROGRAM CONFINT

#     Program to calculate confidence intervals for abundance estimates,
#     assuming a lognormal distribution

#     Programmer:  S.T. Buckland, SASS,  17/5/91 (fortran code)
#     translation to R by I. Katara, 03-06-2024

#      DATA Z1,Z2/1.96,1.645/

# Program to calculate confidence intervals for abundance estimates, assuming a lognormal distribution
confint <- function(P,SEP=NULL,CV=NULL){
#P abundance estimate
#SEP standard error 
#CV coefficient of variation. If cv is given as %, divide by 100 first
  library(gmp)
if (is.null(SEP)) {
  print("A CV is given")
  if (CV > 1.5) print("Either estimate is abnormally imprecise or CV was entered as a %")
} else {
  CV=SEP/P  
  }
Z1=1.96
Z2=1.645
C1=sqrt(log(1.0+CV**2))
C=exp(Z1*C1)
#95% confidence limits
PL=P/C
PU=P*C
#95% rounded confidence limits:
library(plyr)
NL = round_any(PL, 10)
NU = round_any(PU, 10)
#Lower 5% limit:
C=exp(Z2*C1)
PL5=P/C
return(data.frame(PL,PU,NL,NU,PL5))}

#example run: test <- confint(P=127,SEP=NULL,CV=0.08)
#library(readxl)
#data <- read_excel("Annex D - ASI - 2024-4-5.xlsx", sheet = "Table 2")
#for (i in 1:nrow(data)){
#  data[i,c("PL","PU","NL","NU","PL5")] <- confint(P=data$N[i],SEP=NULL,CV=data$CV[i])
#}
#write.csv(data,file = "CLs-gray.csv")
