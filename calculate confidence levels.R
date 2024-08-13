library(readxl)
source("C:/Users/IsidoraKatara/OneDrive - International Whaling Commission/Abundance-Tabs&Refs/Extras/CONFINT.R")

#Brydes
data <- read_excel("C:\\Users\\IsidoraKatara\\OneDrive - International Whaling Commission\\Abundance-Tabs&Refs\\Abund-Masters\\NP\\abundances-NP.xlsx", sheet = "brydes")
for (i in 1:nrow(data)){
  if (!is.na(data$cv[i])) data[i,c("PL","PU","NL","NU","PL5")] <- confint(P=data$estimate[i],SEP=NULL,CV=data$cv[i])
}
write.csv(data,file = "C:\\Users\\IsidoraKatara\\OneDrive - International Whaling Commission\\Abundance-Tabs&Refs\\Abund-Masters\\NP\\CLs-brydes.csv")
