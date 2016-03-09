R Code i used in the project

# cis320project
#getting data from www.usaid.gov/
data <- read.csv("https://explorer.usaid.gov/prepared/us_foreign_aid_country.csv")
names(data)
summary(data$fiscal_year)

summary(data$region_name)

E.Asia_Ocn <- data[which(data$region_name=="East Asia and Oceania"),]
Eu_As <- data[which(data$region_name=="Europe and Eurasia"),]
SCAsia <- data[which(data$region_name=="South and Central Asia"),]

year <- c(1946:2015)


TOT <- NULL
for(i in 1:length(year)){
  TOT[i] <- sum(data[which(data$fiscal_year==year[i]),]$current_amount)}

E.Asia_Ocn1 <- NULL
for(i in 1:length(year)){
  E.Asia_Ocn1[i] <- sum(E.Asia_Ocn[which(E.Asia_Ocn$fiscal_year==year[i]),]$current_amount)}

Eu_As1 <- NULL
for(i in 1:length(year)){
  Eu_As1[i] <- sum(Eu_As[which(Eu_As$fiscal_year==year[i]),]$current_amount)}

SCAsia1 <- NULL
for(i in 1:length(year)){
  SCAsia1[i] <- sum(SCAsia[which(SCAsia$fiscal_year==year[i]),]$current_amount)}




plot(year,TOT,type="l",col=1)
lines(year,E.Asia_Ocn1,type="l",col=4)
lines(year,Eu_As1,type="l",col=3)
lines(year,SCAsia1,type="l",col=2)


plot(year,E.Asia_Ocn1,type="l",col=4,ylim=c(0,3.4*10^10))
lines(year,Eu_As1,type="l",col=3)
lines(year,SCAsia1,type="l",col=2)


econ <- data[which(data$assistance_category_name=="Economic"),]
econ1 <- NULL
for(i in 1:length(year)){
  econ1[i] <- sum(econ[which(econ$fiscal_year==year[i]),]$current_amount)}

mil <- data[which(data$assistance_category_name=="Military"),]
mil1 <- NULL
for(i in 1:length(year)){
  mil1[i] <- sum(mil[which(mil$fiscal_year==year[i]),]$current_amount)}


plot(year,econ1,type="l",col="red")
lines(year,mil1,type="l",col="blue")


#Euro - Asia
Eu_As_econ <- Eu_As[which(Eu_As$assistance_category_name=="Economic"),]
Eu_As_econ1 <- NULL
for(i in 1:length(year)){
  Eu_As_econ1[i] <- sum(Eu_As_econ[which(Eu_As_econ$fiscal_year==year[i]),]$current_amount)}

Eu_As_mil <- Eu_As[which(Eu_As$assistance_category_name=="Military"),]
Eu_As_mil1 <- NULL
for(i in 1:length(year)){
  Eu_As_mil1[i] <- sum(Eu_As_mil[which(Eu_As_mil$fiscal_year==year[i]),]$current_amount)}


plot(year,Eu_As_econ1,type="l",col="red")
lines(year,Eu_As_mil1,type="l",col="blue")


#South - Central Asia
SCAsia_econ <- SCAsia[which(SCAsia$assistance_category_name=="Economic"),]
SCAsia_econ1 <- NULL
for(i in 1:length(year)){
  SCAsia_econ1[i] <- sum(SCAsia_econ[which(SCAsia_econ$fiscal_year==year[i]),]$current_amount)}

SCAsia_mil <- SCAsia[which(SCAsia$assistance_category_name=="Military"),]
SCAsia_mil1 <- NULL
for(i in 1:length(year)){
  SCAsia_mil1[i] <- sum(SCAsia_mil[which(SCAsia_mil$fiscal_year==year[i]),]$current_amount)}


plot(year,SCAsia_econ1,type="l",col="red",ylim=c(0,2.2*10^10))
lines(year,SCAsia_mil1,type="l",col="blue")


#Asia - Oceania
E.Asia_Ocn_econ <- E.Asia_Ocn[which(E.Asia_Ocn$assistance_category_name=="Economic"),]
E.Asia_Ocn_econ1 <- NULL
for(i in 1:length(year)){
  E.Asia_Ocn_econ1[i] <- sum(E.Asia_Ocn_econ[which(E.Asia_Ocn_econ$fiscal_year==year[i]),]$current_amount)}

E.Asia_Ocn_mil <- E.Asia_Ocn[which(E.Asia_Ocn$assistance_category_name=="Military"),]
E.Asia_Ocn_mil1 <- NULL
for(i in 1:length(year)){
  E.Asia_Ocn_mil1[i] <- sum(E.Asia_Ocn_mil[which(E.Asia_Ocn_mil$fiscal_year==year[i]),]$current_amount)}


plot(year,E.Asia_Ocn_econ1,type="l",col="red",ylim=c(0,5*10^9))
lines(year,E.Asia_Ocn_mil1,type="l",col="blue")
