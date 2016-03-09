Yinyang Guo
CIS 320
Project Report
	This project I was focused on US Aid, the data I selected was from usaid.org. The data was dated first in 1950 and until 2010.  The first thing I did was retrieve the data from the usaid.org website.
 
After I retrieve the data from the website, I summarized the data set by years and region and characterize them into groups. When the different region is characterized, this will make the analysis process much easier and more convenient.  
 
 
 
For my analysis, I was focused on Aid for Asia area, East Asia and Oceania, Europe and Eurasia and South and Central Asia. In the analysis, I will use R to demonstrate the changes and difference of Aid in different region of Asia. 
First, I will compare the three different region of Asia and their weight compare to the overall amount of aid sent. From the figure below, most of the aid in the 1950s was towards Europe and Eurasia possible reason is World War II. After 1955, there are significant decrease of aid towards Europe and Eurasia because the aid was sent to help rebuild. And around 1970s, East Asia and Oceania receive about half of the total aid. The possible reason would be the Vietnam War. From 1975 to 2000, there are very few aid sent towards Asia area. In 2005, the aid to South Central Asia was rising, because after U.S. deploy troops to Afghanistan to fight the war, aid was sent there to help rebuild. The oversea aid was at peak in 2010 to 2012. Beginning from 2013, the aid started to decrease significantly.  
 


	The following figure is the graph to show more detail of the aid over the years for the three area. Green is Europe and Eurasia, blue is East Asia and Red is South Central Asia. 

 

There are two type of aid U.S. sent oversea, they are military and economic. These two make up the total aid U.S. sent. For the most part, economic is U.S. preferred type of aid, there is only a few number of years that military aid exceed economic aid. It shows U.S. main focus is on economic growth. 

In Europe and Eurasia, the economic and military aid both have dramatic increases and decrease. In 2003, military aid spiked in that year. And during recent years, both military and economic have decrease significantly for Europe and Eurasia region. 

 
	South Central Asia was aided not much until after 2000. After 2000, both military and economic aid were provided to them, because in year 2001, U.S. deploy troops to that region to fight terrorist. Therefore, the military aid has more than economic aid. And after the troops were called back, the military aid is almost zero, while economic aid still provided but has steadily decrease.  
	East Asia is steady over the years except around 1970s, there was significant increase of military aid, and the reason could be the Vietnam War. After the war ended, the military aid dropped and kept a steady amount. While the military aid in East Asia is not strong, the economic aid has increased in the beginning of 2000, and recently the economic aid also dropped.   

	Overall, the U.S. aid to Asia is still a significant amount of the total. But because recent economic slowdown in Asia, both military and economic aid has decreased. Asia is mainly doing manufacturing for their economic growth. Because the labor cost in Asia is much cheaper than developed countries. In recent years, U.S. cut back foreign aid, therefore the aid has decrease in all three Asia region. Despite the aid cut back, the economic aid is still provided and firms can still go to those area to take advantage of those aid. But do not recommend military firm to go oversea, because there is very few military aid available in the foreign aid program. 

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
