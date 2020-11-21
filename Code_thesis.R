library(tidyverse)
library(lme4)
library(car)
library(pastecs)
library(stargazer)
library(ggthemes)
library(sjPlot)
library(GGally)
library(dplyr)
library(survey)

dta_trust <- readRDS("C:\\Users\\F. Dessoy Caraballo\\Desktop\\Thesis Literature Review\\WVS_TimeSeries_R_v1_2.rds")
load("C:/Users/F. Dessoy Caraballo/Desktop/Thesis Literature Review/ucdp-prio-acd-201.RData")
dta_terror <- read.csv("C:\\Users\\F. Dessoy Caraballo\\Desktop\\Thesis Literature Review\\globalterrorismdb.csv")

################# CLEANING DATASETS ######################

#A004 - important in life: politics
#A102 - Active/inactive membership of political party
#A165 - Most people can be trusted (1-2)
#E018 - Future changes: greater respect for authority (1-2)
#E023 - Interest in politics

dta_trust_clean <- dta_trust[,c("S020","COUNTRY_ALPHA","E023","E018","A102","A165","A004")] %>% 
  dplyr::filter(S020 <= 2011)

dta_conflict <- ucdp_prio_acd_201

dta_terror_clean <- dta_terror %>% 
  dplyr::filter(doubtterr == 1) %>% 
  dplyr::filter(claimed == 1) %>%
  dplyr::filter(iyear >= 1981)

dta_terror_clean <- dta_trust[,c()]

############### ETHIOPIA #####################
######## ETH2 2020 ####### ETH2 2007 #########

dta_eth1 <- dta_trust_clean %>% 
  dplyr::filter(COUNTRY_ALPHA == "ETH")

dta_eth2 <- dta_eth1 %>% 
  dplyr::filter(S020 == 2020)

dta_eth3 <- dta_eth1 %>% 
  dplyr::filter(S020 == 2007)

ggplot(data=dta_eth2, aes(E023)) + 
  geom_histogram()

ggplot(data=dta_eth3, aes(E023)) + 
  geom_histogram()

ggplot(data=dta_eth2, aes(E018)) + 
  geom_histogram()

ggplot(data=dta_eth3, aes(E018)) + 
  geom_histogram()

ggplot(dta_eth2, aes(x=A165))+
  geom_histogram(color="darkblue", fill="lightblue")

ggplot(data=dta_eth, aes(E023)) + 
  geom_histogram()

dta_eth2$A165_cat=ifelse(dta_eth2$A165> 1, "Trust", "Doesn't trust")
dta_eth2$A165_cat<-factor(dta_eth2$A165_cat)
boxplot(dta_eth2$A004~dta_eth2$A165_cat, 
        col='yellow', xlab='Trust and politics importance', 
        ylab='Trust')

dta_conflict_eth <- dta_conflict %>% 
  dplyr::filter(location == "Ethiopia")

ggplot(data = dta_conflict_eth, aes(x = start_date, y = intensity_level))+
  geom_point(color = "#00AFBB", size = 2)

#################################################################################
############################### MERGING DATASETS ################################

merged_data = merge(dta_terror,dta_clean,by.x=c("ISOcode", "year"),by.y=c("Code", "Year"))




#ggplot(data = dta_eth, aes(x = S020, y = E018))+
#  geom_line(color = "#00AFBB", size = 2)

#ggplot(data = dta_eth, mapping = aes(x = E018), na.rm=TRUE) +
#  geom_histogram()
#plot(dta_eth$E018~dta_eth$S020)
#plot.ts(dta_eth$E018)
#ggplot(ucdp_prio_acd_201, aes(x=wt, y=mpg)) +
#  geom_point()

rawData$polyarchy_cat=ifelse(rawData$v2x_polyarchy> .5, "Polyarchy", "Autocracy")
rawData$polyarchy_cat<-factor(rawData$polyarchy_cat)
boxplot(rawData$cases_log~rawData$polyarchy_cat, 
        col='yellow', xlab='Type of Democracy', 
        ylab='Number of Cases')

#Next step: both datasets are time series, so a double y axes chart should help visualize negative and positive case





#############################################################################################################################
svyhist(~ as.numeric("S01026"), dadosPNADc, main = "Cable TV", xlab = "asdasdas", freq = TRUE)

svyboxplot("S010301" ~ "S010302", dadosPNADc, main = "Boxplot do Número de Horas Trabalhadas por Sexo")

ggplot(data=dadosPNADc)+
  geom_density(aes(x="S010301", colour= "Computer"), lwd=1)+
  geom_density(aes(x="S010302", colour= "Tablet"), lwd=1)+
  geom_density(aes(x="S010303", colour= "Smartphone"), lwd=1)+
  geom_density(aes(x="S010304", colour= "Television"), lwd=1)+
  labs(x = "Internet: which gadget you use?")

ggplot(data = dadosPNADc, aes(x = "S01026", y = "S01025")) +
  geom_smooth(method = "loess", se = TRUE, colour = "black") +
  geom_hline(yintercept = 0, linetype = 2, colour = "black") +
  ggtitle("TVs and cable TV")+
  xlab("Cable") +
  ylab("TV")

ggplot(dadosPNADc, aes(x="S01026",y="S01025"))+
  geom_point(size=.1)+
  geom_smooth(method = "lm", fill="navy", colour="red")+
  ggtitle("TVs and cable TV")+
  labs(y="TV", x="Cable TV")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        plot.title=element_text(size=12))

plot(dadosPNADc$S01026,dadosPNADc$S01025, data=dadosPNADc, xlab="Norm Cases", ylab="Polyarchy and Population")
abline(lm(dadosPNADc$S01025~dadosPNADc$S01026, data = dadosPNADc), col = "blue")
