#NOTE: CANNOT just run the entire script; have to install the packages and change the path of the Excel workbook

# install.packages("readxl")
library("readxl")

# importing "Media Reach" and "Other factors" worksheets as separate dataframes into R for Mansi's computer
OF <- read_excel("C://Users//Mansi.Parikh//Documents//Whitney Budget Modelling.xlsx", sheet = 1)
MR <- read_excel("C://Users//Mansi.Parikh//Documents//Whitney Budget Modelling.xlsx", sheet = 2)
VI <- read_excel("C://Users//Mansi.Parikh//Documents//Visitors.xlsx", sheet = 2)
VO <- read_excel("C://Users//Mansi.Parikh//Documents//Visitors.xlsx", sheet = 4)
VN <- read_excel("C://Users//Mansi.Parikh//Documents//Visitors.xlsx", sheet = 6)
VI2 <- read_excel("C://Users//Mansi.Parikh//Documents//Visitors.xlsx", sheet = 8)
VO2 <- read_excel("C://Users//Mansi.Parikh//Documents//Visitors.xlsx", sheet = 10)
VN2 <- read_excel("C://Users//Mansi.Parikh//Documents//Visitors.xlsx", sheet = 12)
MR$ID <- seq.int(nrow(MR))

# importing "Media Reach" and "Other factors" worksheets as separate dataframes into R for Corina's computer
# OF <- read_excel("C://Users//Corina.Constantin//Downloads//Whitney Budget Modelling.xlsx", sheet = 1)
# MR <- read_excel("C://Users//Corina.Constantin//Downloads//Whitney Budget Modelling.xlsx", sheet = 2)

# calculating the average temperate for the week
OF$AvgTemp = ((OF$TempMax)+(OF$TempMin))/2

# to add lags for total impressions
# install.packages("plyr")
library(plyr)
MR$IR1 = MR$IR
MR <- ddply(MR, .(Advertiser), transform, IR1 = c(NA, IR1[-length(IR1)]))
MR$IR2 = MR$IR1
MR <- ddply(MR, .(Advertiser), transform, IR2 = c(NA, IR2[-length(IR2)]))
MR$IR3 = MR$IR2
MR <- ddply(MR, .(Advertiser), transform, IR3 = c(NA, IR3[-length(IR3)]))
MR$IR4 = MR$IR3
MR <- ddply(MR, .(Advertiser), transform, IR4 = c(NA, IR4[-length(IR4)]))
MR$IR5 = MR$IR4
MR <- ddply(MR, .(Advertiser), transform, IR5 = c(NA, IR5[-length(IR5)]))

# dependent variables (adult visitors) - LOG TRANSFORMATION NOT NECESSARY
hist(MR$TotalVisitors)
hist(log(MR$TotalVisitors))
#
hist(MR$International)
hist(log(MR$International))
#
hist(MR$OtherUS)
hist(log(MR$OtherUS))
#
hist(MR$TriStateNYC)
hist(log(MR$TriStateNYC))
# independent variables (IR) logged - should be done for ALL IRs
# independent variables (same time impressions)
hist(MR$IR)
MR$IR_LOG <- log(MR$IR + 1)
hist(MR$IR_LOG)
# independent variables (lagged impressions)
MR$IR1_LOG <- log(MR$IR1+1)
MR$IR2_LOG <- log(MR$IR2+1)
MR$IR3_LOG <- log(MR$IR3+1)
MR$IR4_LOG <- log(MR$IR4+1)
MR$IR5_LOG <- log(MR$IR5+1)

# joining both sets of data so that covariates can be added to the adult visitor ~ impression model
MR$weeknum <- strftime(MR$Week, format = "%V")
MR$year <- format(as.Date(MR$Week, format="%d/%m/%Y"),"%Y")
#
MR$weeknum_S <- as.character(MR$weeknum)
MR$year_S <- as.character(MR$year)
#
MR$dateUI <- with(MR, paste0(year_S,weeknum_S))
#
OF$weeknum <- strftime(OF$DateSelector, format = "%V")
OF$year <- format(as.Date(OF$DateSelector, format="%d/%m/%Y"),"%Y")
#
OF$weeknum_S <- as.character(OF$weeknum)
OF$year_S <- as.character(OF$year)
#
OF$dateUI <- with(OF, paste0(year_S,weeknum_S))
#
drops <- c("weeknum","year","weeknum_S","year_S","X_1","X_2","X_3","X_4","X_5")
OF <- OF[ , !(names(OF) %in% drops)]
MR <- MR[ , !(names(MR) %in% drops)]

# campaign-specific data sets
MRc1 <- MR[c(1:19), ]
MRc2 <- MR[c(20:37), ]
MRc3 <- MR[c(38:42), ]
MRc4 <- MR[c(43:75), ]
#
MRc1$IR_LOG <- log(MRc1$IR+1)
MRc1$IR1 = MRc1$IR
MRc1 <- ddply(MRc1, .(Advertiser), transform, IR1 = c(NA, IR1[-length(IR1)]))
MRc1$IR1_LOG <- log(MRc1$IR1+1)
MRc1$IR2 = MRc1$IR1
MRc1 <- ddply(MRc1, .(Advertiser), transform, IR2 = c(NA, IR2[-length(IR2)]))
MRc1$IR2_LOG <- log(MRc1$IR2+1)
MRc1$IR3 = MRc1$IR2
MRc1 <- ddply(MRc1, .(Advertiser), transform, IR3 = c(NA, IR3[-length(IR3)]))
MRc1$IR3_LOG <- log(MRc1$IR3+1)
#
MRc2$IRLOG <- log(MRc2$IR+1)
MRc2$IR1 = MRc2$IR
MRc2 <- ddply(MRc2, .(Advertiser), transform, IR1 = c(NA, IR1[-length(IR1)]))
MRc2$IR1_LOG <- log(MRc2$IR1+1)
MRc2$IR2 = MRc2$IR1
MRc2 <- ddply(MRc2, .(Advertiser), transform, IR2 = c(NA, IR2[-length(IR2)]))
MRc2$IR2_LOG <- log(MRc2$IR2+1)
MRc2$IR3 = MRc2$IR2
MRc2 <- ddply(MRc2, .(Advertiser), transform, IR3 = c(NA, IR3[-length(IR3)]))
MRc2$IR3_LOG <- log(MRc2$IR3+1)
#
MRc3$IRLOG <- log(MRc3$IR+1)
MRc3$IR1 = MRc3$IR
MRc3 <- ddply(MRc3, .(Advertiser), transform, IR1 = c(NA, IR1[-length(IR1)]))
MRc3$IR1_LOG <- log(MRc3$IR1+1)
MRc3$IR2 = MRc3$IR1
MRc3 <- ddply(MRc3, .(Advertiser), transform, IR2 = c(NA, IR2[-length(IR2)]))
MRc3$IR2_LOG <- log(MRc3$IR2+1)
MRc3$IR3 = MRc3$IR2
MRc3 <- ddply(MRc3, .(Advertiser), transform, IR3 = c(NA, IR3[-length(IR3)]))
MRc3$IR3_LOG <- log(MRc3$IR3+1)
#
MRc4$IRLOG <- log(MRc4$IR+1)
MRc4$IR1 = MRc4$IR
MRc4 <- ddply(MRc4, .(Advertiser), transform, IR1 = c(NA, IR1[-length(IR1)]))
MRc4$IR1_LOG <- log(MRc4$IR1+1)
MRc4$IR2 = MRc4$IR1
MRc4 <- ddply(MRc4, .(Advertiser), transform, IR2 = c(NA, IR2[-length(IR2)]))
MRc4$IR2_LOG <- log(MRc4$IR2+1)
MRc4$IR3 = MRc4$IR2
MRc4 <- ddply(MRc4, .(Advertiser), transform, IR3 = c(NA, IR3[-length(IR3)]))
MRc4$IR3_LOG <- log(MRc4$IR3+1)
# joining data so that covariates can be added to the 4 campaign adult visitor ~ impression models
cDF <- merge(OF, MR, by = "dateUI")
cDFc1 <- merge(OF, MRc1, by = "dateUI")
cDFc2 <- merge(OF, MRc2, by = "dateUI")
cDFc3 <- merge(OF, MRc3, by = "dateUI")
cDFc4 <- merge(OF, MRc4, by = "dateUI")

# International Visitors
scatter.smooth(x=MR$IR, y=MR$International, main="International Visitors ~ Impression Reach")
# maybe find Pearson's correlation coefficient
scatter.smooth(x=MR$IR1, y=MR$International, main="International Visitors ~ Impression Reach (1 lag)")
# correlation coefficient for lagged scatter

intl <- lm(International ~ IR1 + IR2 + IR3, data=MR)
summary(intl)

intlCOV <- lm(International ~ IR1 + IR2 + IR3 + AvgTemp + PrecipAvg + `HL:W` + `CP:W` + `AA:W`, data=cDF)
summary(intlCOV)

intl_LOG <- lm(International ~ IR1_LOG + IR2_LOG + IR3_LOG, data=MR)
summary(intl_LOG)

intlCOV_LOG <- lm(International ~ IR1_LOG + IR2_LOG + IR3_LOG + AvgTemp + PrecipAvg + `HL:W` + `CP:W` + `AA:W`, data=cDF)
summary(intlCOV_LOG)

intlCOVcam_LOG <- lm(International ~ IR1_LOG + IR2_LOG + IR3_LOG + C2 + C3 + C4 + AvgTemp + PrecipAvg + `HL:W` + `CP:W` + `AA:W`, data=cDF)
summary(intlCOVcam_LOG)

intlc1 <- lm(International ~ IR1 + IR2 + IR3, data=MRc1)
summary(intlc1)
intlCOVc1 <- lm(International ~ IR1 + + IR2 + IR3 + AvgTemp + PrecipAvg + `HL:W` + `CP:W` + `AA:W`, data=cDFc1)
summary(intlCOVc1)
intlc1_LOG <- lm(International ~ IR1_LOG + IR2_LOG + IR3_LOG, data=MRc1)
summary(intlc1_LOG)
intlCOVc1_LOG <- lm(International ~ IR1_LOG + IR2_LOG + IR3_LOG + AvgTemp + PrecipAvg + `HL:W` + `CP:W` + `AA:W`, data=cDFc1)
summary(intlCOVc1_LOG)

intlc2 <- lm(International ~ IR1 + IR2 + IR3, data=MRc2)
summary(intlc2)
intlCOVc2 <- lm(International ~ IR1 + + IR2 + IR3 + AvgTemp + PrecipAvg + `HL:W` + `CP:W` + `AA:W`, data=cDFc2)
summary(intlCOVc2)
intlc2_LOG <- lm(International ~ IR1_LOG + IR2_LOG + IR3_LOG, data=MRc2)
summary(intlc2_LOG)
intlCOVc2_LOG <- lm(International ~ IR1_LOG + IR2_LOG + IR3_LOG + AvgTemp + PrecipAvg + `HL:W` + `CP:W` + `AA:W`, data=cDFc2)
summary(intlCOVc2_LOG)

intlc3 <- lm(International ~ IR1, data=MRc3)
summary(intlc3)
intlCOVc3 <- lm(International ~ IR1 + PrecipAvg + `AA:W`, data=cDFc3) ### FEWER LAGS AND COVARIATES
summary(intlCOVc3)
intlc3_LOG <- lm(International ~ IR1_LOG, data=MRc3)
summary(intlc3_LOG)
intlCOVc3_LOG <- lm(International ~ IR1_LOG + PrecipAvg + `AA:W`, data=cDFc3) ### FEWER LAGS AND COVARIATES
summary(intlCOVc3_LOG)
#
summary(lm(International ~ PrecipAvg + `AA:W`, data=cDFc3))

intlc4 <- lm(International ~ IR1 + IR2 + IR3, data=MRc4)
summary(intlc4)
intlCOVc4 <- lm(International ~ IR1 + + IR2 + IR3 + AvgTemp + PrecipAvg + `HL:W` + `CP:W` + `AA:W`, data=cDFc4)
summary(intlCOVc4)
intlc4_LOG <- lm(International ~ IR1_LOG + IR2_LOG + IR3_LOG, data=MRc4)
summary(intlc4_LOG)
intlCOVc4_LOG <- lm(International ~ IR1_LOG + IR2_LOG + IR3_LOG + AvgTemp + PrecipAvg + `HL:W` + `CP:W` + `AA:W`, data=cDFc4)
summary(intlCOVc4_LOG)

# International Visitors - Expedia Impressions
# scatter plots? 
MRe <- MR
MRe$E1 = MRe$EXPEDIAIR
MRe <- ddply(MRe, .(Advertiser), transform, E1 = c(NA, E1[-length(E1)]))
MRe$E2 = MRe$E1
MRe <- ddply(MRe, .(Advertiser), transform, E2 = c(NA, E2[-length(E2)]))
intlE <- lm(International ~ E1 + E2, data=MRe)
summary(intlE)

cDFe <- merge(OF, MRe, by = "dateUI")

intlECOV <- lm(International ~ E1 + E2 + AvgTemp + PrecipAvg + `HL:W` + `CP:W` + `AA:W`, data=cDFe)
summary(intlECOV)

# Other U.S. Visitors
scatter.smooth(x=MR$IR, y=MR$OtherUS, main="Other U.S. Visitors ~ Impression Reach")
scatter.smooth(x=MR$IR1, y=MR$OtherUS, main="Other U.S. Visitors ~ Impression Reach (1 lag)")

other <- lm(OtherUS ~ IR1 + IR2 + IR3, data=MR)
summary(other)

otherCOV <- lm(OtherUS ~ IR1 + IR2 + IR3 + AvgTemp + PrecipAvg + `HL:US` + `CP:US` + `AA:US`, data=cDF)
summary(otherCOV)

other_LOG <- lm(OtherUS ~ IR1_LOG + IR2_LOG + IR3_LOG, data=MR)
summary(other_LOG)

otherCOV_LOG <- lm(OtherUS ~ IR1_LOG + IR2_LOG + IR3_LOG + AvgTemp + PrecipAvg + `HL:US` + `CP:US` + `AA:US`, data=cDF)
summary(otherCOV_LOG)

otherCOVcam_LOG <- lm(OtherUS ~ IR1_LOG + IR2_LOG + IR3_LOG + C2 + C3 + C4 + AvgTemp + PrecipAvg + `HL:US` + `CP:US` + `AA:US`, data=cDF)
summary(otherCOVcam_LOG)

otherc1 <- lm(OtherUS ~ IR1 + IR2 + IR3, data=MRc1)
summary(otherc1)
otherCOVc1 <- lm(OtherUS ~ IR1 + + IR2 + IR3 + AvgTemp + PrecipAvg + `HL:US` + `CP:US` + `AA:US`, data=cDFc1)
summary(otherCOVc1)
otherc1_LOG <- lm(OtherUS ~ IR1_LOG + IR2_LOG + IR3_LOG, data=MRc1)
summary(otherc1_LOG)
otherCOVc1_LOG <- lm(OtherUS ~ IR1_LOG + IR2_LOG + IR3_LOG + AvgTemp + PrecipAvg + `HL:US` + `CP:US` + `AA:US`, data=cDFc1)
summary(otherCOVc1_LOG)

otherc2 <- lm(OtherUS ~ IR1 + IR2 + IR3, data=MRc2)
summary(otherc2)
otherCOVc2 <- lm(OtherUS ~ IR1 + IR2 + IR3 + AvgTemp + PrecipAvg + `HL:US` + `CP:US` + `AA:US`, data=cDFc2)
summary(otherCOVc2)
otherc2_LOG <- lm(OtherUS ~ IR1_LOG + IR2_LOG + IR3_LOG, data=MRc2)
summary(otherc2_LOG)
otherCOVc2_LOG <- lm(OtherUS ~ IR1_LOG + IR2_LOG + IR3_LOG + AvgTemp + PrecipAvg + `HL:US` + `CP:US` + `AA:US`, data=cDFc2)
summary(otherCOVc2_LOG)

otherc3 <- lm(OtherUS ~ IR1, data=MRc3)
summary(otherc3)
otherCOVc3 <- lm(OtherUS ~ IR1 + PrecipAvg + `AA:US`, data=cDFc3) ### FEWER LAGS AND COVARIATES
summary(otherCOVc3)
otherc3_LOG <- lm(OtherUS ~ IR1_LOG, data=MRc3)
summary(otherc3_LOG)
otherCOVc3_LOG <- lm(OtherUS ~ IR1_LOG + PrecipAvg + `AA:US`, data=cDFc3) ### FEWER LAGS AND COVARIATES
summary(otherCOVc3_LOG)

otherc4 <- lm(OtherUS ~ IR1 + IR2 + IR3, data=MRc4)
summary(otherc4)
otherCOVc4 <- lm(OtherUS ~ IR1 + + IR2 + IR3 + AvgTemp + PrecipAvg + `HL:US` + `CP:US` + `AA:US`, data=cDFc4)
summary(otherCOVc4)
otherc4_LOG <- lm(OtherUS ~ IR1_LOG + IR2_LOG + IR3_LOG, data=MRc4)
summary(otherc4_LOG)
otherCOVc4_LOG <- lm(OtherUS ~ IR1_LOG + IR2_LOG + IR3_LOG + AvgTemp + PrecipAvg + `HL:US` + `CP:US` + `AA:US`, data=cDFc4)
summary(otherCOVc4_LOG)

# NYC and Tri-State Visitors
scatter.smooth(x=MR$IR, y=MR$TriStateNYC, main="NYC and Tri-State Visitors ~ Impression Reach")
scatter.smooth(x=MR$IR1, y=MR$TriStateNYC, main="NYC and Tri-State Visitors ~ Impression Reach (1 lag)")

trinyc <- lm(TriStateNYC ~ IR + IR1, data=MR)
summary(trinyc)

trinycCOV <- lm(TriStateNYC ~ IR + IR1 + AvgTemp + PrecipAvg + `HL:NY` + `CP:NY` + `AA:NY`, data=cDF)
summary(trinycCOV)

trinyc_LOG <- lm(TriStateNYC ~ IRLOG + IR1_LOG + IR2_LOG + IR3_LOG, data=MR)
summary(trinyc_LOG)

trinycCOV_LOG <- lm(TriStateNYC ~ IRLOG + IR1_LOG + IR2_LOG + IR3_LOG + AvgTemp + PrecipAvg + `HL:NY` + `CP:NY` + `AA:NY`, data=cDF)
summary(trinycCOV_LOG)

trinycCOVcam_LOG <- lm(TriStateNYC ~ IRLOG + IR1_LOG + IR2_LOG + IR3_LOG + C2 + C3 + C4 + AvgTemp + PrecipAvg + `HL:NY` + `CP:NY` + `AA:NY`, data=cDF)
summary(trinycCOVcam_LOG)

trinycc1 <- lm(TriStateNYC ~ IR + IR1 + IR2 + IR3, data=MRc1)
summary(trinycc1)
trinycCOVc1 <- lm(TriStateNYC ~ IR + IR1 + IR2 + IR3 + AvgTemp + PrecipAvg + `HL:NY` + `CP:NY` + `AA:NY`, data=cDFc1)
summary(trinycCOVc1)
trinycc1_LOG <- lm(TriStateNYC ~ IRLOG + IR1_LOG + IR2_LOG + IR3_LOG, data=MRc1)
summary(trinycc1_LOG)
trinycCOVc1_LOG <- lm(TriStateNYC ~ IRLOG + IR1_LOG + IR2_LOG + IR3_LOG + AvgTemp + PrecipAvg + `HL:NY` + `CP:NY` + `AA:NY`, data=cDFc1)
summary(trinycCOVc1_LOG)

trinycc2 <- lm(TriStateNYC ~ IR + IR1 + IR2 + IR3, data=MRc2)
summary(trinycc2)
trinycCOVc2 <- lm(TriStateNYC ~ IR + IR1 + IR2 + IR3 + AvgTemp + PrecipAvg + `HL:NY` + `CP:NY` + `AA:NY`, data=cDFc2)
summary(trinycCOVc2)
trinycc2_LOG <- lm(TriStateNYC ~ IRLOG + IR1_LOG + IR2_LOG + IR3_LOG, data=MRc2)
summary(trinycc2_LOG)
trinycCOVc2_LOG <- lm(TriStateNYC ~ IRLOG + IR1_LOG + IR2_LOG + IR3_LOG + AvgTemp + PrecipAvg + `HL:NY` + `CP:NY` + `AA:NY`, data=cDFc2)
summary(trinycCOVc2_LOG)

trinycc3 <- lm(TriStateNYC ~ IR + IR1, data=MRc3)
summary(trinycc3)
trinycCOVc3 <- lm(TriStateNYC ~ IR + IR1 + PrecipAvg + `AA:NY`, data=cDFc3) ### FEWER LAGS AND COVARIATES
summary(trinycCOVc3)
trinycc3_LOG <- lm(TriStateNYC ~ IRLOG + IR1_LOG, data=MRc3)
summary(trinycc3_LOG)
trinycCOVc3_LOG <- lm(TriStateNYC ~ IRLOG + IR1_LOG + PrecipAvg + `AA:NY`, data=cDFc3) ### FEWER LAGS AND COVARIATES
summary(trinycCOVc3_LOG)

trinycc4 <- lm(TriStateNYC ~ IR + IR1 + IR2 + IR3, data=MRc4)
summary(trinycc4)
trinycCOVc4 <- lm(TriStateNYC ~ IR + IR1 + + IR2 + IR3 + AvgTemp + PrecipAvg + `HL:NY` + `CP:NY` + `AA:NY`, data=cDFc4)
summary(trinycCOVc4)
trinycc4_LOG <- lm(TriStateNYC ~ IRLOG + IR1_LOG + IR2_LOG + IR3_LOG, data=MRc4)
summary(trinycc4_LOG)
trinycCOVc4_LOG <- lm(TriStateNYC ~ IRLOG + IR1_LOG + IR2_LOG + IR3_LOG + AvgTemp + PrecipAvg + `HL:NY` + `CP:NY` + `AA:NY`, data=cDFc4)
summary(trinycCOVc4_LOG)










# install.packages("minpack.lm")
library(minpack.lm)
adstock<-function(x,rate=0){
  return(as.numeric(filter(x=x,filter=rate,method="recursive"))) }
International <- MR$International
IR <- MR$IR

# method 1
modFit <- nls(MR$International~b0+b1*adstock(MR$IR, rate), start=c(b0=1, b1=1, rate=0), 
              control = list(maxiter = 500), trace = TRUE)
summary(modFit)

modFit <- nls(MR$International~b0+b1*adstock(MR$IR, rate), start=c(b0=1, b1=1, rate=0), 
              algorithm = "port", control = list(maxiter = 500), trace = TRUE)
summary(modFit)

# method 2 
modFit <- nlsLM(MR$International~b0 + b1 * adstock(MR$IR, r1), 
       algorithm = "LM",
       start     = c(b0=   1, b1=   1, r1=0),
       lower     = c(b0=-Inf, b1=-Inf, r1=0),
       upper     = c(b0= Inf, b1= Inf, r1=1))
summary(modFit)

modFit <- nlsLM(MR$International~b0 + b1 * adstock(MR$IR, r1), 
                algorithm = "plinear",
                start     = c(b0=   1, b1=   1, r1=0),
                lower     = c(b0=-Inf, b1=-Inf, r1=0),
                upper     = c(b0= Inf, b1= Inf, r1=1))
summary(modFit)

# method 2a - Mansi
modFit <- nlsLM(MR$International~b0 + b1 * adstock(MR$IR, r1) + 
  b2 * adstock(MR$IR, r2) +
  b3 * adstock(MR$IR, r3) +
  b4 * adstock(MR$IR, r4) +
  b5 * adstock(MR$IR, r5) + 
  b6 * adstock(MR$IR, r6), 
  algorithm = "LM",
  start = c(b0=1, b1=1, b2=1, b3=1, b4=1, b5=1, b6=1, 
            r1=0, r2=0, r3=0, r4=0, r5=0, r6=0),
  lower = c(b0=-Inf, b1=-Inf, b2=-Inf, b3=-Inf, b4=-Inf, b5=-Inf, b6=-Inf, 
            r1=0, r2=0, r3=0, r4=0, r5=0, r6=0),
  upper = c(b0=Inf, b1=Inf, b2=Inf, b3=Inf, b4=Inf, b5=Inf, b6=Inf,
            r1=1, r2=1, r3=1, r4=1, r5=1, r6=1),
  control = list(maxiter = 500), trace = TRUE)
summary(modFit)

# method 2b - Mansi
modFit <- nlsLM(MR$International~b0 + b1 * adstock(MR$IR, r1) + 
                  b2 * adstock(MR$IR, r1) +
                  b3 * adstock(MR$IR, r1) +
                  b4 * adstock(MR$IR, r1) +
                  b5 * adstock(MR$IR, r1) + 
                  b6 * adstock(MR$IR, r1), 
                algorithm = "LM",
                start = c(b0=1, b1=1, b2=1, b3=1, b4=1, b5=1, b6=1, 
                          r1=0),
                lower = c(b0=-Inf, b1=-Inf, b2=-Inf, b3=-Inf, b4=-Inf, b5=-Inf, b6=-Inf, 
                          r1=0),
                upper = c(b0=Inf, b1=Inf, b2=Inf, b3=Inf, b4=Inf, b5=Inf, b6=Inf,
                          r1=1),
                control = list(maxiter = 500), trace = TRUE)
summary(modFit)

# method 3
AdstockRate<-function(MR,International,IR) {
  modFit<-nls(data=MR,International~a+b*adstock(IR,rate),
      control = list(maxiter = 500), trace = TRUE, algorithm = "port",
      start=c(a=1,b=1,rate=0),
      lower=c(a=-Inf,b=-Inf,rate=0),
      upper=c(a=Inf,b=Inf,rate=1))
  if(summary(modFit)$coefficients[3,1]>0) {
    AdstockRate=summary(modFit)#$coefficients[3,1]
  } else {
    modFit<-nlsLM(International~a+b*adstock(IR,rate),data=MR,
      control = list(maxiter = 500), trace = TRUE, algorithm = "LM",
      start=c(a=1,b=1,rate=0),
      lower=c(a=-Inf,b=-Inf,rate=0),
      upper=c(a=Inf,b=Inf,rate=1)) 
    AdstockRate=summary(modFit)#$coefficients[3,1]
  }
return(AdstockRate)
}
AdstockRate(MR,International,IR)



# adstocking for international adults
adstock_rate_I <- 0.5849
max_memory <- 6

learn_rates_I <- rep(adstock_rate_I, max_memory+1) ^ c(0:max_memory)
as_impressions_I <- c(0)
as_impressions_I <- filter(c(rep(0, max_memory), MR$IR), learn_rates_I, method="convolution")
as_impressions_I <- as_impressions_I[!is.na(as_impressions_I)]
MR <- cbind(MR, as_impressions_I)
# option for without max memory
# MR$adstocked_impressions_I = filter(x=MR$IR, filter=adstock_rate, method="recursive")

# adstocking for other U.S. adults
adstock_rate_O <- 0.5916
max_memory <- 6

learn_rates_O <- rep(adstock_rate_O, max_memory+1) ^ c(0:max_memory)
as_impressions_O <- c(0)
as_impressions_O <- filter(c(rep(0, max_memory), MR$IR), learn_rates_O, method="convolution")
as_impressions_O <- as_impressions_O[!is.na(as_impressions_O)]
MR <- cbind(MR, as_impressions_O)

# adstocking for other NYC and tri-state adults
adstock_rate_N <- 0.7628
max_memory <- 6

learn_rates_N <- rep(adstock_rate_N, max_memory+1) ^ c(0:max_memory)
as_impressions_N <- c(0)
as_impressions_N <- filter(c(rep(0, max_memory), MR$IR), learn_rates_N, method="convolution")
as_impressions_N <- as_impressions_N[!is.na(as_impressions_N)]
MR <- cbind(MR, as_impressions_N)









# adstocking impressions chart (adstocked impressions vs. impressions) for international adults
plot(seq(1,length(MR$IR)), MR$IR, xaxt="n", type="h", col="purple",
xlab="Week", ylab="IR", xlim=c(0, max(MR$ID)),
ylim=c(0, max(c(MR$IR, MR$as_impressions_I))), frame.plot=FALSE)
axis(1, xaxp=c(0, 75, 5), las=2)
lines(MR$as_impressions_I, col="red")
legend(5, 3000000, legend=c("Impressions", "Adstocked Impressions"),
col=c("purple","red"), lty=1:1, cex=0.8)
# adstocked impressions vs. international visitors
par(mar = c(4, 4, 4, 4))
plot(seq(1,length(MR$as_impressions_I)), MR$as_impressions_I, 
xaxt="n", yaxt="n", type="l", pch=18, col="red", xlab="Weeks", ylab="Adstocked Impressions", 
ylim=c(0, max(c(MR$International, MR$as_impressions_I))), frame.plot=FALSE)
axis(1, xaxp=c(0, 75, 5), las=2)
axis(2, yaxp=c(0, 3600000, 6), las=0)
par(new = T)
with(MR, plot(seq(1,length(MR$International)), MR$International, type="l", pch=19, col="blue", xaxt="n", yaxt="n",
xlab="", ylab="", xlim=c(0, length(MR$International)), ylim=c(0, max(MR$International)), frame.plot=FALSE))
axis(4, at=seq(0,6000,by=500),las=0)
mtext(side = 4, line = 3, "International Visitors")
legend(45, 1000, legend=c("Adstocked Impressions", "International Visitors"),
col=c("red", "blue"), lty=1:1, cex=0.8)






AS_intl <- lm(International ~ as_impressions_I, data=MR)
summary(AS_intl)

AS_intlCOV <- lm(International ~ as_impressions_I + AvgTemp + PrecipAvg + `HL:W` + `CP:W` + `AA:W`, data=cDF)
summary(AS_intlCOV)

# do not need to log AS impressions for intl
hist(MR$as_impressions_I)
hist(log(MR$as_impressions_I + 1))

AS_intlCOVcam <- lm(International ~ as_impressions_I + C2 + C3 + C4 + AvgTemp + PrecipAvg + `HL:W` + `CP:W` + `AA:W`, data=cDF)
summary(AS_intlCOVcam)



AS_other <- lm(OtherUS ~ as_impressions_O, data=MR)
summary(AS_intl)

AS_otherCOV <- lm(OtherUS ~ as_impressions_O + AvgTemp + PrecipAvg + `HL:US` + `CP:US` + `AA:US`, data=cDF)
summary(AS_otherCOV)

# do not need to log AS impressions for intl
hist(MR$as_impressions_O)
hist(log(MR$as_impressions_O + 1))

AS_otherCOVcam <- lm(OtherUS ~ as_impressions_O + C2 + C3 + C4 + AvgTemp + PrecipAvg + `HL:US` + `CP:US` + `AA:US`, data=cDF)
summary(AS_otherCOVcam)




AS_trinyc <- lm(TriStateNYC ~ as_impressions_N, data=MR)
summary(AS_trinyc)

AS_trinycCOV <- lm(TriStateNYC ~ as_impressions_N + AvgTemp + PrecipAvg + `HL:NY` + `CP:NY` + `AA:NY`, data=cDF)
summary(AS_trinycCOV)

# do not need to log AS impressions for NYC+tri-state
hist(MR$as_impressions_N)
hist(log(MR$as_impressions_N + 1))

AS_trinycCOVcam <- lm(TriStateNYC ~ as_impressions_N + C2 + C3 + C4 + AvgTemp + PrecipAvg + `HL:NY` + `CP:NY` + `AA:NY`, data=cDF)
summary(AS_nyctriCOVcam)









plot(as.ts(VI$International))
plot(as.ts(VO$OtherUS))
plot(as.ts(VN$TriStateNYC))


ts_intlM = ts(VI$International, frequency = 52)
decompose_intlM = decompose(ts_intlM, "multiplicative")
plot(as.ts(decompose_intlM$seasonal))
plot(as.ts(decompose_intlM$trend))
plot(as.ts(decompose_intlM$random))
plot(decompose_intlM)
#
ts_intlA = ts(VI$International, frequency = 52)
decompose_intlA = decompose(ts_intlA, "additive")
plot(as.ts(decompose_intlA$seasonal))
plot(as.ts(decompose_intlA$trend))
plot(as.ts(decompose_intlA$random))
plot(decompose_intlA)
#
VI$Intl_random <- decompose_intlA$random

ts_otherM = ts(VO$OtherUS, frequency = 52)
decompose_otherM = decompose(ts_otherM, "multiplicative")
plot(as.ts(decompose_otherM$seasonal))
plot(as.ts(decompose_otherM$trend))
plot(as.ts(decompose_otherM$random))
plot(decompose_otherM)
#
ts_otherA = ts(VO$OtherUS, frequency = 52)
decompose_otherA = decompose(ts_otherA, "additive")
plot(as.ts(decompose_otherA$seasonal))
plot(as.ts(decompose_otherA$trend))
plot(as.ts(decompose_otherA$random))
plot(decompose_otherA)
#
VO$Other_random <- decompose_otherA$random

ts_trinycM = ts(VN$TriStateNYC, frequency = 52)
decompose_trinycM = decompose(ts_trinycM, "multiplicative")
plot(as.ts(decompose_trinycM$seasonal))
plot(as.ts(decompose_trinycM$trend))
plot(as.ts(decompose_trinycM$random))
plot(decompose_trinycM)
#
ts_trinycA = ts(VN$TriStateNYC, frequency = 52)
decompose_trinycA = decompose(ts_trinycA, "additive")
plot(as.ts(decompose_trinycA$seasonal))
plot(as.ts(decompose_trinycA$trend))
plot(as.ts(decompose_trinycA$random))
plot(decompose_trinycA)
#
VN$TriNYC_random <- decompose_trinycA$random











Intl_resids <- lm(International ~ `NYC:W`, data=VI)
VI$resids <- residuals(Intl_resids)

Other_resids <- lm(OtherUS ~ `NYC:US`, data=VO)
VO$resids <- residuals(Other_resids)

TriNYC_resids <- lm(TriStateNYC ~ `NYC:NYC`, data=VN)
VN$resids <- residuals(TriNYC_resids)

Intl2_resids <- lm(International ~ GA, data=VI2)
VI2$resids <- residuals(Intl2_resids)

Other2_resids <- lm(Other ~ GA, data=VO2)
VO2$resids <- residuals(Other2_resids)

NYC2_resids <- lm(NYC ~ GA, data=VN2)
VN2$resids <- residuals(NYC2_resids)




# ignore the below for now

# install.packages("dyn")
# library(dyn)
# model <- dyn$lm()

# install.packages("DataCombine")
# library(DataCombine)
# MR <- MR[nrow(MR)+1,]
# MR <- slide(MR, Var="Impression Reach", slideBy=1)

# cDF$Sales <- cDF$`Price per Paid Visitor` * cDF$`Total Visitors.y`
# spendCols <- grep("Spend", colnames(cDF))
# cDF$Spend <- rowSums(cDF[spendCols], na.rm = TRUE)
# cDF$Profit <- cDF$Sales - cDF$Spend
# cDF$ROI <- cDF$Profit/cDF$Spend
# 
# NYCtri <- ts(MR[[20]], start = c(2016,45), frequency = 52)

