setwd("C:\\Users\\sony\\Documents\\linear regression assignment")
carprice <- read.csv(file.choose())

#install_package("MASS")
#install_package("car")

library(MASS)
library(car)
library(dplyr)
library(tidyr)
library(ggplot2)

#view(carprice)
str(carprice)

#Data cleaning 

carprice <- separate(carprice,CarName,c('company','model'),sep='[[:space:]]',extra='merge',fill='right')
carprice$company[which(carprice$company=='maxda')]<- "mazda"
carprice$company[which(carprice$company=='porcshce')]<- "porsche"
carprice$company[which(carprice$company=='toyouta')]<- "toyota"
carprice$company[which(carprice$company=='vokswagen'|carprice$company=='vw')]<- "volkswagen"

carprice$company <- toupper(carprice$company)
colnames(carprice) <- tolower(colnames(carprice))

#removing the model column as it has not much significance as per the business requirement. 

carprice <- carprice[,-4]

View(carprice)
str(carprice)

#Now, we need to remove duplicate values (if any) in the dataset .

unique(carprice)

#No duplicate Values in the data set .

#Next important step is to check for missing values and treat if any. Use sum(is.na()) to check if there are any missing.

sum(is.na(carprice))

#Fortunately, there are no missing values in the dataset. so, let's move ahead

#Next step is to treat the outliers (if any). To check for outliers, we find out the quantile values at each 1% interval
# and wherever there is a high jump from one quantile to another, we cap/floor those values.



#Firstly, check if there are outliers in the variables one by one.

#1..carlength

quantile(carprice$carlength,seq(0,1,0.01))

#There is a sudden increase in carlength in 99 to 100 percentile , therefore replacing the outliers . 

carprice$carlength[which(carprice$carlength>202.480)] <- 202.480

#2..enginesize 

quantile(carprice$enginesize,seq(0,1,0.01))

#There is a sudden increase in the value from 96 to 97 percentile ,therefore replacing the outliers.

carprice$enginesize[which(carprice$enginesize>209.00)] <- 209.00


#3..horsepower 

quantile(carprice$horsepower,seq(0,1,0.01))

#There is a sudden change of valuess from 97 to 98 percentile , therefore handling the outliers . 

carprice$horsepower[which(carprice$horsepower>184.00)] <- 184.00

#4..peakrpm

quantile(carprice$peakrpm,seq(0,1,0.01))

#There is a sudden rise from 99 to 100 percentile , therefore handling the outliers . 

carprice$peakrpm[which(carprice$peakrpm>6000)] <- 6000

#5..citympg

quantile(carprice$citympg,seq(0,1,0.01))

#There is a sudden rise from 98 to 99 percentile , therefore handling the outliers . 

carprice$citympg[which(carprice$citympg>38.00)] <- 38.00


#6..highwaympg

quantile(carprice$highwaympg,seq(0,1,0.01))

#There is a sudden rise from 99 to 100 percentile , therefore handling the outliers . 

carprice$highwaympg[which(carprice$highwaympg>49.88)] <- 49.88

#write.csv(carprice,file = "cartab.csv",col.names = T)

#As per the Analaysis , these were the numeric attributes having outliers which is removed now . 

#Replacing "four" and "two" with 4 & 2 for door numbers 

levels(carprice$doornumber)<- c(4, 2)
carprice$doornumber <- as.numeric(levels(carprice$doornumber))[carprice$doornumber]

str(carprice)

#similarly replacing cylinder numbers 

levels(carprice$cylindernumber)<- c(8, 5, 4, 6, 3, 12, 2)
carprice$cylindernumber <- as.numeric(levels(carprice$cylindernumber))[carprice$cylindernumber]

#str(carprice)

# Replacing diesel "0" gas "1"
levels(carprice$fueltype)<- c(0, 1)
carprice$fueltype <- as.numeric(levels(carprice$fueltype))[carprice$fueltype]

#Replacing std "1" turbo "0"
levels(carprice$aspiration)<- c(1,0)
carprice$aspiration <- as.numeric(levels(carprice$aspiration))[carprice$aspiration]

#replacing front "1" rear "0"
levels(carprice$enginelocation)<- c(1,0)
carprice$enginelocation <- as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]

#str(carprice)

#Taking a copy of the carprice df and following the rest analysis on it ....

carprice1 <-carprice


#However, a general practice is to club the levels of a factor variable together to reduce the number of levels. 

#Reducing the number of levels by clubbing the fuelsystem 4bbl,mfi,spfi,spdi to a category others. 
#Since it is not having the right amount of values for correct prediction .

table(carprice1$fuelsystem)

levels(carprice1$fuelsystem)[levels(carprice1$fuelsystem) %in% c('spdi','4bbl','mfi','spfi')] <- 'others'

table(carprice1$enginetype)

#Reducing the number of levels by clubbing the enginetype dohcv,rotor to a category others

levels(carprice1$enginetype)[levels(carprice1$enginetype) %in% c('dohcv','rotor')] <- 'others'

table(carprice1$carbody)

#Reducing the number of levels by clubbing the carbody convertible ,hardtop to others .

levels(carprice1$carbody)[levels(carprice1$carbody) %in% c('convertible','hardtop') ] <- 'others'

str(carprice1)

#Converting the company variable to factors . 

carprice1$company <- as.factor(carprice1$company)

str(carprice1)

#The next step is to create dummy variables to convert the categorical variable radial_highways to
#numerical. Use model.matrix(~ <attribute_name> -1, data=housing) and store it in a variable dummy.

#..creating dummy variables for the categorical values..

dummy_drivewheel <- model.matrix(~drivewheel -1,data=carprice1)
carprice1 <- cbind(carprice1[,-which(colnames(carprice1)=='drivewheel')],dummy_drivewheel)

dummy_carbody <- model.matrix(~carbody -1,data=carprice1)
carprice1 <- cbind(carprice1[,-which(colnames(carprice1)=='carbody')],dummy_carbody)

dummy_engtyp <- model.matrix(~enginetype -1,data=carprice1)
carprice1 <- cbind(carprice1[,-which(colnames(carprice1)=='enginetype')],dummy_engtyp)

dummy_fuelsys <- model.matrix(~fuelsystem -1,data=carprice1)
carprice1 <- cbind(carprice1[,-which(colnames(carprice1)=='fuelsystem')],dummy_fuelsys)

dummy_company <- model.matrix(~company -1,data=carprice1)
carprice1 <- cbind(carprice1[,-which(colnames(carprice1)=='company')],dummy_company)


#View(carprice1)
#str(carprice1)

#------- MODELLING---------- #

#corr <-cor(carprice1)
#corr

set.seed(12345)

car.pop <- nrow(carprice)

train_indices <- sample(1:car.pop,0.7*car.pop)

#Making Training & Testing Data Set


train_id <- carprice1$car_id[train_indices]  #Fetching the train and test id's for plotting graphs
test_id <- carprice1$car_id[-train_indices]

carprice1 <- carprice1[,-which(colnames(carprice1)=='car_id')]  # removing the car_id column from df . 


train.carprice1 <-carprice1[train_indices,]
test.carprice1 <-carprice1[-train_indices,]


# model building: model1 - consisting of all variables

model <- lm(price~. ,data=train.carprice1)  

summary(model) #Adjusted R-squared:  0.9519

#using step AIC

step <- stepAIC(model,direction = 'both')

step


#Taking the last model call from the step function after the variables were reduced, and taking the
#remaining variables in another model ... model1

model1 <-lm(formula = price ~ symboling + fueltype + aspiration + enginelocation + 
              carwidth + curbweight + cylindernumber + enginesize + compressionratio + 
              horsepower + citympg + highwaympg + carbodyothers + enginetypeothers + 
              enginetypel + enginetypeohcf + companyAUDI + companyBMW + 
              companyBUICK + companyCHEVROLET + companyDODGE + companyHONDA + 
              companyJAGUAR + companyMAZDA + companyMERCURY + companyMITSUBISHI + 
              companyNISSAN + companyPEUGEOT + companyPLYMOUTH + companyPORSCHE + 
              companyRENAULT + companyTOYOTA + companyVOLKSWAGEN + carbodywagon, 
            data = train.carprice1)

summary(model1)   #Adjusted R-squared:  0.9575 

vif(model1)

#highwaympg vif =28.909464 p value 0.074134 (remove)

model2 <-lm(formula = price ~ symboling + fueltype + aspiration + enginelocation + 
              carwidth + curbweight + cylindernumber + enginesize + compressionratio + 
              horsepower + citympg + carbodyothers + enginetypeothers + 
              enginetypel + enginetypeohcf + companyAUDI + companyBMW + 
              companyBUICK + companyCHEVROLET + companyDODGE + companyHONDA + 
              companyJAGUAR + companyMAZDA + companyMERCURY + companyMITSUBISHI + 
              companyNISSAN + companyPEUGEOT + companyPLYMOUTH + companyPORSCHE + 
              companyRENAULT + companyTOYOTA + companyVOLKSWAGEN + carbodywagon, 
            data = train.carprice1)

summary(model2)   #Adjusted R-squared:  0.9566 

#vif(model2)
sort(vif(model2),decreasing = T)

#enginetypel p value 0.053990 vif= 19.537261 (remove)

model3 <-lm(formula = price ~ symboling + fueltype + aspiration + enginelocation + 
              carwidth + curbweight + cylindernumber + enginesize + compressionratio + 
              horsepower + citympg + carbodyothers + enginetypeothers 
            + enginetypeohcf + companyAUDI + companyBMW + 
              companyBUICK + companyCHEVROLET + companyDODGE + companyHONDA + 
              companyJAGUAR + companyMAZDA + companyMERCURY + companyMITSUBISHI + 
              companyNISSAN + companyPEUGEOT + companyPLYMOUTH + companyPORSCHE + 
              companyRENAULT + companyTOYOTA + companyVOLKSWAGEN + carbodywagon, 
            data = train.carprice1)

summary(model3)   #Adjusted R-squared:  0.9555 

#vif(model3)
sort(vif(model3),decreasing = T)

# citympg vif=12.203707 p value 0.064414 (remove)

model4 <-lm(formula = price ~ symboling + fueltype + aspiration + enginelocation + 
              carwidth + curbweight + cylindernumber + enginesize + compressionratio + 
              horsepower + carbodyothers + enginetypeothers 
            + enginetypeohcf + companyAUDI + companyBMW + 
              companyBUICK + companyCHEVROLET + companyDODGE + companyHONDA + 
              companyJAGUAR + companyMAZDA + companyMERCURY + companyMITSUBISHI + 
              companyNISSAN + companyPEUGEOT + companyPLYMOUTH + companyPORSCHE + 
              companyRENAULT + companyTOYOTA + companyVOLKSWAGEN + carbodywagon, 
            data = train.carprice1)

summary(model4)   #Adjusted R-squared:  0.9545 

#vif(model4)
sort(vif(model4),decreasing = T)

#aspiration pvalue 0.090805 ,vif=5.017197  (remove)

model5 <-lm(formula = price ~ symboling + fueltype + enginelocation + 
              carwidth + curbweight + cylindernumber + enginesize + compressionratio + 
              horsepower + carbodyothers + enginetypeothers 
            + enginetypeohcf + companyAUDI + companyBMW + 
              companyBUICK + companyCHEVROLET + companyDODGE + companyHONDA + 
              companyJAGUAR + companyMAZDA + companyMERCURY + companyMITSUBISHI + 
              companyNISSAN + companyPEUGEOT + companyPLYMOUTH + companyPORSCHE + 
              companyRENAULT + companyTOYOTA + companyVOLKSWAGEN + carbodywagon, 
            data = train.carprice1)

summary(model5)   #Adjusted R-squared:  0.9538 

#vif(model5)
sort(vif(model5),decreasing = T)

#carbodywagon pvalue 0.500993  (remove)

model6 <-lm(formula = price ~ symboling + fueltype + enginelocation + 
              carwidth + curbweight + cylindernumber + enginesize + compressionratio + 
              horsepower + carbodyothers + enginetypeothers 
            + enginetypeohcf + companyAUDI + companyBMW + 
              companyBUICK + companyCHEVROLET + companyDODGE + companyHONDA + 
              companyJAGUAR + companyMAZDA + companyMERCURY + companyMITSUBISHI + 
              companyNISSAN + companyPEUGEOT + companyPLYMOUTH + companyPORSCHE + 
              companyRENAULT + companyTOYOTA + companyVOLKSWAGEN , 
            data = train.carprice1)

summary(model6)   #Adjusted R-squared:  0.954 

#vif(model6)
sort(vif(model6),decreasing = T)

#companyCHEVROLET pvalue 0.478555 (remove)

model7 <-lm(formula = price ~ symboling + fueltype + enginelocation + 
              carwidth + curbweight + cylindernumber + enginesize + compressionratio + 
              horsepower + carbodyothers + enginetypeothers 
            + enginetypeohcf + companyAUDI + companyBMW + 
              companyBUICK + companyDODGE + companyHONDA + 
              companyJAGUAR + companyMAZDA + companyMERCURY + companyMITSUBISHI + 
              companyNISSAN + companyPEUGEOT + companyPLYMOUTH + companyPORSCHE + 
              companyRENAULT + companyTOYOTA + companyVOLKSWAGEN , 
            data = train.carprice1)

summary(model7)   #Adjusted R-squared:  0.9542 

#vif(model7)
sort(vif(model7),decreasing = T)

#symboling pvalue 0.209224  (remove)

model8 <-lm(formula = price ~ fueltype + enginelocation + 
              carwidth + curbweight + cylindernumber + enginesize + compressionratio + 
              horsepower + carbodyothers + enginetypeothers 
            + enginetypeohcf + companyAUDI + companyBMW + 
              companyBUICK + companyDODGE + companyHONDA + 
              companyJAGUAR + companyMAZDA + companyMERCURY + companyMITSUBISHI + 
              companyNISSAN + companyPEUGEOT + companyPLYMOUTH + companyPORSCHE + 
              companyRENAULT + companyTOYOTA + companyVOLKSWAGEN , 
            data = train.carprice1)

summary(model8)   #Adjusted R-squared:  0.954 

#vif(model8)
sort(vif(model8),decreasing = T)

#Comapny audi pvalue 0.079967   (remove)

model9 <-lm(formula = price ~ fueltype + enginelocation + 
              carwidth + curbweight + cylindernumber + enginesize + compressionratio + 
              horsepower + carbodyothers + enginetypeothers 
            + enginetypeohcf+ companyBMW + 
              companyBUICK + companyDODGE + companyHONDA + 
              companyJAGUAR + companyMAZDA + companyMERCURY + companyMITSUBISHI + 
              companyNISSAN + companyPEUGEOT + companyPLYMOUTH + companyPORSCHE + 
              companyRENAULT + companyTOYOTA + companyVOLKSWAGEN , 
            data = train.carprice1)

summary(model9)   #Adjusted R-squared:  0.9531 

#vif(model9)
sort(vif(model9),decreasing = T)

#fuel type vip  74.208489 pvalue 0.074  (remove)

model10 <-lm(formula = price ~ enginelocation + 
               carwidth + curbweight + cylindernumber + enginesize + compressionratio + 
               horsepower + carbodyothers + enginetypeothers 
             + enginetypeohcf+ companyBMW + 
               companyBUICK + companyDODGE + companyHONDA + 
               companyJAGUAR + companyMAZDA + companyMERCURY + companyMITSUBISHI + 
               companyNISSAN + companyPEUGEOT + companyPLYMOUTH + companyPORSCHE + 
               companyRENAULT + companyTOYOTA + companyVOLKSWAGEN , 
             data = train.carprice1)

summary(model10)   #Adjusted R-squared:  0.9522 

#vif(model10)
sort(vif(model10),decreasing = T)

#compressionratio pvalue 0.570498  (remove)

model11 <-lm(formula = price ~ enginelocation + 
               carwidth + curbweight + cylindernumber + enginesize  + 
               horsepower + carbodyothers + enginetypeothers 
             + enginetypeohcf+ companyBMW + 
               companyBUICK + companyDODGE + companyHONDA + 
               companyJAGUAR + companyMAZDA + companyMERCURY + companyMITSUBISHI + 
               companyNISSAN + companyPEUGEOT + companyPLYMOUTH + companyPORSCHE + 
               companyRENAULT + companyTOYOTA + companyVOLKSWAGEN , 
             data = train.carprice1)

summary(model11)   #Adjusted R-squared:  0.9525 

#vif(model11)
sort(vif(model11),decreasing = T)

#enginesize vif 12.640704 pvalue 0.053387  (remove)

model12 <-lm(formula = price ~ enginelocation + 
               carwidth + curbweight + cylindernumber  + 
               horsepower + carbodyothers + enginetypeothers 
             + enginetypeohcf+ companyBMW + 
               companyBUICK + companyDODGE + companyHONDA + 
               companyJAGUAR + companyMAZDA + companyMERCURY + companyMITSUBISHI + 
               companyNISSAN + companyPEUGEOT + companyPLYMOUTH + companyPORSCHE + 
               companyRENAULT + companyTOYOTA + companyVOLKSWAGEN , 
             data = train.carprice1)

summary(model12)   #Adjusted R-squared:  0.9514 

#vif(model12)
sort(vif(model12),decreasing = T)

#companyMERCURY pvalue 0.187010  (remove)

model13 <-lm(formula = price ~ enginelocation + 
               carwidth + curbweight + cylindernumber  + 
               horsepower + carbodyothers + enginetypeothers 
             + enginetypeohcf+ companyBMW + 
               companyBUICK + companyDODGE + companyHONDA + 
               companyJAGUAR + companyMAZDA + companyMITSUBISHI + 
               companyNISSAN + companyPEUGEOT + companyPLYMOUTH + companyPORSCHE + 
               companyRENAULT + companyTOYOTA + companyVOLKSWAGEN , 
             data = train.carprice1)

summary(model13)   #Adjusted R-squared:  0.9511 

#vif(model13)
sort(vif(model13),decreasing = T)
 
#companyVOLKSWAGEN pvalue 0.085299  (remove)

model14 <-lm(formula = price ~ enginelocation + 
               carwidth + curbweight + cylindernumber  + 
               horsepower + carbodyothers + enginetypeothers 
             + enginetypeohcf+ companyBMW + 
               companyBUICK + companyDODGE + companyHONDA + 
               companyJAGUAR + companyMAZDA + companyMITSUBISHI + 
               companyNISSAN + companyPEUGEOT + companyPLYMOUTH + companyPORSCHE + 
               companyRENAULT + companyTOYOTA, 
             data = train.carprice1)

summary(model14)   #Adjusted R-squared:  0.9502 

#vif(model14)
sort(vif(model14),decreasing = T)

#companyHONDA pvalue 0.175306  (remove)

model15 <-lm(formula = price ~ enginelocation + 
               carwidth + curbweight + cylindernumber  + 
               horsepower + carbodyothers + enginetypeothers 
             + enginetypeohcf+ companyBMW + 
               companyBUICK + companyDODGE+ 
               companyJAGUAR + companyMAZDA + companyMITSUBISHI + 
               companyNISSAN + companyPEUGEOT + companyPLYMOUTH + companyPORSCHE + 
               companyRENAULT + companyTOYOTA, 
             data = train.carprice1)

summary(model15)   #Adjusted R-squared:  0.9499 

vif(model15)
sort(vif(model15),decreasing = T)

#companyPEUGEOT pvalue 0.108803   (remove)

model16 <-lm(formula = price ~ enginelocation + 
               carwidth + curbweight + cylindernumber  + 
               horsepower + carbodyothers + enginetypeothers 
             + enginetypeohcf+ companyBMW + 
               companyBUICK + companyDODGE+ 
               companyJAGUAR + companyMAZDA + companyMITSUBISHI + 
               companyNISSAN + companyPLYMOUTH + companyPORSCHE + 
               companyRENAULT + companyTOYOTA, 
             data = train.carprice1)

summary(model16)   #Adjusted R-squared:  0.9492 

#vif(model16)
sort(vif(model16),decreasing = T)

#companyMAZDA pvalue 0.119410   (remove)
model17 <-lm(formula = price ~ enginelocation + 
               carwidth + curbweight + cylindernumber  + 
               horsepower + carbodyothers + enginetypeothers 
             + enginetypeohcf+ companyBMW + 
               companyBUICK + companyDODGE+ 
               companyJAGUAR + companyMITSUBISHI + 
               companyNISSAN + companyPLYMOUTH + companyPORSCHE + 
               companyRENAULT + companyTOYOTA, 
             data = train.carprice1)

summary(model17)   #Adjusted R-squared:  0.9486 

#vif(model17)
sort(vif(model17),decreasing = T)

#companyNISSAN pvalue 0.067025    (remove)

model18 <-lm(formula = price ~ enginelocation + 
               carwidth + curbweight + cylindernumber  + 
               horsepower + carbodyothers + enginetypeothers 
             + enginetypeohcf+ companyBMW + 
               companyBUICK + companyDODGE+ 
               companyJAGUAR + companyMITSUBISHI 
             + companyPLYMOUTH + companyPORSCHE + 
               companyRENAULT + companyTOYOTA, 
             data = train.carprice1)

summary(model18)   #Adjusted R-squared:  0.9477 

#vif(model18)
sort(vif(model18),decreasing = T)

#companyDODGE pvalue 0.214778     (remove)
model19 <-lm(formula = price ~ enginelocation + 
               carwidth + curbweight + cylindernumber  + 
               horsepower + carbodyothers + enginetypeothers 
             + enginetypeohcf+ companyBMW + 
               companyBUICK + 
               companyJAGUAR + companyMITSUBISHI 
             + companyPLYMOUTH + companyPORSCHE + 
               companyRENAULT + companyTOYOTA, 
             data = train.carprice1)

summary(model19)   #Adjusted R-squared:  0.9474 

#vif(model19)
sort(vif(model19),decreasing = T)


#companyPLYMOUTH pvalue 0.199531    (remove)

model20 <-lm(formula = price ~ enginelocation + 
               carwidth + curbweight + cylindernumber  + 
               horsepower + carbodyothers + enginetypeothers 
             + enginetypeohcf+ companyBMW + 
               companyBUICK + 
               companyJAGUAR + companyMITSUBISHI 
             + companyPORSCHE + 
               companyRENAULT + companyTOYOTA, 
             data = train.carprice1)

summary(model20)   #Adjusted R-squared:  0.9471 

#vif(model20)
sort(vif(model20),decreasing = T)

#companyRENAULT pvalue 0.119456   (remove)

model21 <-lm(formula = price ~ enginelocation + 
               carwidth + curbweight + cylindernumber  + 
               horsepower + carbodyothers + enginetypeothers 
             + enginetypeohcf+ companyBMW + companyBUICK + 
               companyJAGUAR + companyMITSUBISHI 
             + companyPORSCHE  
             + companyTOYOTA, 
             data = train.carprice1)

summary(model21)   #Adjusted R-squared:  0.9465 

#vif(model21)
sort(vif(model21),decreasing = T)

#enginetypeohcf pvalue 0.076518   (remove)

model22 <-lm(formula = price ~ enginelocation + 
               carwidth + curbweight + cylindernumber + 
               horsepower + carbodyothers + enginetypeothers +
               companyBMW + companyBUICK + companyJAGUAR + companyMITSUBISHI +
               companyPORSCHE+ companyTOYOTA, 
             data = train.carprice1)

summary(model22)   #Adjusted R-squared:  0.9456 

vif(model22)
sort(vif(model22),decreasing = T)

#companyMITSUBISHI cor(carprice1$companyMITSUBISHI,carprice1$price) = -0.131 pvalue  0.037023 *   (remove)

model23 <-lm(formula = price ~ enginelocation + 
               carwidth + curbweight + cylindernumber + 
               horsepower + carbodyothers + enginetypeothers +
               companyBMW + companyBUICK + companyJAGUAR  +
               companyPORSCHE+ companyTOYOTA, 
             data = train.carprice1)

summary(model23)   #Adjusted R-squared:  0.9442 

#vif(model23)
sort(vif(model23),decreasing = T)

#companyTOYOTA pvalue 0.040053 *    (remove)

model24 <-lm(formula = price ~ enginelocation + 
               carwidth + curbweight + cylindernumber + 
               horsepower + carbodyothers + enginetypeothers +
               companyBMW + companyBUICK + companyJAGUAR  +
               companyPORSCHE, 
             data = train.carprice1)

summary(model24)   #Adjusted R-squared:  0.9428 

#vif(model24)
sort(vif(model24),decreasing = T) 

#enginetypeothers pvalue 0.006560     (remove)
model25 <-lm(formula = price ~ enginelocation + 
               carwidth + curbweight + cylindernumber + 
               horsepower + carbodyothers+
               companyBMW + companyBUICK + companyJAGUAR  +
               companyPORSCHE, 
             data = train.carprice1)

summary(model25)   #Adjusted R-squared:  0.9399 

#vif(model25)
sort(vif(model25),decreasing = T)

#cylindernumber pvalue 0.057738     (remove)

model26 <-lm(formula = price ~ enginelocation + 
               carwidth + curbweight+ 
               horsepower+
               companyBMW + companyBUICK + companyJAGUAR  +
               companyPORSCHE, 
             data = train.carprice1)

summary(model26)   #Adjusted R-squared:  0.9352 

#vif(model26)
sort(vif(model26),decreasing = T)

#curbweight pvalue 0.001879 **    (remove)

model27 <-lm(formula = price ~ enginelocation + 
               carwidth+ 
               horsepower+
               companyBMW + companyBUICK + companyJAGUAR  +
               companyPORSCHE, 
             data = train.carprice1)

summary(model27)   #Adjusted R-squared:  0.9308 

#vif(model27)
sort(vif(model27),decreasing = T)

#companyPORSCHE pvalue  0.00133 **    (remove)
model28 <-lm(formula = price ~ enginelocation + 
               carwidth+ 
               horsepower+
               companyBMW + companyBUICK + companyJAGUAR, 
             data = train.carprice1)

summary(model28)   #Adjusted R-squared:  0.9259 

#vif(model28)
sort(vif(model28),decreasing = T)

#FINAL MODEL IS "MODEL28" having 5 driving variables having significance pvalue less than 0.05 and less VIF .
#Testing the model on test data . 

#prediction of car price in test data set 

predicted_price <- predict(model28, test.carprice1[,-which(colnames(test.carprice1)=='price')])
test.carprice1$test_price <- predicted_price

# Now, we need to test the r square between actual and predicted sales. 

r <- cor(test.carprice1$price,test.carprice1$test_price)
rsquared <- cor(test.carprice1$price,test.carprice1$test_price)^2
rsquared  

#r squared value for is "0.8735" which is comparable to the calculated value of adjsuted r square . 

#plotting the graph

# residual plot
residualPlot(model28)

#Observe that the errors (the differences between the actual values and the values predicted by the model)
#are randomly distributed. What this essentially confirms is that there are no variables that could have
#helped explain the model better

plotting_data <- data.frame(car_id=test_id, actual=test.carprice1$price, predicted=test.carprice1$test_price)
plotting_data$residuals <- plotting_data$actual - plotting_data$predicted

# plotting actual and residual values

#View(plotting_data)
ggplot(plotting_data, aes(actual, residuals)) +geom_point() +geom_smooth() 

# As price increases residual increases . 

#****************************
#     DRIVER VARIABLES      *
#                           *
#    1. HORSEPOWER          *
#    2. CARWIDTH            *
#    3. ENGINELOCATION      * 
#    4. COMPANYBUICK        *
#    5. COMPANY JAGUAR      * 
#    6. COMPANY BMW         *
#*****************************

cor(train.carprice1$horsepower,train.carprice1$price)   #...0.81
cor(train.carprice1$carwidth,train.carprice1$price)   #...0.73
cor(train.carprice1$enginelocation,train.carprice1$price) #...-0.37
cor(train.carprice1$companyBUICK,train.carprice1$price)   #...0.51
cor(train.carprice1$companyJAGUAR,train.carprice1$price)   #...0.37
cor(train.carprice1$companyBMW,train.carprice1$price)   #...0.81

#These are the following variables , derived from the final model .
#"Horse-power" is higly positively correlated if horse power increases price of the car also increases.
#"Engine location" is negatively correlated , so with engine location preference price varies . 
#"carwidth" is positively correlated , therefore with inc in carwidth price increases . 
#"companyBUICK", "companyJAGUAR", "companyBMV" are affecting the price and are positively correlated .   


