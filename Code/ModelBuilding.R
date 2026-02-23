### Load data and factor all dichotoous and categorical
### variables

load("Data/fentanyldata.rdata")

fentanyldata$Site <- as.character(fentanyldata$Site)

fentanyldata$SiteType <- replace(fentanyldata$Site,
                                 fentanyldata$Site %in% c("D","B"),
                                 "Abstinance")

fentanyldata$SiteType <- ifelse(fentanyldata$SiteType %in% c("C","A"),
                                "Overdose Prevention",
                                fentanyldata$SiteType)

fentanyldata$SiteType <- ifelse(fentanyldata$SiteType == "E",
                                "Non-HOPS",
                                fentanyldata$SiteType)

#set the levels of SourceSeason 
fentanyldata$SiteType <- factor(fentanyldata$SiteType,
                                levels = c("Abstinance", "Non-HOPS", "Overdose Prevention"))

### reduce to the variables of interest, which are climatic predictors

fentanyldata_q.sub1 <- fentanyldata[ , c("Fentanyl_Concentration_ug_m3_TWA", "logFent", "SiteType", "Room_Type", "Temperature_C", 
                      "Ventilation", "Drug_Use_Observed")]

### evaluate the relationship between logPM25 and all independent 
### variables

# SiteType
boxplot(logFent~SiteType, data = fentanyldata_q.sub1)
summary(aov(logFent~SiteType, data = fentanyldata_q.sub1))
summary(lm(logFent~SiteType, data = fentanyldata_q.sub1))

# Room_Type
boxplot(logFent~Room_Type, data = fentanyldata_q.sub1)
summary(aov(logFent~Room_Type, data = fentanyldata_q.sub1))
summary(lm(logFent~Room_Type, data = fentanyldata_q.sub1))

# Temperature

plot(logFent~Temperature_C, data = fentanyldata_q.sub1)
cor.test(fentanyldata_q.sub1$logFent, fentanyldata_q.sub1$Temperature_C, method = "pearson")
summary(lm(logFent~Temperature_C, data = fentanyldata_q.sub1)) 

# Ventilation
boxplot(logFent~Ventilation, data = fentanyldata_q.sub1)
var.test(logFent~Ventilation, data = fentanyldata_q.sub1)
t.test(logFent~Ventilation, data = fentanyldata_q.sub1, var.equal = T)
summary(lm(logFent~Ventilation, data = fentanyldata_q.sub1)) 

# Drug Use Observed
boxplot(logFent~Drug_Use_Observed, data = fentanyldata_q.sub1)
var.test(logFent~Drug_Use_Observed, data = fentanyldata_q.sub1)
t.test(logFent~Drug_Use_Observed, data = fentanyldata_q.sub1, var.equal = T)
summary(lm(logFent~Drug_Use_Observed, data = fentanyldata_q.sub1)) 


### stat with a model that has all variables

all <- lm(logFent ~ SiteType + Room_Type + Temperature_C + Ventilation + Drug_Use_Observed,
          data = fentanyldata_q.sub1)
summary(all)

no_ventilation <- lm(logFent ~ SiteType + Room_Type + Temperature_C + Drug_Use_Observed,
          data = fentanyldata_q.sub1)
summary(no_ventilation)

no_temperature <- lm(logFent ~ SiteType + Room_Type + Ventilation + Drug_Use_Observed,
                     data = fentanyldata_q.sub1)
summary(no_temperature)

no_temp_vent <- lm(logFent ~ SiteType + Room_Type + Drug_Use_Observed,
                   data = fentanyldata_q.sub1)
summary(no_temp_vent)



##### Just sites with drug consumption


fentanyldata_SiteConsumption <- subset(fentanyldata, SiteType %in% c("Non-HOPS", "Overdose Prevention") )


fentanyldata_SiteConsumption <- fentanyldata_SiteConsumption[ , c("Fentanyl_Concentration_ug_m3_TWA", "logFent", "SiteType", "Room_Type", "Temperature_C", 
                                         "Ventilation", "Drug_Use_Observed")]


### evaluate the relationship between logPM25 and all independent 
### variables

# SiteType
boxplot(logFent~SiteType, data = fentanyldata_SiteConsumption)
summary(aov(logFent~SiteType, data = fentanyldata_SiteConsumption))
summary(lm(logFent~SiteType, data = fentanyldata_SiteConsumption))

# Room_Type
boxplot(logFent~Room_Type, data = fentanyldata_SiteConsumption)
summary(aov(logFent~Room_Type, data = fentanyldata_SiteConsumption))
summary(lm(logFent~Room_Type, data = fentanyldata_SiteConsumption))

# Temperature

plot(logFent~Temperature_C, data = fentanyldata_SiteConsumption)
cor.test(fentanyldata_SiteConsumption$logFent, fentanyldata_SiteConsumption$Temperature_C, method = "pearson")
summary(lm(logFent~Temperature_C, data = fentanyldata_SiteConsumption)) 

# Ventilation
boxplot(logFent~Ventilation, data = fentanyldata_SiteConsumption)
var.test(logFent~Ventilation, data = fentanyldata_SiteConsumption)
t.test(logFent~Ventilation, data = fentanyldata_SiteConsumption, var.equal = T)
summary(lm(logFent~Ventilation, data = fentanyldata_SiteConsumption)) 

# Drug Use Observed
boxplot(logFent~Drug_Use_Observed, data = fentanyldata_SiteConsumption)
var.test(logFent~Drug_Use_Observed, data = fentanyldata_SiteConsumption)
t.test(logFent~Drug_Use_Observed, data = fentanyldata_SiteConsumption, var.equal = T)
summary(lm(logFent~Drug_Use_Observed, data = fentanyldata_SiteConsumption)) 


### start with a model that has all variables

all <- lm(logFent ~ SiteType + Room_Type + Temperature_C + Ventilation + Drug_Use_Observed,
          data = fentanyldata_SiteConsumption)
summary(all)

no_vent <- lm(logFent ~ SiteType + Room_Type + Temperature_C  + Drug_Use_Observed,
          data = fentanyldata_SiteConsumption)
summary(no_vent)

no_vent_temp <- lm(logFent ~ SiteType + Room_Type  + Drug_Use_Observed,
              data = fentanyldata_SiteConsumption)
summary(no_vent_temp)


### Final variable looking at combined ventilation and drug use

fentanyldata$Ventilation <- as.numeric(as.character(fentanyldata$Ventilation))
fentanyldata$Drug_Use_Observed <- as.numeric(as.character(fentanyldata$Drug_Use_Observed))

fentanyldata$Vent_with_Drug_Use <-  as.numeric(fentanyldata$Ventilation & fentanyldata$Drug_Use_Observed)
fentanyldata$Vent_with_Drug_Use <- factor(fentanyldata$Vent_with_Drug_Use)

fentanyldata$Vent_no_Drug_Use <-  as.numeric(fentanyldata$Ventilation & !fentanyldata$Drug_Use_Observed)
fentanyldata$Vent_no_Drug_Use <- factor(fentanyldata$Vent_no_Drug_Use)

fentanyldata$No_vent_no_Drug_Use <-  as.numeric(!fentanyldata$Ventilation & !fentanyldata$Drug_Use_Observed)
fentanyldata$No_vent_no_Drug_Use <- factor(fentanyldata$No_vent_no_Drug_Use)

fentanyldata$No_vent_with_Drug_Use <-  as.numeric(!fentanyldata$Ventilation & fentanyldata$Drug_Use_Observed)
fentanyldata$No_vent_with_Drug_Use <- factor(fentanyldata$No_vent_with_Drug_Use)


## Just look at when drug use is observed, for curiosity

library(dplyr)

fentanyldata_DUO <- fentanyldata %>%
  filter(Drug_Use_Observed == 1)

fentanyldata_DUO$Ventilation <- factor(fentanyldata_DUO$Ventilation)
# SiteType
boxplot(logFent~SiteType, data = fentanyldata_DUO)
summary(aov(logFent~SiteType, data = fentanyldata_DUO))
summary(lm(logFent~SiteType, data = fentanyldata_DUO))

# Room_Type
boxplot(logFent~Room_Type, data = fentanyldata_DUO)
summary(aov(logFent~Room_Type, data = fentanyldata_DUO))
summary(lm(logFent~Room_Type, data = fentanyldata_DUO))

# Temperature

plot(logFent~Temperature_C, data = fentanyldata_DUO)
cor.test(fentanyldata_DUO$logFent, fentanyldata_DUO$Temperature_C, method = "pearson")
summary(lm(logFent~Temperature_C, data = fentanyldata_DUO)) 

# Ventilation
boxplot(logFent~Ventilation, data = fentanyldata_DUO)
var.test(logFent~Ventilation, data = fentanyldata_DUO)
t.test(logFent~Ventilation, data = fentanyldata_DUO, var.equal = T)
summary(lm(logFent~Ventilation, data = fentanyldata_DUO)) 


all_DUO <- lm(logFent ~ SiteType + Room_Type + Temperature_C + Ventilation,
          data = fentanyldata_DUO)
summary(all_DUO)

### JUST PLAYING AROUND 
### Test relationships between new variables

# Ventilation with drug use
boxplot(logFent~Vent_with_Drug_Use, data = fentanyldata)
var.test(logFent~Vent_with_Drug_Use, data = fentanyldata)
t.test(logFent~Vent_with_Drug_Use, data = fentanyldata, var.equal = T)
summary(lm(logFent~Vent_with_Drug_Use, data = fentanyldata)) 

# Ventilation with no drug use
boxplot(logFent~Vent_no_Drug_Use, data = fentanyldata)
var.test(logFent~Vent_no_Drug_Use, data = fentanyldata)
t.test(logFent~Vent_no_Drug_Use, data = fentanyldata, var.equal = T)
summary(lm(logFent~Vent_no_Drug_Use, data = fentanyldata)) 

# No ventilation with no drug use
boxplot(logFent~No_vent_no_Drug_Use, data = fentanyldata)
var.test(logFent~No_vent_no_Drug_Use, data = fentanyldata)
t.test(logFent~No_vent_no_Drug_Use, data = fentanyldata, var.equal = T)
summary(lm(logFent~No_vent_no_Drug_Use, data = fentanyldata))

# No ventilation with  drug use
boxplot(logFent~No_vent_with_Drug_Use, data = fentanyldata)
var.test(logFent~No_vent_with_Drug_Use, data = fentanyldata)
t.test(logFent~No_vent_with_Drug_Use, data = fentanyldata, var.equal = T)
summary(lm(logFent~No_vent_with_Drug_Use, data = fentanyldata))

fentanyldata_Vent_DrugUse <- fentanyldata[ , c("Fentanyl_Concentration_ug_m3_TWA", "logFent", "SiteType", "Room_Type", "Temperature_C", 
                                                                  "No_vent_no_Drug_Use", "Vent_no_Drug_Use", "No_vent_with_Drug_Use", "Vent_with_Drug_Use")]

### start with a model that has all variables

all <- lm(logFent ~ SiteType + Room_Type + Temperature_C + No_vent_no_Drug_Use + Vent_no_Drug_Use + No_vent_with_Drug_Use + Vent_with_Drug_Use,
          data = fentanyldata_Vent_DrugUse)
summary(all)
