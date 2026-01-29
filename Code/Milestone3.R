##############################################
# This script reads, explores, and manipulates the
# PM2.5 data for SPPH567.
##############################################


### read the fentanyl data
fentanyldata <- read.csv(file = "Data/fentdatanewLOD_Jan28 - ventilation fixed.csv",
                             header = T,
                             stringsAsFactors = F)

### create a fentanyl concentration variable in the dataframe
fentanyldata$logFent <- log(fentanyldata$Fentanyl_Concentration_ug_m3_TWA)

### convert Ventilation to type factor

fentanyldata$Ventilation <- factor(fentanyldata$Ventilation,
                       levels = c(0,1))

### convert Drug_Use_Observed to type factor
fentanyldata$Drug_Use_Observed <- factor(fentanyldata$Drug_Use_Observed,
                         levels = c(0, 1))

### make a boxplot of log-transformed Fentanyl concentrations vs. Ventilation

boxplot(fentanyldata$logFent ~ fentanyldata$Ventilation,
        ylab = "log(Fentanyl Concentrations (ug/m3))",
        xlab = "Ventilation status",
        main = "Ventilation vs. Log-Transformed Fentanyl Concentrations",
        col = c("blue", "orange"),
        las = 1)

boxplot(fentanyldata$logFent ~ fentanyldata$Drug_Use_Observed,
        ylab = "log(Fentanyl Concentrations (ug/m3))",
        xlab = "Drug use observed status",
        main = "Drug use observed vs. Log-Transformed Fentanyl Concentrations",
        col = c("blue", "orange"),
        las = 1)

### run a t-test and Welch's test on the log-transformed PM2.5
### concentrations vs. wind

t.test(fentanyldata$logFent ~ fentanyldata$Ventilation, var.equal = T)
t.test(fentanyldata$logFent ~ fentanyldata$Ventilation, var.equal = F)

t.test(fentanyldata$logFent ~ fentanyldata$Drug_Use_Observed, var.equal = T)
t.test(fentanyldata$logFent ~ fentanyldata$Drug_Use_Observed, var.equal = F)

### run a linear regression model with the log-transformed data
lm.ventilation <- lm(fentanyldata$logFent ~ fentanyldata$Ventilation)

lm.Drug_Use <- lm(fentanyldata$logFent ~ fentanyldata$Drug_Use_Observed)

### save models
lm.ventilation.summary <- summary(lm.ventilation)
lm.Drug_Use.summary <- summary(lm.Drug_Use)

lm.ventilation.intercept <- lm.ventilation.summary$coefficients[1,1]
lm.Drug_Use.intercept <- lm.Drug_Use.summary$coefficients[1,1]

lm.ventilation.slope <- lm.ventilation.summary$coefficients[2,1]
lm.Drug_Use.slope <- lm.Drug_Use.summary$coefficients[2,1]

## calculate geometric means for each dichotomous variable

geomeanVentOff <- exp(lm.ventilation.intercept)
geomeanVentOn <- exp(lm.ventilation.intercept + lm.ventilation.slope)

geomeanDrugUse0 <- exp(lm.Drug_Use.intercept)
geomeanDrugUse1 <- exp(lm.Drug_Use.intercept + lm.Drug_Use.slope)

### calculate risk ratios
VentRiskRatio <- geomeanVentOn/geomeanVentOff
DrugUseRiskRatio <- geomeanDrugUse1/geomeanDrugUse0

### Confidence intervals

VentLCI <- exp(lm.ventilation.slope - (1.96*lm.ventilation.summary$coefficients[2,2]))
VentUCI <- exp(lm.ventilation.slope + (1.96*lm.ventilation.summary$coefficients[2,2]))

DrugUseLCI <- exp(lm.Drug_Use.slope - (1.96*lm.Drug_Use.summary$coefficients[2,2]))
DrugUseUCI <- exp(lm.Drug_Use.slope + (1.96*lm.Drug_Use.summary$coefficients[2,2]))

### test variance

library(dplyr)

fentanylVentOn <- fentanyldata %>%
  filter(Ventilation == 1) %>%
  pull(Fentanyl_Concentration_ug_m3_TWA) %>%
  log()

fentanylVentOff <- fentanyldata %>%
  filter(Ventilation == 0) %>%
  pull(Fentanyl_Concentration_ug_m3_TWA) %>%
  log()

sVentOn <- var(fentanylVentOn)
sVentOff <- var(fentanylVentOff)

fentanylDrugUseYes <- fentanyldata %>%
  filter(Drug_Use_Observed == 1) %>%
  pull(Fentanyl_Concentration_ug_m3_TWA) %>%
  log()

fentanylDrugUseNo <- fentanyldata %>%
  filter(Drug_Use_Observed == 0) %>%
  pull(Fentanyl_Concentration_ug_m3_TWA) %>%
  log()

sDrugUseYes <- var(fentanylDrugUseYes)
sDrugUseNo <- var(fentanylDrugUseNo)
