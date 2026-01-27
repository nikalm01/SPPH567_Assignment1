##############################################
# This script reads, explores, and manipulates the
# PM2.5 data for SPPH567.
##############################################


### read the fentanyl data
fentanyldata <- read.csv(file = "Data/fentdatanewLOD.csv",
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
