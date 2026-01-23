##############################################
# This script reads and explores the fentanyl 
# exposure data for Milestone 2
##############################################


### read the airborne fentanyl data
### Note that the data is kept in the data folder; update 
### the file parameter as the datasets are updated

fentanyldataNoNA <- read.csv(file = "Data/Fentanyl_Data_Jan20_NM.csv",
                         header = T,
                         stringsAsFactors = F)

### summarize airborne fentanyl values

#summary(fentanyldata$Fentanyl_ng_m3)
#mean(fentanyldata$Fentanyl_ng_m3, na.rm = T)

data <- fentanyldataNoNA$Fentanyl_Concentration_ug_m3_TWA

summary(data)
mean(data)

### create an object with the <LOD values replaced with 4/sqrt(2)

#LOD <- 6.903

# make a new object with the updated values in it

#lodOverRoot2 <- replace(fentanyldata$Fentanyl_ng_m3,
                       # list = is.na(fentanyldata$Fentanyl_ng_m3),
                       # values = LOD/sqrt(2))

#check summary of the updated data
#summary(lodOverRoot2)

### calculate the arithmetic mean, standard deviation, median and 95th percentile of the untransformed PM2.5 values


### log-transform the lodOverRoot2 values and calculate summaries

#lodOverRoot2log <- log(lodOverRoot2)
logFentanylConcNoNA <- log(data)

# calculate arithmetic mean, sd, median, and 95th percentile of the log-transformed PM2.5 values
mean(logFentanylConcNoNA)
sd(logFentanylConcNoNA)
median(logFentanylConcNoNA)
quantile(logFentanylConcNoNA, 0.95)

# calculate the geometric mean, sd, median, and 95th percentile of the log-transformed PM2.5 values

exp(mean(logFentanylConcNoNA))
exp(sd(logFentanylConcNoNA))
exp(median(logFentanylConcNoNA))
exp(quantile(logFentanylConcNoNA, 0.95))

### Create density plots of the fentanyl exposure data

# load ggplot2
library(ggplot2)

# convert log-transformed and imputed fentanyl exposure data to a dataframe
# for ggplot2
logdFentanylConcdf <- data.frame(value = logFentanylConcNoNA)

# create density plot with ggplot2
log_density <- ggplot(logdFentanylConcdf, aes(x=value)) + 
  geom_density()+
  labs(title="Density curve of log-transformed airborne fentanyl exposure data",
       x="Airborne fentanyl concentration (ug/m^3)", y = "Density") +
  theme(plot.title = element_text(
    hjust = 0.5, 
    size = 10))
log_density


# create density plot with ggplot2
density <- ggplot(fentanyldataNoNA, aes(x=Fentanyl_Concentration_ug_m3_TWA)) + 
  geom_density() +
  labs(title="Density curve of airborne fentanyl exposure",
       x="Airborne fentanyl concentration (ug/m^3)", y = "Density")+
  theme(plot.title = element_text(
    hjust = 0.5, 
    size = 10))
density


# create density plot for temperature with ggplot2
temperature_density <- ggplot(fentanyldataNoNA, aes(x=Temperature_C)) + 
  geom_density() +
  labs(title="Density curve of temperature",
       x="Temperature (degrees Celcius)", y = "Density")+
  theme(plot.title = element_text(
    hjust = 0.5, 
    size = 12))
temperature_density

### Hi world

### Hi Hallie, let's practice pulling!\


### Hi!! pratice push!


