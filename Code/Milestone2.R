##############################################
# This script reads and explores the fentanyl 
# exposure data for Milestone 2
##############################################


### read the airborne fentanyl data
### Note that the data is kept in the data folder; update 
### the file parameter as the datasets are updated
fentanyldata <- read.csv(file = "Data/synthetic_fentanyl_exposure_dataset.csv",
                     header = T,
                     stringsAsFactors = F)

### summarize airborne fentanyl values

summary(fentanyldata$Fentanyl_ng_m3)
mean(fentanyldata$Fentanyl_ng_m3, na.rm = T)

### create an object with the <LOD values replaced with 4/sqrt(2)

LOD <- 6.903

# make a new object with the updated values in it

lodOverRoot2 <- replace(fentanyldata$Fentanyl_ng_m3,
                        list = is.na(fentanyldata$Fentanyl_ng_m3),
                        values = LOD/sqrt(2))

#check summary of the updated data
summary(lodOverRoot2)

### calculate the arithmetic mean, standard deviation, median and 95th percentile of the untransformed PM2.5 values

mean(lodOverRoot2)
sd(lodOverRoot2)
median(lodOverRoot2)
quantile(lodOverRoot2, 0.95)

### log-transform the lodOverRoot2 val;ues and calculate summaries

lodOverRoot2log <- log(lodOverRoot2)

# calculate arithmetic mean, sd, median, and 95th percentile of the log-transformed PM2.5 values
mean(lodOverRoot2log)
sd(lodOverRoot2log)
median(lodOverRoot2log)
quantile(lodOverRoot2log, 0.95)

# calculate the geometric mean, sd, median, and 95th percentile of the log-transformed PM2.5 values

exp(mean(lodOverRoot2log))
exp(sd(lodOverRoot2log))
exp(median(lodOverRoot2log))
exp(quantile(lodOverRoot2log, 0.95))

### Create density plots of the fentanyl exposure data

# load ggplot2
library(ggplot2)

# convert log-transformed and imputed fentanyl exposure data to a dataframe
# for ggplot2
lodOverRoot2logdf <- data.frame(value = lodOverRoot2log)

# create density plot with ggplot2
log_density <- ggplot(lodOverRoot2logdf, aes(x=value)) + 
  geom_density()+
  labs(title="Density curve of log-transformed airborne fentanyl exposure data with LOD/sqrt(2)",
       x="Airborne fentanyl concentration (ng/m^3)", y = "Density") +
  theme(plot.title = element_text(
      hjust = 0.5, 
      size = 10))
log_density

# convert imputed fentanyl exposure data to a dataframe
# for ggplot2
lodOverRoot2df <- data.frame(value = lodOverRoot2)

# create density plot with ggplot2
density <- ggplot(lodOverRoot2df, aes(x=value)) + 
  geom_density() +
  labs(title="Density curve of airborne fentanyl exposure data with LOD/sqrt(2)",
       x="Airborne fentanyl concentration (ng/m^3)", y = "Density")+
  theme(plot.title = element_text(
    hjust = 0.5, 
    size = 10))
density


# create density plot for temperature with ggplot2
temperature_density <- ggplot(fentanyldata, aes(x=Temperature_C)) + 
  geom_density() +
  labs(title="Density curve of temperature",
       x="Temperature (degrees Celcius)", y = "Density")+
  theme(plot.title = element_text(
    hjust = 0.5, 
    size = 12))
temperature_density

### plot a histogram of the transformed values example from tutorial

hist(lodOverRoot2)

# improve the base histogram
hist(lodOverRoot2,
     breaks = 50,
     main = "PM2.5 with LOD/sqrt(2)",
     ylab = "Frequency",
     xlab = "PM2.5 Concentration (ug/m3)",
     col = "blue",
     las = 1)
