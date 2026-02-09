### Load data and factor all dichotoous and categorical
### variables

load("Data/fentanyldata.rdata")


### plot logPM25 vs. AOD and add the correlation directly to the plot

cor <- cor.test(fentanyldata$logFent, fentanyldata$Temperature_C)
plot(data = fentanyldata,
     logFent ~ Temperature_C,
     main = "log(Fentanyl) vs. Temperature",
     ylab = "log(Fentanyl concentration (ug/m3))",
     xlab = "Temperature (degrees Celcius)")
text(3,0, paste("Pearson's r =", round(cor$estimate, 2)))

### Regression model for logPM25 and AOD when FRP = 0

fit.temp <- lm(data = fentanyldata, logFent ~ Temperature_C)

exp(confint(fit.temp))
exp(coef(fit.temp))

plot(fit.temp, which = 4, )


fit.temp_vent <- lm(data = fentanyldata,
                   logFent ~ Temperature_C + Ventilation)

exp(confint(fit.temp_vent))
exp(coef(fit.temp_vent))

fit.temp_DUO <- lm(data = fentanyldata,
                    logFent ~ Temperature_C + Drug_Use_Observed)

exp(confint(fit.temp_DUO))
exp(coef(fit.temp_DUO))

fit.vent_DUO <- lm(data = fentanyldata,
                   logFent ~ Ventilation + Drug_Use_Observed)

exp(confint(fit.vent_DUO))
exp(coef(fit.vent_DUO))

fit.temp_DUO_Site <- lm(data = fentanyldata,
                   logFent ~ Temperature_C + Drug_Use_Observed + Site)

exp(confint(fit.temp_DUO_Site))
exp(coef(fit.temp_DUO_Site))

### plot logPM25 vs. AOD and add model results to the plot

plot(data = fentanyldata,
     logFent ~ Temperature_C,
     main = "log(PM2.5) vs. AOD",
     ylab = "log(PM2.5 concentration (ug/m3))",
     xlab = "Aerosol Optical Depth")
abline(fit.aod, col = "red", lwd = 2)
text(3,0, paste0("AOD RR =", 
                 round(exp(fit.aod$coefficients[2]), 2),
                 " [",
                 round(exp(confint(fit.aod)[2,1]), 2),
                 ", ",
                 round(exp(confint(fit.aod)[2,2]), 2),
                 "]"))

### plot the QQ and cook's distance for fit.aod

plot(fit.aod, which = 4, ) #Cook's D
plot(fit.aod, which = 2, ) #QQ

### pairwise matrix plot of variables under consideration

pairs(zeroFRP[ , !(names(zeroFRP) %in% c("HMSsmoke", "FRP", "LandConc", "PM25"))],
      gap = 0.5)

fit.all <- lm(data = zeroFRP,
              logPM25 ~ City + Year + Season + AOD + PBLH + Windy + Temperature + Humidity)


fit.noyear <- lm(data = zeroFRP,
                 logPM25 ~ City + Season + AOD + PBLH + Windy +
                   Temperature + Humidity)

anova(fit.all, fit.noyear)


fit.step <- step(fit.all, direction = "both", trace = F)
summary(fit.step)


### Make predictions with the final model

to.predict <- data.frame(City = c("Prince George", "Quesnel", "Quesnel"),
                         Season = c("Winter", "Summer", "Spring"),
                         AOD = c(2, 4, 1),
                         PBLH = c(500, 2000, 1000),
                         Windy = c("No", "No", "Yes"),
                         Temperature = c(-15, 15, 5),
                         Humidity = c("Low", "High", "Moderate"))
predict(fit.step, to.predict, interval = "confidence")
exp(predict(fit.step, to.predict, interval = "confidence"))