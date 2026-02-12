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

library(ggplot2)
ggplot(fentanyldata, aes(x = Temperature_C, y = logFent, color = Site)) + 
  geom_point()+
  labs(title="Log(Fentanyl) vs. Temperature",x="Temperature (degrees C)", y = "log(Fentanyl Concentrations (ug/m3))")
  
ggplot(fentanyldata, aes(x = Temperature_C, y = logFent)) + 
  geom_point()+
  labs(title="Log(Fentanyl) vs. Temperature",x="Temperature (degrees C)", y = "log(Fentanyl Concentrations (ug/m3))")


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

