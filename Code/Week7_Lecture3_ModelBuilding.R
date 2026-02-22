################################################
# This script walks through the linear model
# building process to address the study question
# "What climatic variables are most predictive 
# of PM2.5 concentrations in Quesnel during 
# very cold weather?"
################################################

  library(dplyr)

### load pm25data

  load("Data/pm25_model_building.rdata")
  
### figure out which city has highest PM2.5 during cold weather
  
  pm25data %>%
    filter(Temperature < 0) %>%
    group_by(City) %>%
    summarize(gmPM25 = exp(mean(logPM25)))
  
### subset to the rows of interest
  
  q.sub0 <- subset(pm25data, Temperature < 0 & City == "Quesnel")
  
### check the summary of q.sub0 to see how subsetting has affected the 
### variables
  
  summary(q.sub0)
  # City must be dropped, only Quesnel
  # Year is balanced
  # Season has no summer observations, but okay to keep
  # HMSsmoke has only 1 observation in YES category, must be dropped
  # FRP has low numbers in high and moderate category
  # AOD range much reduced
  # LandConc max is 13.3
  # PBLH much reduced
  # Windy has similar balance
  # Temperature down to -25
  # Humidity has no high values
  
### summary shows one very low PM25 value, likely an error so remove
  
  q.sub0 <- subset(q.sub0, PM25 >= 1)
  
### reduce to the variables of interest, which are climatic predictors
  
  q.sub0 <- q.sub0[ , c("PM25", "logPM25", "Season", "PBLH", "Windy", 
                        "Temperature", "Humidity")]
  
### refactor the variables that have lost data in one category,
### otherwise the empty category will persist
  
  q.sub0$Season = factor(q.sub0$Season, 
                         levels = c("Spring", "Autumn", "Winter"))
  q.sub0$Humidity = factor(q.sub0$Humidity, 
                           levels = c("Low", "Moderate"))
  
### evaluate the relationship between logPM25 and all independent 
### variables
  
  # Season
  boxplot(logPM25~Season, data = q.sub0)
  summary(aov(logPM25~Season, data = q.sub0))
  summary(lm(logPM25~Season, data = q.sub0)) 
  #R2 = 0.059, fall positive, winter positive
  
  # PBLH
  plot(logPM25~PBLH, data = q.sub0)
  cor.test(q.sub0$logPM25, q.sub0$PBLH, method = "pearson")
  summary(lm(logPM25~PBLH, data = q.sub0)) 
  #R2 = 0.070, effect negative
  
  # Windy
  boxplot(logPM25~Windy, data = q.sub0)
  var.test(logPM25~Windy, data = q.sub0)
  t.test(logPM25~Windy, data = q.sub0, var.equal = T)
  summary(lm(logPM25~Windy, data = q.sub0)) 
  #R2 = 0.107, yes negative
  
  # Temperature
  plot(logPM25~Temperature, data = q.sub0)
  cor.test(q.sub0$logPM25, q.sub0$Temperature, method = "pearson")
  summary(lm(logPM25~Temperature, data = q.sub0)) 
  #R2 = 0.004, effect positive
  
  # Humidity
  boxplot(logPM25~Humidity, data = q.sub0)
  var.test(logPM25~Humidity, data = q.sub0)
  t.test(logPM25~Humidity, data = q.sub0, var.equal = T)
  summary(lm(logPM25~Humidity, data = q.sub0)) 
  #R2 = 0.005, moderate positive
  
### Evaluate the relationships between independent variables
  
  # Season vs. PBLH
  pair <- PBLH~Season
  boxplot(pair, data = q.sub0)
  summary(aov(pair, data = q.sub0))
  # Spring is higher than winter and fall
  
  # Season vs. Windy
  pair <- Windy~Season
  mosaicplot(pair, data = q.sub0)
  chisq.test(q.sub0$Windy, q.sub0$Season)
  # no relationship
  
  # Season vs. Temperature
  pair <- Temperature~Season
  boxplot(pair, data = q.sub0)
  summary(aov(pair, data = q.sub0))
  # winter colder than spring and fall
  
  # Season vs. Humidity
  pair <- Humidity~Season
  mosaicplot(pair, data = q.sub0)
  chisq.test(q.sub0$Humidity, q.sub0$Season)
  # difference between winter and fall, not sprint
  
  # PBLH vs. Windy
  pair <- PBLH~Windy
  boxplot(pair, data = q.sub0)
  var.test(pair, data = q.sub0)
  t.test(pair, data = q.sub0, var.equal = T)
  # higher on windy days
  
  # PBLH vs. Temperature
  pair <- PBLH~Temperature
  plot(pair, data = q.sub0)
  cor.test(q.sub0$PBLH, q.sub0$Temperature, method = "pearson")
  # clear positive relationship
  
  # PBLH vs. Humidity
  pair <- PBLH~Humidity
  boxplot(pair, data = q.sub0)
  var.test(pair, data = q.sub0)
  t.test(pair, data = q.sub0, var.equal = F)
  # Lower humidity when PBLH is high
  
  # Windy vs. Temperature
  pair <- Temperature~Windy
  boxplot(pair, data = q.sub0)
  var.test(pair, data = q.sub0)
  t.test(pair, data = q.sub0, var.equal = F)
  # Windy days are warmer
  
  # Windy vs. Humidity
  pair <- Humidity~Windy
  mosaicplot(pair, data = q.sub0)
  chisq.test(q.sub0$Humidity, q.sub0$Windy)
  # no relationship
  
  # Temperature vs. Humidity
  pair <- Temperature~Humidity
  boxplot(pair, data = q.sub0)
  var.test(pair, data = q.sub0)
  t.test(pair, data = q.sub0, var.equal = F)
  # Only warmed days are moderate humidity
  
### stat with a model that has all variables
  
  all <- lm(logPM25 ~ Season + PBLH + Windy + Temperature + Humidity,
            data = q.sub0)
  summary(all)
  # adjusted R = 0.1956, Humidity insignificant, all variables have
  # effects in the same direction as crude models
  
### remove humidity 
  
  no.humid <- lm(logPM25 ~ Season + PBLH + Windy + Temperature,
            data = q.sub0)
  summary(no.humid)
  # adjusted R = 0.1934, effects of other variables have not
  # moved much . Looks like a good model 
  
### remove all remaining variables to ensure they contribute at least
### 1% to the r2 value
  
  # Season
  summary(lm(logPM25 ~ PBLH + Windy + Temperature, data = q.sub0))
  # R2 = 0.1678
  
  # PBLH
  summary(lm(logPM25 ~ Season + Windy + Temperature, data = q.sub0))
  # R2 = 0.1683
  
  # Windy
  summary(lm(logPM25 ~ Season + PBLH + Temperature, data = q.sub0))
  # R2 = 0.1016
  
  # Temperature
  summary(lm(logPM25 ~ Season + PBLH + Windy, data = q.sub0))
  # R2 = 0.1741
  
### check what the stepwise approach would have produced
  
  step(all, direction = "both", trace = F)
  # same thing!
  
### Check model diagnostics
  
  plot(no.humid, which = 2)
  plot(no.humid, which = 4)
  
### extract crude and adjusted RRs for each variable 
  
  #Season
  exp(lm(logPM25~Season, data = q.sub0)$coefficients) #crude
  exp(no.humid$coefficients[1:3]) #adjusted
  
  #PBLH
  exp(lm(logPM25~PBLH, data = q.sub0)$coefficients) #crude
  exp(no.humid$coefficients[c(1,4)]) #adjusted
  
  #Windy
  exp(lm(logPM25~Windy, data = q.sub0)$coefficients) #crude
  exp(no.humid$coefficients[c(1,5)]) #adjusted
  
  #Temperature
  exp(lm(logPM25~Temperature, data = q.sub0)$coefficients) #crude
  exp(no.humid$coefficients[c(1,6)]) #adjusted
  
### Plot fitted vs. observed values
  
  plot(no.humid$fitted.values, q.sub0$logPM25)
  
### Make predictions for high and low scenarios
  
  to.predict <- data.frame(Season = c("Autumn", "Spring"),
                           PBLH = c(200, 1500),
                           Windy = c("No", "Yes"),
                           Temperature = c(0, -25))
  
  exp(predict(no.humid, to.predict, interval = "confidence"))


  