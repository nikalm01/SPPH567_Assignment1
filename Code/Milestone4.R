##############################################
# This script reads, explores, and manipulates the
# PM2.5 data for SPPH567.
##############################################


### read the fentanyl data
load("Data/fentanyldata.rdata")

### make a boxplot of log-transformed Fentanyl concentrations vs. Site

boxplot(fentanyldata$logFent ~ fentanyldata$Site,
        ylab = "log(Fentanyl Concentrations (ug/m3))",
        xlab = "Site",
        main = "Site vs. Log-Transformed Fentanyl Concentrations",
        col = c("blue", "orange"),
        las = 1)

library(ggplot2)

Site_Boxplot <- ggplot(fentanyldata, aes(x=Site, y=logFent, fill=Site)) + 
  geom_boxplot()+
  labs(title="Site vs. Log-Transformed Fentanyl Concentrations",x="Site", y = "log(Fentanyl Concentrations (ug/m3))")+
  theme(plot.title=element_text(size=12))
Site_Boxplot

Room_Boxplot <- ggplot(fentanyldata, aes(x=Room_Type, y=logFent, fill=Room_Type)) + 
  geom_boxplot()+
  labs(title="Room Type vs. Log-Transformed Fentanyl Concentrations",x="Room Type", y = "log(Fentanyl Concentrations (ug/m3))")+
  theme(plot.title=element_text(size=12))
Room_Boxplot

### convert the Room Type variable to factor

#check which group has the lowest geometric mean
aggregate(fentanyldata$logFent,
          by = list(fentanyldata$Room_Type),
          FUN = mean)

#set the levels of Room_Type 
fentanyldata$Room_Type <- factor(fentanyldata$Room_Type,
                                levels = c("office", "hallway", "amenity", "SRO_unit", "HOPS"))



### convert the Site variable to factor

#check which group has the lowest geometric mean
aggregate(fentanyldata$logFent,
          by = list(fentanyldata$Site),
          FUN = mean)

#set the levels of Site 
fentanyldata$Site <- factor(fentanyldata$Site,
                                 levels = c("D", "B", "E", "C", "A"))


### run anovas on the two new variables

aov.Room_Type <- aov(fentanyldata$logFent ~ fentanyldata$Room_Type)
summary(aov.Room_Type)
TukeyHSD(aov.Room_Type)

aov.Site <- aov(fentanyldata$logFent ~ fentanyldata$Site)
summary(aov.Site)
TukeyHSD(aov.Site)

### run linear regressions with the two new variables

lm.Room_Type <- lm(fentanyldata$logFent ~ fentanyldata$Room_Type)
summary(lm.Room_Type)
exp(confint(lm.Room_Type))
exp(coef(lm.Room_Type))

lm.Site <- lm(fentanyldata$logFent ~ fentanyldata$Site)
summary(lm.Site)
exp(confint(lm.Site))
exp(coef(lm.Site))

save(fentanyldata, file = "Data/fentanyldata.rdata")
