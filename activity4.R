datB <- read.csv("/Users/margaretmanning/GitHub/ENVST_activity4/a04/beaver_dam.csv")
View(datB)

#pch designates point shapes (will be in homework)
plot(datB$dams.n, datB$area.ha, 
     pch = 19,
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab = "Number of beaver dams")
#trouble shooting tip - think about how big plotting window is

#looks like it could be good for a linear regression
#we need to create linear model first 
dam.mod <- lm(datB$area.ha ~ datB$dams.n)

#make and standardize residuals (helps visuals standard deviations)
dam.res <- rstandard(dam.mod)

#double check assumptions - residuals are norm dist
#Q-QPlot and Q-Qline
qqnorm(dam.res)
qqline(dam.res)

#assumption check - equal variance 
plot(datB$dams.n, dam.res, 
     xlab = "beaver dams",
     ylab = "standardized residual",
     pch = 19)
abline(h=0)

#checked assumptions, now can interpret model
#coefficients most important
summary(dam.mod)

#start interpretation at intercept - when at 0 we can expect to see this
#next look at slope - with one unit increase, what is the result
#R^2 value can tell you how the linear model fits 

#make a plot of beaver dams and surface water 
plot(datB$dams.n, datB$area.ha,
     pch = 19,
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab = "Number of beaver dams")
#add regression line
#make line width thicker
abline(dam.mod, lwd=2)

pheno <- read.csv("/Users/margaretmanning/GitHub/ENVST_activity4/a04/red_maple_pheno.csv")

#set up panel of plots with one row and two columns
par(mfrow=c(1,2))
plot(pheno$Tmax,pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Maximum temperature (C)")
plot(pheno$Prcp,pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Precipitation (mm)")
par(mfrow=c(1,2))
plot(pheno$Lat,pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Latitude")
plot(pheno$elev,pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Elevation")
par(mfrow=c(1,2))
plot(pheno$Tmax,pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Maximum temperature (C)")
plot(pheno$doy ~ as.factor(pheno$siteDesc),
     pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Site Description")

#turn off par argument 
dev.off()

#Check for multi-collinearity using covariance plots
plot(~ pheno$Lat + pheno$Tmax + pheno$Tmin + pheno$Prcp + pheno$elev + pheno$siteDesc)

#make sitedesignation zero/one using ifelse funtion
pheno$urID <- ifelse(pheno$siteDesc == "Urban",1,0)

#set up multiple regression 
mlr <- lm(pheno$doy ~ pheno$Tmax + pheno$Prcp + pheno$elev + pheno$urID)

#calculate fitted values from regression line 
mlFitted <- fitted(mlr)

#calculating residuals 
leaf.res <- rstandard(mlr)

#check normal dist
qqnorm(leaf.res)
qqline(leaf.res)

#check equal variance 
plot(mlFitted, leaf.res,
     xlab = "Fitted values",
     ylab = "Standardize residuals",
     pch = 19)
abline(h=0)



