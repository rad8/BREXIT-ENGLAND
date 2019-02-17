#Bivariate Analysis

# a scatterplot with the independent variable and respective correlation test

#........1. Young_age
plot(young_age, pct_Leave, pch = 16, col = "red", cex = 0.5)
cor(young_age, pct_Leave, method = "spearman")


#........2. middle_age
plot(middle_age, pct_Leave, pch = 16, col = "red", cex = 0.5)
cor(middle_age, pct_Leave, method = "spearman")

#........3. older_age
plot(older_age, pct_Leave, pch = 16, col = "red", cex = 0.5)
cor(older_age, pct_Leave, method = "spearman")

#........4. hh_deprived
plot(hh_deprived, pct_Leave, pch = 16, col = "red", cex = 0.5)
cor(hh_deprived, pct_Leave, method = "spearman")

#........5. Claimant
plot(claimant, pct_Leave, pch = 16, col = "red", cex = 0.5)
cor(claimant, pct_Leave, method = "spearman")

#........6. Economically inactive
plot(econ_inactive, pct_Leave, pch = 16, col = "red", cex = 0.5)
cor(econ_inactive, pct_Leave, method = "spearman")

#........7. white ethnicity 
plot(ethn_white, pct_Leave, pch = 16, col = "red", cex = 0.5)
cor(ethn_white, pct_Leave, method = "spearman")

#........8. jobs density 
plot(Jobsdensity, pct_Leave, pch = 16, col = "red", cex = 0.5)
cor(Jobsdensity, pct_Leave, method = "spearman")

#........8. jobs density 
plot(White_migrants, pct_Leave, pch = 16, col = "red", cex = 0.5)
cor(White_migrants, pct_Leave, method = "spearman")

#........9. level4 and above 
plot(level4andabove, pct_Leave, pch = 16, col = "red", cex = 0.5)
cor(level4andabove, pct_Leave, method = "spearman")
#cor.test(level4andabove, pct_Leave, method = "spearman", conf.level = 0.95)

d.frm <- EU_result[complete.cases(EU_result[,c("pct_Leave","level4andabove")]),]

#two dimensional box plot
PlotBag(x=EU_result$level4andabove, y=EU_result$pct_Leave, xlab="Share of Degree and above qualified",
        ylab="Leave vote percentage", main="Two-dimensional Boxplot")



#........10 no qualifications
plot(noqualifications, pct_Leave, pch = 16, col = "red", cex = 0.5)
cor(noqualifications, pct_Leave, method = "spearman")

#........11. Higher and interim
plot(higher_interm, pct_Leave, pch = 16, col = "red", cex = 0.5)
cor.test(higher_interm, pct_Leave, method = "spearman", conf.level = 0.95)

#........12. Semi and unskilled
plot(semi_unskilled, pct_Leave, pch = 16, col = "red", cex = 0.5)
cor.test(semi_unskilled, pct_Leave, method = "spearman", conf.level = 0.95)

#........13. Property owners
plot(prop_Owned, pct_Leave, pch = 16, col = "red", cex = 0.5)
cor(prop_Owned, pct_Leave, method = "spearman")

#........14. Socialrenters
plot(socialrented, pct_Leave, pch = 16, col = "red", cex = 0.5)
cor(socialrented, pct_Leave, method = "spearman")

#........15. crime
plot(crime, pct_Leave, pch = 16, col = "red", cex = 0.5)
cor(crime, pct_Leave, method = "spearman")

#........15. life satisfaction
plot(life_Sat, pct_Leave, pch = 16, col = "red", cex = 0.5)
cor(life_Sat, pct_Leave, method = "spearman")

#........16. disposable income
plot(disp_inc2016, pct_Leave, pch = 16, col = "red", cex = 0.5)
cor(disp_inc2016, pct_Leave, method = "spearman")


#....MULTIVARIATE ANALSYIS


#For multivariate, Order all variables and get rid of unncessary variables such as location
EU_ordered<- EU_result[c(5,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27)]



#do correlation  
cor.matrix <- cor(EU_ordered, use="complete.obs",  method="pearson")
cor.df <- as.data.frame(cor.matrix)
print(cor.df,digits=3)




#-----Visualise correlation-------------------------------------------


library(corrgram)
# corrgram works best with Pearcon correlation
corrgram(EU_ordered, order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="Pearson Multivariate Correlation matrix")


# DIMENSIONALITY REDUCTION

#........... Statistical significance test

cor.test(EU_ordered$pct_Leave, EU_ordered$young_age, method = "spearman")# -0.18 reject null hypothesis 
cor.test(EU_ordered$pct_Leave, EU_ordered$middle_age, method = "spearman")# -0.24 reject null hypothesis 
cor.test(EU_ordered$pct_Leave, EU_ordered$older_age, method = "spearman")# 0.30 reject null hypothesis 
cor.test(EU_ordered$pct_Leave, EU_ordered$hh_deprived, method = "spearman")# -0.47 reject null hypothesis 
cor.test(EU_ordered$pct_Leave, EU_ordered$claimant, method = "spearman")# 0.28 reject null hypothesis 
cor.test(EU_ordered$pct_Leave, EU_ordered$econ_inactive, method = "spearman")# 0.36 reject null hypothesis 
cor.test(EU_ordered$pct_Leave, EU_ordered$ethn_white, method = "spearman")# 0.35 reject null hypothesis 
cor.test(EU_ordered$pct_Leave, EU_ordered$Jobsdensity, method = "spearman")# -0.40 reject null hypothesis 
cor.test(EU_ordered$pct_Leave, EU_ordered$White_migrants, method = "spearman")# -0.27 reject null hypothesis 
cor.test(EU_ordered$pct_Leave, EU_ordered$level4andabove, method = "spearman")# -0.873 reject null hypothesis 
cor.test(EU_ordered$pct_Leave, EU_ordered$noqualifications, method = "spearman")# 0.80 reject null hypothesis 
cor.test(EU_ordered$pct_Leave, EU_ordered$higher_interm, method = "spearman")# -0.76 reject null hypothesis 
cor.test(EU_ordered$pct_Leave, EU_ordered$semi_unskilled, method = "spearman")# 0.48 reject null hypothesis 
cor.test(EU_ordered$pct_Leave, EU_ordered$socialrented, method = "spearman")# 0.003 safe to reject 
cor.test(EU_ordered$pct_Leave, EU_ordered$crime, method = "spearman")# 0.05 reject null hypothesis
cor.test(EU_ordered$pct_Leave, EU_ordered$life_Sat, method = "spearman")# -0.02 safe to reject null hypothesis
cor.test(EU_ordered$pct_Leave, EU_ordered$disp_inc2016, method = "spearman")# -0.57 reject null hypothesis



# select variables by excluding those not required; the %in% operator means 'matching'
myvars <- names(EU_ordered) %in% c("pct_Leave", "life_Sat", "young_age","claimant", "higher_interm", "socialrented")
                                  

# the ! operator means NOT
EU_ordered2 <- EU_ordered[!myvars]  
str(EU_ordered2)
rm(myvars)

#visualise correlation between independent variables

pairs.panels(EU_ordered2, method = "spearman", hist.col = "grey", col = "blue")


#-----Section 07-------------------------------------------

# Determine Number of Factors to Extract
library(nFactors)

# get eigenvalues
ev <- eigen(cor(EU_ordered2))
ev
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type="b", col="blue", xlab="variables")

# calculate cumulative proportion of eigenvalue and plot
ev.sum<-0
for(i in 1:length(ev$value)){
  ev.sum<-ev.sum+ev$value[i]
}
ev.list1<-1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i]=ev$value[i]/ev.sum
}
ev.list2<-1:length(ev$value)
ev.list2[1]<-ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i]=ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type="b", col="red", xlab="number of components", ylab ="cumulative proportion")

#-----Section 08-------------------------------------------

# Varimax Rotated Principal Components
# retaining 'nFactors' components
library(psych)
library(GPArotation)

fit <- principal(EU_ordered2, nfactors=6, rotate="varimax")
fit




#-----Section 10-------------------------------------------

attach(EU_ordered3)


# Select the variables after factor analysis for use in classification


boxplot(older_age, ethn_white, prop_Owned, crime,
        names=c("older_age", "ethn_white", "prop_owned", "crime"))

#exclude the unnecesssary factor analysis

myvars <- c("older_age", "econ_inactive", "White_migrants", "noqualifications")
EU_ordered3 <- EU_ordered2[myvars]
str(EU_ordered3)
rm(myvars)

# Start from here: Prepare Data


boxplot(EU_ordered3) # visualise the variables
# scale to 0-1
EU_ordered3<- apply(EU_ordered3, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
library(DMwR)
help(SoftMax)

summary(EU_ordered3)
boxplot(EU_ordered3)  # visualise the variables after scaling

# check new variables are uncorrelated
cor.matrix<-cor(EU_ordered3, use="complete.obs", method="spearman")
cor.df <-as.data.frame(cor.matrix)
print(cor.df,digits=3)

library(DescTools)
par(mfrow=c(1,2))
n <- cor(EU_ordered3, use="complete.obs", method="spearman")
PlotCorr(n, col=PalDescTools("RedWhiteBlue1", 100), border="grey",
         args.colorlegend=list(labels=Format(seq(1,-1,-.25), 2), frame="grey"))
- 15 -
  PlotWeb(n, col=c(hred, hblue))



#-----Section 11-------------------------------------------


#....... Ward Hierarchical Clustering
d <- dist(EU_ordered3, method = "euclidean") # distance matrix

set.seed(12345)
fit <- hclust(d, method="ward.D")
plot (fit, labels = EU_result$LA_Name)


# plot(fit) # display dendogram
groups <- cutree(fit, k=4) # cut tree into 2 clusters
# draw dendogram with red borders around the 3 clusters
rect.hclust(fit, k=4, border="red")

# assign cluster number to borough name
mydata1 <- data.frame(EU_result$LA_code)
names(mydata1)[1] <- "LA_code"
mydata1 <- within (mydata1, Name <- EU_result$LA_Name)
mydata1 <- within (mydata1, cluster <- groups)
mydata1 <- data.frame(mydata1, fit$cluster)

mydata1
write.csv(mydata1,file.choose())


#........Instpect Clusters
# get cluster

aggregate(EU_ordered3,by=list(fit$cluster),FUN=mean)

#????????????????????????????????????????????????????

#-----Section 13-------------------------------------------

library(maptools)  # also loads sp library
library(rgdal)
library(GISTools)
library(classInt)
library(RColorBrewer)

# Read in the shapefile of london_polygon

england.polygon <- readOGR(".", "england_lad_2011_gen_clipped")
plot(england.polygon, border = "black", col = "lightgrey")


# merge map and attribute files
england.jn <- merge(england.polygon, mydata1, by.x="code", by.y="LA_code")

# set class intervals, colour and number of classes
shades <- shading(breaks=c(1,2,3,4,5), cols = brewer.pal(4,"Set1"))

# Draw the map polygons
choropleth(england.jn, england.jn$cluster, shades, main="Ward's hierarchical clustering")
#choropleth(england.jn, england.jn$fit.cluster, shades, main="Ward's hierarchical clustering")
# invisible(text(getSpPPolygonsLabptSlots(London.jn), labels=as.character(London.jn$fit.sm.cluster), cex=0.8))
invisible(text(x=england.jn$X_CENTROID, y=england.jn$Y_CENTROID, labels=as.character(england.jn$cluster), cex=0.8))

#????????????????????????????????????????????????????????????
# assign cluster to each record




# check normality of dependent variable before linear regression

attach(EU_result)
# dependent variable histogram
hist(pct_Leave, freq = F, col = "light blue", border = "dark blue", main = "Vote Leave distribution", xlab = "Vote Leave percentage") 
lines(density(sort(pct_Leave)))

hist(pct_Leave, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.05),
     xlab = "percentage of vote leave", main = "Histogram")
rug (pct_Leave)
# Add a density curve
lines (density(sort(pct_Leave)))



# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(pct_Leave), to = max(pct_Leave), by = 0.1)
yfit = dnorm(xfit, mean(pct_Leave), sd(pct_Leave))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

# QQ plot to check normality

qqnorm(pct_Leave, xlab = "Theoretical Quantiles: Leave vote percentage")
qqline(pct_Leave, col=2) ## red color

# KS test of normality
ks.test(pct_Leave,"pnorm", mean(pct_Leave), sd(pct_Leave))



#......HYPOTHESIS TESTING

#Testing hypothesis 2: There is no significant difference between Leave outcome and Remain outcome by Region.
# create a crosstable of Region by outcome
myxtab <- xtabs(~ Region + outcome, exclude = "", data=EU_result)
# extract columns 'leave' and 'Remain' into a new data frame
mytable <- as.data.frame(cbind(myxtab[,"Leave"],myxtab[,"Remain"]))
names(mytable)[1] <- "Leave"
names(mytable)[2] <- "Remain"
# print the table
mytable

# visualise
library(DescTools)
PlotMosaic(as.matrix(mytable), main = "Region ~ outcome")
PlotMosaic(t(as.matrix(mytable)), main = "outcome ~ Region")
PlotCirc(t(as.matrix(mytable)), main = "outcome -> Region")
# t() gives transpose of a matrix.

#-----Section 03-------------------------------------------

# Pearson's Chi-squared Test

# simple chi-squared test
chisq.test(mytable)

# simple chi-squared with additional outputs
chisq_out <- chisq.test(mytable)
chisq_out$observed
chisq_out$expected
chisq_out$stdres
# Standardised residuals are the difference between observed and expected (residual) divided by standard deviation of residuals.
# If your residuals are +/-2, then there is a definite difference, if +/-3 then something unusual is happening.

# chi-squared test using Monte Carlo simulation
chisq.test(mytable, correct = FALSE,
           p = rep(1/length(mytable), length(mytable)), rescale.p = FALSE,
           simulate.p.value = TRUE, B = 2000)

# calculate effect size
CramerV(mytable, conf.level = 0.95)

#check partial correlations

#partial correlation

library(ppcor)

#calculate partial correlation using Pearson and then Spearman
pcor.test(EU_ordered$pct_Leave, EU_ordered$older_age,EU_ordered$ethn_white, method="spearman")
pcor.test(EU_ordered$pct_Leave, EU_ordered$level4andabove,EU_ordered$Jobsdensity, method="spearman")
pcor.test(EU_ordered$pct_Leave, EU_ordered$level4andabove,EU_ordered$disp_inc2016, method="spearman")

#EU_ordered<- EU_ordered[-c(326), ]
#boxplot(EU_ordered$Jobsdensity)

#.............Regression........................................


# MODEL 1 Linear Regression

attach(EU_ordered)

model1 <- lm(pct_Leave ~ noqualifications)

# add regression line to scatter plot
plot(noqualifications, pct_Leave, main="Scatterplot",
     xlab="no qualifications ", ylab="Leave Vote Percentage")
abline(model1, col="red")

summary(model1)
hist(model1$residuals, col = "light blue")
rug(model1$residuals)
# consider normality of residuals
plot(model1$residuals ~ model1$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model1$residuals, "pnorm", mean(model1$residuals), sd(model1$residuals))




#-----Section 03-------------------------------------------


# Multiple Regression

# model with 6 variables from factor analysis
model2 <- lm (pct_Leave ~ older_age  + econ_inactive + White_migrants + noqualifications)
summary(model2)
hist(model2$residuals, col = "light blue")
rug(model2$residuals)
# consider normality of residuals
plot(model2$residuals ~ model2$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model2$residuals, "pnorm", mean(model2$residuals), sd(model2$residuals))




# calculate variance inflation factor
library(car)
vif(model2)
sqrt(vif(model2)) > 2  # if > 2 vif too high



# relative importance of variables
library(relaimpo)
calc.relimp(model2, type = c("lmg"), rela = TRUE)

# use a stepwise approach to search for a best model

library(RcmdrMisc)

# variables from model 2
model2step <- lm (pct_Leave ~ older_age  + econ_inactive + White_migrants + noqualifications)

# forward stepwise selection
model3 <- stepwise(model2step, direction="forward")
summary(model3)
hist(model3$residuals, col="light blue")
rug(model3$residuals)
plot(model3$residuals ~ model3$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model3$residuals, "pnorm", mean(model3$residuals), sd(model3$residuals))
sqrt(vif(model3)) > 2
calc.relimp(model3, type = c("lmg"), rela = TRUE)


# all variables model
rm(stepmodel4)

model4step <- lm (pct_Leave ~ older_age  + econ_inactive + White_migrants + noqualifications
              + young_age+ middle_age+ hh_deprived+ claimant+ethn_white+ Jobsdensity+ level4andabove
              +higher_interm + semi_unskilled + prop_Owned+ socialrented+ crime+life_Sat+ disp_inc2016)
model4 <- stepwise(model4step, direction="forward")
summary(model4)
hist(model4$residuals, col="light blue")
rug(model4$residuals)
plot(model4$residuals ~ model3$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model4$residuals, "pnorm", mean(model4$residuals), sd(model4$residuals))
sqrt(vif(model4)) > 2
calc.relimp(model4, type = c("lmg"), rela = TRUE)


# model 5

# variables from model 2
model5 <- lm (pct_Leave ~ older_age  + econ_inactive + noqualifications + hh_deprived)

# forward stepwise selection

summary(model5)
hist(model5$residuals, col="light blue")
rug(model5$residuals)
plot(model5$residuals ~ model5$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model5$residuals, "pnorm", mean(model5$residuals), sd(model5$residuals))
sqrt(vif(model5)) > 2
calc.relimp(model5, type = c("lmg"), rela = TRUE)


# test whether model2 and model4 are significantly different using F test
anova(model3, model5, test = "F")

#log transformation left skewed dependent variable
attach(EU_result)

logpct_leave <- log(101-pct_Leave)


# dependent variable histogram
hist(logpct_leave, freq = F, col = "light blue", border = "dark blue", main = "Natural Log transformed Leave vote", xlab = "Leave vote") 
lines(density(sort(logpct_leave)))


rug (logpct_leave)
# Add a density curve
lines (density(sort(logpct_leave)))



# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(logpct_leave), to = max(logpct_leave), by = 0.3)
yfit = dnorm(xfit, mean(logpct_leave), sd(logpct_leave))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

# QQ plot to check normality

qqnorm(logpct_leave, xlab = "Theoretical Quantiles: Log transformed Leave vote")
qqline(logpct_leave, col=2) ## red color

ks.test(logpct_leave, "pnorm", mean(logpct_leave), sd(logpct_leave))

#........ log trnsformed regression 
# variables from model 5
model6 <- lm (logpct_leave ~ older_age  + econ_inactive + noqualifications + hh_deprived)

# model 6

summary(model6)
hist(model6$residuals, col="light blue")
rug(model6$residuals)
plot(model6$residuals ~ model6$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model6$residuals, "pnorm", mean(model6$residuals), sd(model6$residuals))
sqrt(vif(model6)) > 2
calc.relimp(model6, type = c("lmg"), rela = TRUE)


#.................... Logistic Regression

#For logistic regression, Order all variables and get rid of unncessary variables such as location

summary(EU_result)
eu.ordered4<- EU_result[c(6,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27)]
summary(eu.ordered4)


# correlations between variables
library(polycor)

eu.poly.cor <- hetcor(eu.ordered4)
eu.poly.cor$type
print(eu.poly.cor$correlations, digits = 2)

# first round use all variables
#converti leave vote as 1 and 0
EU_result$outcome <- ifelse(EU_result$outcome=="Leave",1,0)
mylogit1 <- glm(outcome ~ older_age  + econ_inactive + White_migrants + noqualifications
                + young_age+ middle_age+ hh_deprived+ claimant+ethn_white+ Jobsdensity+ level4andabove
                +higher_interm + semi_unskilled + prop_Owned+ socialrented+ crime+life_Sat+ disp_inc2016)
summary(mylogit1)
sqrt(vif(mylogit1)) > 2

# second round excluding not significant variables
mylogit2 <- glm(outcome ~ older_age + level4andabove + middle_age,
                data = eu.ordered4, family = "binomial")
summary(mylogit2)
sqrt(vif(mylogit2)) > 2

#-----Section 07-------------------------------------------

# calculate Odds Ratio - Exp(b)
exp(coef(mylogit2))

# calculate the 95% confidence intervals (2 tail)
exp(cbind(OR <- coef(mylogit2), confint(mylogit2)))

# variable relative importance for glm() models
library(caret)
varImp(mylogit2)


# R-Squared Computation
yhat <- predict(mylogit2, newdata=eu.ordered4, type="response")
compdata <- data.frame(eu.ordered4, yhat)
compdata$outcome <- as.numeric(compdata$outcome) # change factor back to numeric
r <- cor(compdata$outcome, compdata$yhat)
r
rsq <- r^2
rsq

#-----Section 08-------------------------------------------

# remove all variables from the environment
rm(list=ls())

library(DescTools)
