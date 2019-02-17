#1.7.............. Univariate analysis after standardisation and removing the City........

#check summary statistics for all variables at once
library(pastecs)
stat.desc(EU_result)

attach(EU_result)


# ...... a. probability density histogram dependant variable leave percenatage
# boxplot of dependent variable

boxdata<- boxplot(pct_Leave,
                  names=c("Leave vote Percentage "),
                  xlab="Leave vote", ylab="percentage", col = "Bisque")
# check by factor 

# boxplot for variable of "percentage leave" by factor Region
op <- par(mar = c(5, 8, 4, 2) + 0.1)
boxplot(pct_Leave ~ Region, xlab=" percentage of Leave vote", ylab="",
        horizontal = TRUE, las = 1, cex.axis = 0.7, col = "grey")

library(DescTools)
# Visualisa boxplot in a quantile graph
Desc(Region ~ pct_Leave, data=EU_result, digits=1)


#PlotBubble(EU_result$x, EU_result$y, area=EU_result$pct_Leave, col=SetAlpha("deeppink4",0.4),
           #border="darkblue",
           #xlab="", ylab="", panel.first=grid(), main="Percentage Leave vote in England")
#text(EU_result$x, EU_result$y, labels=EU_result$Region, cex=0.7, adj=0.5)

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



# ...... b. probability density histogram Vote percenatage difference

# dependent variable histogram
hist(Diff, freq = F, col = "light blue", border = "dark blue", main = "Histogram", xlab = "%age Vote Outcome Difference") 
lines(density(sort(Diff)))


hist(Diff, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.03),
     xlab = "Voting Outcome Percenatage difference", main = "Histogram")
rug (Diff)
# Add a density curve
lines (density(sort(Diff)))

# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(Diff), to = max(Diff), by = 0.1)
yfit = dnorm(xfit, mean(Diff), sd(Diff))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

# QQ plot to check normality

qqnorm(Diff, xlab = "Theoretical Quantiles: Difference (Leave - Remain)")
qqline(Diff, col=2) ## red color

# KS test of normality
ks.test(Diff,"pnorm", mean(Diff), sd(Diff))




# # ...... 1. probability density histogram Vote young_age


# variable histogram
hist(young_age, freq = F, col = "light blue", border = "dark blue", main = "Histogram", xlab = "Young age") 
lines(density(sort(young_age)))


hist(young_age, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.10),
     xlab = "young age distribution", main = "Histogram")
rug (young_age)
# Add a density curve
lines (density(sort(young_age)))

# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(young_age), to = max(young_age), by = 0.1)
yfit = dnorm(xfit, mean(young_age), sd(young_age))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)


# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

# QQ plot to check normality

qqnorm(young_age, xlab = "Theoretical Quantiles: Young age group")
qqline(young_age, col=2) ## red color

# KS test of normality
ks.test(young_age,"pnorm", mean(young_age), sd(young_age))


# # ...... 2. probability density histogram Vote middle_age

# dependent variable histogram


#histogram
hist(middle_age, freq = F, col = "light blue", border = "dark blue", main = "Histogram", xlab = "Middle age distribution") 
lines(density(sort(middle_age)))


hist(middle_age, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.30),
     xlab = "Middle age distribution", main = "Histogram")
rug (middle_age)
# Add a density curve
lines (density(sort(middle_age)))

# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(middle_age), to = max(middle_age), by = 0.1)
yfit = dnorm(xfit, mean(middle_age), sd(middle_age))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

# QQ plot to check normality

qqnorm(middle_age, xlab = "Theoretical Quantiles: middle age group")
qqline(middle_age, col=2) ## red color

# KS test of normality
ks.test(middle_age,"pnorm", mean(middle_age), sd(middle_age))


# # ...... 3. probability density histogram Vote older_age

# dependent variable histogram
hist(older_age, freq = F, col = "light blue", border = "dark blue", main = "Histogram", xlab = "Older age distribution") 
lines(density(sort(older_age)))


hist(older_age, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.10),
     xlab = "Older age distribution", main = "Histogram")
rug (older_age)
# Add a density curve
lines (density(sort(older_age)))

# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(older_age), to = max(older_age), by = 0.1)
yfit = dnorm(xfit, mean(older_age), sd(older_age))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
# QQ plot to check normality

qqnorm(older_age, xlab = "Theoretical Quantiles: older age group")
qqline(older_age, col=2) ## red color

# KS test of normality
ks.test(older_age,"pnorm", mean(older_age), sd(older_age))



# # ...... 5. probability density histogram deprived household

# dependent variable histogram

hist(hh_deprived, freq = F, col = "light blue", border = "dark blue", main = "Histogram", xlab = "hh_deprived distribution") 
lines(density(sort(hh_deprived)))


hist(hh_deprived, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.13),
     xlab = "hh_deprived distribution", main = "Histogram")
rug (hh_deprived)
# Add a density curve
lines (density(sort(hh_deprived)))

# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(hh_deprived), to = max(hh_deprived), by = 0.1)
yfit = dnorm(xfit, mean(hh_deprived), sd(hh_deprived))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)


# QQ plot to check normality

qqnorm(hh_deprived, xlab = "Theoretical Quantiles: Deprived household")
qqline(hh_deprived, col=2) ## red color

# KS test of normality
ks.test(hh_deprived,"pnorm", mean(hh_deprived), sd(hh_deprived))



# # ...... 6. probability density histogram Claimant

# dependent variable histogram
hist(claimant, freq = F, col = "light blue", border = "dark blue", main = "Histogram", xlab = "claimant") 
lines(density(sort(claimant)))


hist(claimant, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.60),
     xlab = "claimant distribution", main = "Histogram")
rug (claimant)
# Add a density curve
lines (density(sort(claimant)))

# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(claimant), to = max(claimant), by = 0.1)
yfit = dnorm(xfit, mean(claimant), sd(claimant))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

# QQ plot to check normality

qqnorm(claimant, xlab = "Theoretical Quantiles: Claimant percentage")
qqline(claimant, col=2) ## red color

# KS test of normality
ks.test(claimant,"pnorm", mean(claimant), sd(claimant))


 #7. probability density histogram white ethnicity

# dependent variable histogram
hist(ethn_white, freq = F, col = "light blue", border = "dark blue", main = "Histogram", xlab = "White ethnicity") 
lines(density(sort(ethn_white)))


hist(ethn_white, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.60),
     xlab = "White ethnicity distribution", main = "Histogram")
rug (ethn_white)
# Add a density curve
lines (density(sort(ethn_white)))

# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(ethn_white), to = max(ethn_white), by = 0.1)
yfit = dnorm(xfit, mean(ethn_white), sd(ethn_white))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

# QQ plot to check normality

qqnorm(ethn_white, xlab = "Theoretical Quantiles: White ethnicity")
qqline(ethn_white, col=2) ## red color

# KS test of normality
ks.test(ethn_white,"pnorm", mean(ethn_white), sd(ethn_white))


# # ...... 7. probability density histogram economically inactive

# independent variable histogram
hist(econ_inactive, freq = F, col = "light blue", border = "dark blue", main = "Histogram", xlab = "Economically Inactive") 
lines(density(sort(econ_inactive)))


hist(econ_inactive, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.10), xlim = c(0,99),
     xlab = "econ_inactive", main = "Histogram")
rug (econ_inactive)
# Add a density curve
lines (density(sort(econ_inactive)))

# QQ plot to check normality

qqnorm(econ_inactive, xlab = "Theoretical Quantiles: Economically Inactive")
qqline(econ_inactive, col=2) ## red color

# KS test of normality
ks.test(econ_inactive,"pnorm", mean(econ_inactive), sd(econ_inactive))


# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(econ_inactive), to = max(econ_inactive), by = 0.1)
yfit = dnorm(xfit, mean(econ_inactive), sd(econ_inactive))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

# # ...... 8. probability density histogram jobs density

# dependent variable histogram
hist(Jobsdensity, freq = F, col = "light blue", border = "dark blue", main = "Histogram", xlab = "Jobsdensity") 
lines(density(sort(Jobsdensity)))


hist(Jobsdensity, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.15), 
     xlab = "Jobsdensity distribution", main = "Histogram")
rug (Jobsdensity)
# Add a density curve
lines (density(sort(Jobsdensity)))

# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(Jobsdensity), to = max(Jobsdensity), by = 0.1)
yfit = dnorm(xfit, mean(Jobsdensity), sd(Jobsdensity))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
# QQ plot to check normality

qqnorm(Jobsdensity, xlab = "Theoretical Quantiles: Jobs Density")
qqline(Jobsdensity, col=2) ## red color

# KS test of normality
ks.test(Jobsdensity,"pnorm", mean(Jobsdensity), sd(Jobsdensity))

# # ...... 8. probability density histogram migrant change

# independent variable histogram
hist(White_migrants, freq = F, col = "light blue", border = "dark blue", main = "Histogram", xlab = "percentage of White migrants") 
lines(density(sort(White_migrants)))


hist(White_migrants, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.25), 
     xlab = "white migrants", main = "Histogram")
rug (White_migrants)
# Add a density curve
lines (density(sort(White_migrants)))

# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(White_migrants), to = max(White_migrants), by = 0.1)
yfit = dnorm(xfit, mean(White_migrants), sd(White_migrants))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
# QQ plot to check normality

qqnorm(White_migrants, xlab = "Theoretical Quantiles: White migrants")
qqline(White_migrants, col=2) ## red color

# KS test of normality
ks.test(White_migrants,"pnorm", mean(White_migrants), sd(White_migrants))


# # ...... 9. probability density histogram level 4 and above

# independent variable histogram
hist(level4andabove, freq = F, col = "light blue", border = "dark blue", main = "Histogram", xlab = "level4 and above qualifications") 
lines(density(sort(level4andabove)))


hist(level4andabove, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.08), 
     xlab = "level 4 and above qualification", main = "Histogram")
rug (level4andabove)
# Add a density curve
lines (density(sort(level4andabove)))

# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(level4andabove), to = max(level4andabove), by = 0.1)
yfit = dnorm(xfit, mean(level4andabove), sd(level4andabove))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
# QQ plot to check normality

qqnorm(level4andabove, xlab = "Theoretical Quantiles: Level 4 and above")
qqline(level4andabove, col=2) ## red color

# KS test of normality
ks.test(level4andabove,"pnorm", mean(level4andabove), sd(level4andabove))

# # ...... 10. probability density histogram no qualifications

# dependent variable histogram
hist(noqualifications, freq = F, col = "pink", border = "black", main = "Histogram", xlab = "no qualifications") 
lines(density(sort(noqualifications)))


hist(noqualifications, col = "pink", border = "black", freq = F, ylim = c(0,0.10), 
     xlab = "no qualifications", main = "Histogram")
rug (noqualifications)
# Add a density curve
lines (density(sort(noqualifications)))

# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(noqualifications), to = max(noqualifications), by = 0.1)
yfit = dnorm(xfit, mean(noqualifications), sd(noqualifications))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
# QQ plot to check normality

qqnorm(noqualifications, xlab = "Theoretical Quantiles: No Qualifications")
qqline(noqualifications, col=2) ## red color

# KS test of normality
ks.test(noqualifications,"pnorm", mean(noqualifications), sd(noqualifications))


# # ...... 11. probability density histogram high social grade

# dependent variable histogram
hist(higher_interm, freq = F, col = "pink", border = "black", main = "Histogram", xlab = "High and intermediate social grade") 
lines(density(sort(higher_interm)))


#hist(abhigher_interm, col = "pink", border = "black", freq = F, ylim = c(0,0.10), 
#xlab = "High and intermediate social grade", main = "Histogram")
rug (higher_interm)
# Add a density curve
lines (density(sort(higher_interm)))

# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(higher_interm), to = max(higher_interm), by = 0.1)
yfit = dnorm(xfit, mean(higher_interm), sd(higher_interm))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

# QQ plot to check normality

qqnorm(higher_interm, xlab = "Theoretical Quantiles: higher and intermediate social grade")
qqline(higher_interm, col=2) ## red color

# KS test of normality
ks.test(higher_interm,"pnorm", mean(level4andabove), sd(level4andabove))


# ...... 12. probability density histogram lowest social grade

# dependent variable histogram
hist(semi_unskilled, freq = F, col = "pink", border = "black", main = "Histogram", xlab = "Lowest social grade and unemployed") 
lines(density(sort(semi_unskilled)))


hist(semi_unskilled, col = "pink", border = "black", freq = F, ylim = c(0,0.10), 
     xlab = "Lowest social grade and unemployed", main = "Histogram")
rug (semi_unskilled)
# Add a density curve
lines (density(sort(semi_unskilled)))

# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(semi_unskilled), to = max(semi_unskilled), by = 0.1)
yfit = dnorm(xfit, mean(semi_unskilled), sd(semi_unskilled))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

# QQ plot to check normality

qqnorm(semi_unskilled, xlab = "Theoretical Quantiles: Semi and unskilled")
qqline(semi_unskilled, col=2) ## red color

# KS test of normality
ks.test(semi_unskilled,"pnorm", mean(semi_unskilled), sd(semi_unskilled))

# # ...... 13. probability density histogram Property owned

# dependent variable histogram
hist(prop_Owned, freq = F, col = "pink", border = "black", main = "Histogram", xlab = "Property owners") 
lines(density(sort(prop_Owned)))


hist(prop_Owned, col = "pink", border = "black", freq = F, ylim = c(0,0.07), 
     xlab = "property owners", main = "Histogram")
rug (prop_Owned)
# Add a density curve
lines (density(sort(prop_Owned)))

# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(prop_Owned), to = max(prop_Owned), by = 0.1)
yfit = dnorm(xfit, mean(prop_Owned), sd(prop_Owned))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
# QQ plot to check normality

qqnorm(prop_Owned, xlab = "Theoretical Quantiles: Property owners")
qqline(prop_Owned, col=2) ## red color

# KS test of normality
ks.test(prop_Owned,"pnorm", mean(prop_Owned), sd(prop_Owned))

# # ...... 14. probability density histogram social renters

# dependent variable histogram
hist(socialrented, freq = F, col = "pink", border = "black", main = "Histogram", xlab = "Social renters") 
lines(density(sort(socialrented)))


hist(socialrented, col = "pink", border = "black", freq = F, ylim = c(0,0.12), 
     xlab = "social renters", main = "Histogram")
rug (socialrented)
# Add a density curve
lines (density(sort(socialrented)))

# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(socialrented), to = max(socialrented), by = 0.1)
yfit = dnorm(xfit, mean(socialrented), sd(socialrented))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
# QQ plot to check normality

qqnorm(socialrented, xlab = "Theoretical Quantiles: Social renters")
qqline(socialrented, col=2) ## red color

# KS test of normality
ks.test(socialrented,"pnorm", mean(socialrented), sd(socialrented))


# # ...... 15. probability density histogram social crime

# dependent variable histogram
hist(crime, freq = F, col = "pink", border = "black", main = "Histogram", xlab = "Crime percentage") 
lines(density(sort(crime)))


hist(crime, col = "pink", border = "black", freq = F, ylim = c(0,0.18), 
     xlab = "Crime percentage", main = "Histogram")
rug (crime)
# Add a density curve
lines (density(sort(crime)))

# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(crime), to = max(crime), by = 0.1)
yfit = dnorm(xfit, mean(crime), sd(crime))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

# QQ plot to check normality

qqnorm(crime, xlab = "Theoretical Quantiles: crime")
qqline(crime, col=2) ## red color

# KS test of normality
ks.test(crime,"pnorm", mean(crime), sd(crime))

# # ...... 16. probability density histogram life satisfaction

# dependent variable histogram
hist(life_Sat, freq = F, col = "pink", border = "black", main = "Histogram", xlab = "Life Satisfaction") 
lines(density(sort(life_Sat)))


hist(life_Sat, col = "pink", border = "black", freq = F, ylim = c(0,2.5), 
     xlab = "life_Satisfaction", main = "Histogram")
rug (life_Sat)
# Add a density curve
lines (density(sort(life_Sat)))

# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(life_Sat), to = max(life_Sat), by = 0.1)
yfit = dnorm(xfit, mean(life_Sat), sd(life_Sat))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
# QQ plot to check normality

qqnorm(life_Sat, xlab = "Theoretical Quantiles: Life Satisfaction")
qqline(life_Sat, col=2) ## red color

# KS test of normality
ks.test(life_Sat,"pnorm", mean(life_Sat), sd(life_Sat))


# # ...... 16. probability density histogram gross disposable household income


# dependent variable histogram
hist(disp_inc2016, freq = F, col = "pink", border = "black", main = "Histogram", xlab = "Gross household income") 
lines(density(sort(disp_inc2016)))


hist(disp_inc2016, col = "pink", border = "black", freq = F, ylim = c(0,1.5), 
     xlab = "Gross Household income", main = "Histogram")
rug (disp_inc2016)
# Add a density curve
lines (density(sort(disp_inc2016)))

# Add a Normal curve
# seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
#    length.out = NULL, along.with = NULL, ...)
xfit <- seq(from = min(disp_inc2016), to = max(disp_inc2016), by = 0.1)
yfit = dnorm(xfit, mean(disp_inc2016), sd(disp_inc2016))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
# QQ plot to check normality

qqnorm(disp_inc2016, xlab = "Theoretical Quantiles: Disposable income")
qqline(disp_inc2016, col=2) ## red color

# KS test of normality
ks.test(disp_inc2016,"pnorm", mean(disp_inc2016), sd(disp_inc2016))


detach(EU_result)

