#-----Get data-------------------------------------------

# set working directory
setwd(dirname(file.choose()))
getwd()
# read in data from csv file
EU_result <- read.csv("EU_ref.csv", stringsAsFactors = FALSE)

# STEP 1....... Exploratory Data Analysis...................

head(EU_result)    

str(EU_result)


#....... PREPROCESSING...........
#1.1..... change outcome to factor and life_sat as num

EU_result$outcome <- factor(EU_result$outcome)
EU_result$life_Sat<-as.numeric(EU_result$life_Sat)


#check string again
str(EU_result)

#.......... UNIVARIATE ANALYSIS.......

# Plot missing values

library(naniar)
library(ggplot2)
gg_miss_var(EU_result)

#check life satisfaction summary

summary(EU_result$life_Sat)


#check misssing values with map and summary statistics again

gg_miss_var(EU_result)
summary(EU_result$life_Sat)

#life satisfaction missing value replace with average (row 52 and 294)

EU_result$life_Sat[is.na(EU_result$life_Sat)] <- mean(EU_result$life_Sat, na.rm = TRUE)



#1.3  box plot and inspect outliers

# dependent variable....1

# boxplot(x, main,xlab, ylab)
boxplot(EU_result$pct_Leave, xlab="Vote Leave ", ylab="leave")
# label outliers
boxdata <- boxplot(EU_result$pct_Leave, xlab="Leave vote", ylab="percentage")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(EU_result$pct_Leave==boxdata$out[i]),pos=4, cex=1)}

# inspect outliers 
EU_result[305,]
EU_result[321,]
# Hackeny and southwark, lowest percentage of vote leave

# dependent variable ......2

# boxplot(x, main,xlab, ylab)
boxplot(EU_result$Diff, xlab="Leave-Remain Percentage ", ylab="difference")
# label outliers
boxdata <- boxplot(EU_result$Diff, xlab="Leave-Remain percentage", ylab="difference")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(EU_result$Diff==boxdata$out[i]),pos=4, cex=1)}

# inspect outliers 
EU_result[305,]
EU_result[321,]
# Hackeny and southwark, lowest percentage of vote leave

#... no difference between the two dependent variables.............

# ..... 1. population2011 variable
# boxplot(x, main,xlab, ylab)
boxplot(EU_result$pop_2011, xlab="Population 2011", ylab="Count")

# label outliers
boxdata <- boxplot(EU_result$pop_2011, xlab="Population 2011", ylab="Count")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(EU_result$pop_2011==boxdata$out[i]),pos=4, cex=1)}
# inspect outliers 
EU_result[281,]
EU_result[291,]

#result is 281 Birmingham with 1073045 population and Leeds 291 with 751485 population



# ... 2. Create a boxplot of age variables

attach(EU_result)
boxplot(young_age, middle_age,older_age,
        names=c("young", "middle", "older"),
        xlab="Age grounp", ylab="Count", col = "Bisque")

# check young people
boxdata <- boxplot(young_age, xlab="young", ylab="Count")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(young_age==boxdata$out[i]),pos=4, cex=1)}

#result is mainly 281 and 291 birmingham and leeds again
EU_result[281,]

# check middle ages
boxdata <- boxplot(middle_age, xlab="young", ylab="Count")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(middle_age==boxdata$out[i]),pos=4, cex=1)}
# result the two most extreme values are sill Birmingham and Leeds

#check older age
boxdata <- boxplot(older_age, xlab="young", ylab="Count")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(older_age==boxdata$out[i]),pos=4, cex=1)}
# result is still birmingham and leeds




# ... 4. Create a boxplot of deprivation variable
boxplot(hh_deprived,
        names=c("Deprivation"),
        xlab="deprived household", ylab="Count", col = "Bisque")

# check young people
boxdata <- boxplot(hh_deprived, xlab="deprived household", ylab="Count")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(hh_deprived==boxdata$out[i]),pos=4, cex=1)}



# result is again Birmingham and leeds with outliers

#............NEED TO STANDARDISE...............


detach(EU_result)

#1.4 ..............standardisation........................

EU_result <- within (EU_result,young_age <- (young_age/ pop_2011)*100)
EU_result <- within (EU_result, middle_age <- (middle_age / pop_2011)*100)
EU_result <- within (EU_result,older_age <- (older_age/ pop_2011)*100)
EU_result <- within (EU_result,hh_deprived <- (hh_deprived/ pop_2011)*100)
EU_result <- within (EU_result, claimant <- (claimant / econ_active)*100)
EU_result <- within (EU_result,econ_inactive<- (econ_inactive/ pop_2011)*100)
EU_result <- within (EU_result, ethn_white <- (ethn_white / pop_2011)*100)
EU_result <- within (EU_result, White_migrants <- (White_migrants / pop_2011)*100)
EU_result <- within (EU_result, level4andabove<- (level4andabove / pop_2011)*100)
EU_result <- within (EU_result,noqualifications <- (noqualifications/ pop_2011)*100)
EU_result <- within (EU_result, higher_interm <- (higher_interm / pop_2011)*100)
EU_result <- within (EU_result, semi_unskilled<- (semi_unskilled / pop_2011)*100)
EU_result <- within (EU_result,prop_Owned <- (prop_Owned/ pop_2011)*100)
EU_result <- within (EU_result, socialrented <- (socialrented / pop_2011)*100)
EU_result <- within (EU_result, crime <- (crime/ pop_2011)*100)



#1.5 .........boxplot after standardisation...................................................
attach(EU_result)

# ...  Create a boxplot of age variables
boxplot(young_age, middle_age, older_age,
        names=c("young", "middle", "older"),
        xlab="Age grounp", ylab="percentage", col = "Bisque")

#.... 1 check young people
boxdata <- boxplot(young_age, xlab="young", ylab="percentage")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(young_age==boxdata$out[i]),pos=4, cex=1)}
EU_result[323,]
#.......SEE DETAILS OF OUTLIERS....
for(i in 1:length(boxdata$group)){
  print(EU_result[EU_result$young_age==boxdata$out[i],
                  c("LA_Name","LA_code","young_age")])
}

#outlier is Tower Hamlets which has 46% percent of young people

# ......2 heck middle ages
boxdata <- boxplot(middle_age, xlab="middle age", ylab="percentage")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(middle_age==boxdata$out[i]),pos=4, cex=1)}
EU_result[320,]
EU_result[176,]
EU_result[209,]

#.......SEE DETAILS OF OUTLIERS....
for(i in 1:length(boxdata$group)){
  print(EU_result[EU_result$middle_age==boxdata$out[i],
                  c("LA_Name","LA_code","middle_age")])
}

# result 320=richmond upon thames 26% middle age, 176= North Norfolk 17% middle age
#209 west somerset= 16 percent middle age

#....3 check older age
boxdata <- boxplot(older_age, xlab="older age", ylab="percentage")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(older_age==boxdata$out[i]),pos=4, cex=1)}

EU_result[209,]
EU_result[323,]

#.......SEE DETAILS OF OUTLIERS....
for(i in 1:length(boxdata$group)){
  print(EU_result[EU_result$older_age==boxdata$out[i],
                  c("LA_Name","LA_code","older_age")])
}

#  west Somerset have the highest older age percentage of 52%
# Tower hamlet 15% older age




#.......SEE DETAILS OF OUTLIERS....
for(i in 1:length(boxdata$group)){
  print(EU_result[EU_result$females==boxdata$out[i],
                  c("LA_Name","LA_code","females")])
}



# ... 5. Create a boxplot of deprivation variable
boxplot(hh_deprived,
        names=c("Deprivation"),
        xlab="deprived household", ylab="percentage", col = "Bisque")

# check deprivation household
boxdata <- boxplot(hh_deprived, xlab="deprived household", ylab="percentage")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(hh_deprived==boxdata$out[i]),pos=4, cex=1)}
#check result
EU_result[294,]

#.......SEE DETAILS OF OUTLIERS....
for(i in 1:length(boxdata$group)){
  print(EU_result[EU_result$hh_deprived==boxdata$out[i],
                  c("LA_Name","LA_code","hh_deprived")])
}
# city of London has highest deprivation of 33% because of high dimention 1 percentage
# expect to be surprised (exploratory data analysis book)

# ... 6. Create a boxplot of claimant variable
boxplot(claimant,
        names=c("claimant"),
        xlab="claimant per economically active pop", ylab="percentage", col = "Bisque")

# check claimant people
boxdata <- boxplot(claimant, xlab="claimant per working population", ylab="percentage")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(claimant==boxdata$out[i]),pos=4, cex=1)}
# check outlier

EU_result[281,]
EU_result[10, ]
EU_result[2, ]

# Birmingham and Kingston upon Hull and Middlesbrough 6% claimant per working population,

# ... 7. Create a boxplot of economically inactive variable
boxplot(econ_inactive,
        names=c("Economically inactive"),
        xlab="economically inactive", ylab="percentage", col = "Bisque")

# check economically inactive
boxdata <- boxplot(EU_result$econ_inactive, xlab="economically inactive percentage", ylab="percentage")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(econ_inactive==boxdata$out[i]),pos=4, cex=1)}
#.......SEE DETAILS OF OUTLIERS....
for(i in 1:length(boxdata$group)){
print(EU_result[EU_result$econ_inactive==boxdata$out[i],
                c("LA_Name","LA_code","econ_inactive")])
}




# check outlier

#


# ... 6. Create a boxplot of white ethnicity variable
boxplot(ethn_white,
        names=c("white ethnicity"),
        xlab="white ethnicity percentage", ylab="percentage", col = "Bisque")

# check 
boxdata <- boxplot(ethn_white, xlab="white ethnicity", ylab="percentage")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(ethn_white==boxdata$out[i]),pos=4, cex=1)}
# check outlier

EU_result[318,]
EU_result[298, ]
EU_result[319, ]
EU_result[302, ]

# result
#318 Newham white ethnicity only 29%, Brent 298 is 36%, Redbridge 319 42%, Ealing 302, 49%.

# ... 7. Create a boxplot of jobs density variable
#exclude outlier

boxplot(Jobsdensity,
        names=c("jobs density"),
        xlab="jobs density", ylab="density", col = "Bisque")

# check 
boxdata <- boxplot(Jobsdensity, xlab="jobs density", ylab="density")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(Jobsdensity==boxdata$out[i]),pos=4, cex=1)}
# check outlier

EU_result[294,]
#.......SEE DETAILS OF OUTLIERS....
for(i in 1:length(boxdata$group)){
  print(EU_result[EU_result$Jobsdensity==boxdata$out[i],
                  c("LA_Name","LA_code","jobsdensity")])
}




# ... 8. Boxplot of level4andabove qualification
boxplot(level4andabove,
        names=c("Level 4 and above"),
        xlab="Level 4 and above", ylab="percentage", col = "Bisque")

# check no qualification
boxdata <- boxplot(level4andabove, xlab="percent of level 4 and above qualification", ylab="percentage")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(level4andabove==boxdata$out[i]),pos=4, cex=1)}
# check outlier

EU_result[294,]

# City of London have 63% level 4 and above

# ... 9. Boxplot of no qualifications
boxplot(noqualifications,
        names=c("no qualification"),
        xlab="percent of no qualification", ylab="percentage", col = "Bisque")

# check no qualification
boxdata <- boxplot(noqualifications, xlab="percent of no qualifications", ylab="percentage")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(noqualifications==boxdata$out[i]),pos=4, cex=1)}
# check outlier

EU_result[294,]

# city of London has only 6% of its population with no qualification



# ... 10. Create a boxplot of social grade variable
boxplot(abhigher_interm,
        names=c("Higher and Intermediate "),
        xlab="Higher and Intermediate occupation", ylab="percentage", col = "Bisque")

# check social grade
boxdata <- boxplot(abhigher_interm, xlab="Higher and intermediate occupation", ylab="percentage")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(abhigher_interm==boxdata$out[i]),pos=4, cex=1)}
# check outlier
EU_result[281,]
EU_result[291, ]
EU_result[325, ]
# Birmingham and Leeds and Wandsworth

# ... 11. Create a boxplot of lowest grade
boxplot(unemployed_lwstgrade,
        names=c("unemployed and lowest grade "),
        xlab="lowest grade", ylab="percentage", col = "Bisque")

# check social grade
boxdata <- boxplot(unemployed_lwstgrade, xlab="lowest grade", ylab="percentage")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(unemployed_lwstgrade==boxdata$out[i]),pos=4, cex=1)}
# check outlier
#no outlier

# ... 12. Create a boxplot of property owned
boxplot(prop_Owned,
        names=c("own properties "),
        xlab="properties owned", ylab="percentage", col = "Bisque")

# check property owned
boxdata <- boxplot(prop_Owned, xlab="own properties", ylab="percentage")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(prop_Owned==boxdata$out[i]),pos=4, cex=1)}
# check outlier
EU_result[323,]
EU_result[305, ]

# 323 Tower hamlet own property only 21% while Hackeny own 24%

# ... 13. Create a boxplot of social renter
boxplot(socialrented,
        names=c("Social Renters "),
        xlab="Social Renters", ylab="percentage", col = "Bisque")

# check social rent
boxdata <- boxplot(socialrented, xlab="Social Renters", ylab="percentage")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(socialrented==boxdata$out[i]),pos=4, cex=1)}
# check outlier
EU_result[323,]
EU_result[305, ]
EU_result[312, ]
#323 tower hamlets 44% social rented, Hackeny 43% and Islington 41%.

# ... 14. Create a boxplot of crime
boxplot(crime_count,
        names=c("Crime "),
        xlab="crime", ylab="percentage", col = "Bisque")

# check crime
boxdata <- boxplot(crime_count, xlab="crime", ylab="percentage")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(crime_count==boxdata$out[i]),pos=4, cex=1)}
# check outlier
EU_result[294,]
EU_result[326,]
# City of London has the highes crime percentage 70% followed by westminister 22%

# ... 15. Create a boxplot of life satisfaction survey
boxplot(life_Sat,
        names=c("Life Satisfaction "),
        xlab="Life satisfaction", ylab="score", col = "Bisque")

# check life satisfaction
boxdata <- boxplot(life_Sat, xlab="Life Satisfaction", ylab="score")
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(life_Sat==boxdata$out[i]),pos=4, cex=1)}
# check outlier
EU_result[168,]
EU_result[179, ]

#168 North Kesteven scored highest life satisfaction of 8.31 while Corby scored the lowest at 6.86

# ... 16. Create a boxplot of gross disposable income

boxdata <- boxplot(g_disp_inc_2016, xlab="Gross disposable HH income", ylab="million pounds per household", col = "Bisque")

#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  text(boxdata$group[i], boxdata$out[i],
       which(g_disp_inc_2016==boxdata$out[i]),pos=4, cex=1)}
# check outlier
EU_result[313,]
EU_result[326,]
EU_result[300,]

#Kensington and Chelsea and Westminister have highest gross disposable household income 6 million
#Camden 5 million hh gross disposable income per household.

# 1.6 remove City of london and do box plot
EU_result<- EU_result[-c(294), ]







