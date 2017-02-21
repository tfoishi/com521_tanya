#PC0. Load up your dataset as you did in Week 3 PC2.

library(readr)
week3_dataset_tanya <- read_csv("~/Documents/uwcom521-assignments/week_03/week3_dataset-tanya.csv")
View(week3_dataset_tanya)

tanya.week3<- week3_dataset_tanya

#PC1. If you recall from Week PC6, x and y seemed like they linearly related. We now have the 
#tools and terminology to describe this relationship and to estimate just how related they are. 
#Run a t.test between x and y in the dataset and be ready to interpret the results for the class.
t.test(tanya.week3$x, tanya.week3$y)

##Relationship is statistically significant at the p<.000 level.

#PC2. Estimate how correlated x and y are with each other.

cor(tanya.week3$x, tanya.week3$y)
##Idk, only that  @ .844387 they are strongly positively correlated with each other. 

#PC3. Recode your data in the way that I laid out in Week 3 PC7.

tmp.week3 <- week3_dataset_tanya

tmp.week3$i <- as.logical(tmp.week3$i)
tmp.week3$j <- as.logical(tmp.week3$j)
tmp.week3$k.factor <- factor(tmp.week3$k,
                                 levels=c(0,1,2,3),
                                 labels=c("none", "some", "lots", "all"))
#PC4. Generate a set of three linear models and be ready to intrepret the 
#coefficients, standard errors, t-statistics, p-values, 
#and {\displaystyle \mathrm {R} ^{2}} {\displaystyle \mathrm {R} ^{2}} for each:
yxlm1 <- lm(y ~ x, data = tmp.week3)
yxlm2.i <- lm(y ~ x + i, data = tmp.week3)
yxlm3.ij <- lm(y ~ x + i + j, data = tmp.week3)
yxlm4.ijk <- lm(y ~ x + i + j + k, data = tmp.week3) 

summary.lm(yxlm1)
summary.lm(yxlm2.i)
summary.lm(yxlm3.ij)
summary.lm(yxlm4.ijk)

summary(yxlm1)$r.squared
summary(yxlm2.i)$r.squared
summary(yxlm3.ij)$r.squared
summary(yxlm4.ijk)$r.squared

#PC5. Generate a set of residual plots for the final model 
#(c) and be ready to interpret your model in terms of each of these:

#(a) A histogram of the residuals.
plot(hist(resid(yxlm4.ijk)))


#(b) Plot the residuals by your values of x, i, j, and k (four different plots).

plot(tmp.week3$x, residuals(yxlm4.ijk))
plot(tmp.week3$i, residuals(yxlm4.ijk))
plot(tmp.week3$j, residuals(yxlm4.ijk))
plot(tmp.week3$k, residuals(yxlm4.ijk))

#(c) A QQ plot to evaluate the normality of residuals assumption.

qqnorm(residuals(yxlm4.ijk))

#PC6. Generate a nice looking publication-ready table with a series of fitted models and put them in a Word document.
install.packages("stargazer")
library("stargazer")
stargazer(yxlm4.ijk, type = "text")

#PC7. Load up the dataset once again and fit the following linear models and be ready to interpret them similar 
#to the way you did above in PC4:

Halloween2012.2014.2015_PLOS <- read.delim("~/Downloads/Halloween2012-2014-2015_PLOS.tab")
View(Halloween2012.2014.2015_PLOS)

#(a) {\displaystyle {\widehat {\mathrm {fruit} }}=\beta _{0}+\beta _{1}\mathrm {obama} +\varepsilon } 
#{\displaystyle {\widehat {\mathrm {fruit} }}=\beta _{0}+\beta _{1}\mathrm {obama} +\varepsilon }

fruit <- Halloween2012.2014.2015_PLOS$fruit
obama <- Halloween2012.2014.2015_PLOS$obama
fruit.obama <- lm(fruit ~ obama, data = Halloween2012.2014.2015_PLOS)
fruit.obama
summary(fruit.obama)

#model doesn't really explain variability; variables aren't sig

#(b) Add a control for age and a categorical version of a control for year to the model in (a).


age <- Halloween2012.2014.2015_PLOS$age
year <- as.factor(Halloween2012.2014.2015_PLOS$year)
control.for.age <- lm(fruit ~ obama + age + year, data = Halloween2012.2014.2015_PLOS)
control.for.age
summary(control.for.age)

#model doesn't really explain variability; variables aren't sig

#PC8. Take a look at the residuals for your model in (a) and try to interpret these as you would in PC4 above. 
#What do you notice?

hist(resid(fruit.obama))
plot(resid(fruit.obama))
qqnorm(resid(fruit.obama))

#PC9. Run the simple model in (a) three times on three subsets of the dataset: 
#just 2012, 2014, and 2015. Be ready to talk through the results.
d.2012 <- subset(Halloween2012.2014.2015_PLOS, year == 2012)
d.2014 <- subset(Halloween2012.2014.2015_PLOS, year == 2014)
d.2015 <- subset(Halloween2012.2014.2015_PLOS, year == 2015)

fruit.2012 <- d.2012$fruit
obama.2012 <- d.2012$obama
fruit.2014 <- d.2014$fruit
obama.2014 <- d.2014$obama
fruit.2015 <- d.2015$fruit
obama.2015 <- d.2015$obama

fruit.obama.2012 <- lm(fruit.2012 ~ obama.2012, data = d.2012)
fruit.obama.2012
summary(fruit.obama.2012)

fruit.obama.2014 <- lm(fruit.2014 ~ obama.2014, data = d.2014)
fruit.obama.2014
summary(fruit.obama.2014)

fruit.obama.2015 <- lm(fruit.2015 ~ obama.2015, data = d.2015)
fruit.obama.2015
summary(fruit.obama.2015)
