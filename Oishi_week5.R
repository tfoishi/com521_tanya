#PC 0 
com521_population <- read.delim("/var/folders/h1/xwx8p4rj3jj2d27g189wrksr0000gn/T//Rtmpt49Ovv/data14cc7539aae7")
mean(com521_population$x)
#PC 1 compute SE
#first, compute SE by hand 
sd(week2.dataset)/sqrt(length(week2.dataset))

mean(week2.dataset) + (1.96 * sd(week2.dataset)/sqrt(length(week2.dataset))) * c(-1,1) 
t.test(week2.dataset)
#next, use the built in formula for confidence interval

t.test(com521_population$x, conf.level = 0.95)
#they are not the same because t.test is nt=ot assuming normal distribution, assumpting t-distribution. 
#Yes, true mean is inside confidence interval.

##PC2 Let's look beyond the mean. Compare the distribution from your sample of x to the true population. 
##Draw histograms and compute other descriptive and summary statistics. 
##What do you notice? Be ready to talk for a minute or two about the differences.
summary(week2.dataset)
summary(com521_population$x)
hist(week2_dataset)
hist(com521_population$x)
#histogram com521 looks to fit the normal distribution whereas week2.dataset slightly less so (though the general shape is there)

# PC3 
##Compute the mean of y from the true population and then create the mean and confidence interval from the y in your sample. 
##Is it in or out?
mean(com521_population$y)
t.test(com521_population$y)
#I don't think we have a Y variable in the week 2 dataset--were we meant to look at week3? in which case:
mean(week3_dataset.tanya$y)
t.test(week3_dataset.tanya$y)
#the mean of week3_dataset.tanya$y is outside of the confidence interval of com521_population$y

# PC 4
##I want you to run a simple simulation that demonstrates one of the most fundamental insights of statistics:
##(a) Create a vector of 10,000 randomly generated numbers that are uniformly distributed between 0 and 9.
random.vector <- runif( n=10000, min=0, max=9)
##(b) Take the mean of that vector. Draw a histogram.
mean(random.vector)
hist(random.vector)
##(c) Create 100 random samples of 2 items each from your randomly generated data and take the mean of each sample. 
##Create a new vector that contains those means. Describe/display the distribution of those means.
random.samples<- function(i) {
  sample1<- sample(random.vector, 2)
  mean(sample1)
}

random.sample.means <- sapply(rep(1, 100), random.samples)

summary(random.sample.means)
hist(random.sample.means)

##(d) Do (c) except make the items 10 items in each sample instead of 2. 
random.samples.10 <- function(i) {
  sample1<- sample(random.vector, 10)
  mean(sample1)
}
means10<- sapply(rep(1, 100), random.samples.10)

##Then do (c) again except with 100 items. Be ready to describe how the histogram changes as the sample size increases. 
##(HINT: You'll make me very happy if you write a function to do this.)
random.samples.100 <- function(i) {
  sample1 <- sample(random.vector, 100)
  mean(sample1)
}

means100 <- sapply(rep(1, 100), random.samples.100)

summary(random.sample.means)
summary(means10)
summary(means100)

hist(random.sample.means)
hist(means10)
hist(means100)

#PC5. 
##Do PC4 again but with random data drawn from a normal distribution (mu = 42, sigma = 42)
##instead of a uniform distribution. How are you results different than in PC4?
random.vector.2 <- rnorm(n=10000, mean=42, sd=42)

mean(random.vector.2)
hist(random.vector.2)

random.samples<- function(i) {
  sample1<- sample(random.vector.2, 2)
  mean(sample1)
}

random.sample.means <- sapply(rep(1, 100), random.samples)

summary(random.sample.means)
hist(random.sample.means)
##(d) Do (c) except make the items 10 items in each sample instead of 2. 
random.samples.10 <- function(i) {
  sample1<- sample(random.vector.2, 10)
  mean(sample1)
}
means10<- sapply(rep(1, 100), random.samples.10)

##Then do (c) again except with 100 items. Be ready to describe how the histogram changes as the sample size increases. 
##(HINT: You'll make me very happy if you write a function to do this.)
random.samples.100 <- function(i) {
  sample1 <- sample(random.vector,2, 100)
  mean(sample1)
}

means100 <- sapply(rep(1, 100), random.samples.100)

summary(random.sample.means)
summary(means10)
summary(means100)

hist(random.sample.means)
hist(means10)
hist(means100)
