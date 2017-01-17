#PC 4
#Next time do more notes. 
summary(week3_dataset.tanya)
ncol(week3_dataset.tanya)
nrow(week3_dataset.tanya)

sd(week3_dataset.tanya$x)
sd(week3_dataset.tanya$y)
var(week3_dataset.tanya$x)
var(week3_dataset.tanya$y)

hist(week3_dataset.tanya$x)
hist(week3_dataset.tanya$j)
hist(week3_dataset.tanya$i)
hist(week3_dataset.tanya$k)
hist(week3_dataset.tanya$y)

table(week3_dataset.tanya$j)
table(week3_dataset.tanya$i)
table(week3_dataset.tanya$k)

#PC5
summary(week3_dataset.tanya$x)
summary(week2.dataset)

#PC6
week3_dataset.tanya$k <-factor(week3_dataset.tanya$j)
week3_dataset.tanya$i <-as.logical(week3_dataset.tanya$i)
week3_dataset.tanya$j<- as.logical(week3_dataset.tanya$j)
ggplot(data=week3_dataset.tanya) + geom_point() + aes(x=x, y=y, color=i, shape=j,size=k)


#PC 7
levels(week3_dataset.tanya$k)[levels(week3_dataset.tanya$k)=="3"] <- "all" 
levels(week3_dataset.tanya$k)[levels(week3_dataset.tanya$k)=="2"] <- "lots" 
levels(week3_dataset.tanya$k)[levels(week3_dataset.tanya$k)=="1"] <- "some" 
levels(week3_dataset.tanya$k)[levels(week3_dataset.tanya$k)=="0"] <- "none" 
week3_dataset.tanya$k

#PC 8
week3_dataset.tanya[week3_dataset.tanya$i == 0, "i"]<-NA
week3_dataset.tanya[is.na(week3_dataset.tanya$i)]<-0

#PC 9
#now they are frequency 
summary(week3_dataset.tanya)
ggplot(data=week3_dataset.tanya) + geom_point() + aes(x=x, y=y, color=i, shape=j,size=k)
