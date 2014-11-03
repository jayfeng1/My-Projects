
#Read in file
movie.data = read.csv("currmoviedata.csv", header = T)
#Parse data for inaccurracies 
movie.data$summer <- as.factor(movie.data$summer)
movie.data1 <- movie.data[which(movie.data$WP.initiation >= 0),]
movie.data2 <- movie.data1[-36,]

#Subset data
movie.1000 = subset(movie.data2, movie.data2$N.theaters > 1000)
movie.summer = subset(movie.1000, movie.data2$season == "S")

nf <- layout(matrix(c(1,0,2:5,0,0,0,6,7,8,0,9,0),5,3,byrow=TRUE),rep.int(6,times= 3), rep.int(6,times=5))

#Plot graphs
par(mar = c(6,6,1,1))
plot(movie.data2$WP.view, movie.data2$revenue)
cor.plot = cor(movie.data2$WP.view, movie.data2$revenue)
linear.data = lm(revenue ~ WP.view, data = movie.data2)
abline(linear.data)

par(mar= c(6,6,1,1))
plot(movie.data2$WP.view, movie.data2$rev.theater)
par(mar= c(6,6,1,1))
plot(movie.data2$WP.initiation, movie.data2$revenue)
par(mar= c(6,6,1,1))
plot(movie.data2$N.theaters, movie.data2$revenue)
par(mar= c(6,6,1,1))
plot(movie.summer$N.theaters, movie.summer$revenue)
par(mar= c(6,6,1,1))
hist(movie.data2$N.theaters)

# Testing movies that were released in less than 1000 theaters
movie.small = subset(movie.data2, movie.data2$N.theaters < 1000)
movie.1000 = subset(movie.data2, movie.data2$N.theaters > 1000)
movie.100 = subset(movie.data2, movie.data2$N.theaters < 101)

par(mar= c(6,6,1,1))
hist(movie.100$N.theaters)
par(mar= c(6,6,1,1))
hist(movie.1000$N.theaters)
par(mar= c(6,6,1,1))
cor.store2=cor(movie.1000$WP.view, movie.1000$revenue)
plot(movie.1000$WP.view, movie.1000$revenue)
linear.1000 = lm(revenue ~ WP.view, data = movie.1000)
abline(linear.1000)
``` 
#Cross Validation and model testing in full code form/no packages
#Running through and comparing four different models involving amount of theaters, wikipedia
#article iniation date, season the movie is released in, and amount of wikipedia page views
samp.size <- nrow(movie.data2)
errors<- matrix(0, nrow=samp.size, ncol=4)

for(i in 1:samp.size) {
## Leave out the ith observation
  new.dat <- movie.data2[-i,] 

# Fit the four models to the new data and calculate "errors"
  new.lm1 <- lm(revenue ~ N.theaters + WP.initiation*WP.view, data=new.dat)
  pred <- with(movie.data2, predict(new.lm1, newdata=data.frame(N.theaters=N.theaters[i], WP.initiation=WP.initiation[i], WP.view=WP.view[i]), type='response', control=list(maxit=50)))

  errors[i,1] <- movie.data2$revenue[i] - pred

  new.lm2 <- lm(revenue ~ N.theaters + WP.initiation*WP.view+ summer, data=new.dat)
  pred <- with(movie.data2, 
  predict(new.lm2, newdata=data.frame(N.theaters=N.theaters[i], WP.initiation=WP.initiation[i], WP.view=WP.view[i], summer = summer[i]), 
  type='response', control=list(maxit=50)))
  
  errors[i,2] <- movie.data2$revenue[i] - pred
  
  new.lm3 <- lm(revenue ~ N.theaters +WP.view+ WP.initiation:WP.view, data=new.dat)
  pred <- with(movie.data2, 
  predict(new.lm3, 
  newdata=data.frame(N.theaters=N.theaters[i], 
  WP.initiation=WP.initiation[i], WP.view=WP.view[i], summer = summer[i], rev.theater = rev.theater[i]), 
  type='response', control=list(maxit=50)))
  
  errors[i,3] <- movie.data2$revenue[i] - pred
  
  new.lm4 <- lm(revenue ~ N.theaters+summer, data=new.dat)
  pred <- with(movie.data2, 
  predict(new.lm4, 
  newdata=data.frame(N.theaters=N.theaters[i], 
  WP.initiation=WP.initiation[i], WP.view=WP.view[i], summer = summer[i], rev.theater = rev.theater[i]), 
  type='response', control=list(maxit=50)))
  
  errors[i,4] <- movie.data2$revenue[i] - pred

}

cross.val = apply(errors, 2, function(x) mean(x^2))

* Model 1 = `r cross.val[1]`
* Model 2 = `r cross.val[2]`
* Model 3 = `r cross.val[3]`
* Model 4 = `r cross.val[4]`

# _Resampling Approach_  

## boot strap
library(boot)
boot.size = 10000
movie.reg.est <- function(data.frame, indices){
new.data = data.frame[indices,]
boot.model = lm(formula = revenue ~ N.theaters + WP.initiation*WP.view, 
data=new.data)
return(coef(boot.model))
}

movie.boot <-boot(data=movie.data2, statistic=movie.reg.est, R=boot.size)
movie.ci <- matrix(nrow=4,ncol=2,dimnames = list(c("N.Theater","WP.initiation","WP.view","WP.initiation:WP.view")))

row.names = rownames(movie.ci)

for(param in 2:5){
movie.ci[param-1,]=boot.ci(movie.boot, conf=.95, type="bca",index=param)$bca[4:5]
}

#Based on the model selection that has been processed, use "resampling subject" of `r boot.size` bootstrap samples to construct BCa confidence interval of the model. Since there are
#279 observed samples, it is assumed the model is asymptotically normal and acquire confidence interval from normal distribution. However unable to identify the CDF F of the data. 
#Therefore use of the resampling approach to ease the assumption of unknown CDF F.

* 95% CI for `r row.names[1]` = (`r movie.ci[1,1]`, `r movie.ci[1,2]`)
* 95% CI for `r row.names[2]` = (`r movie.ci[2,1]`, `r movie.ci[2,2]`)
* 95% CI for `r row.names[3]` = (`r movie.ci[3,1]`, `r movie.ci[3,2]`)
* 95% CI for `r row.names[4]` = (`r movie.ci[4,1]`, `r movie.ci[4,2]`)  
#Results show that except for the interaction between wikipedia page view and wikipedia page created day, has siginificance, which indicates that the model is good.


##For the discussion
model1 <- lm(revenue ~ N.theaters + WP.initiation*WP.view, data = movie.data2)
dis.pre <- numeric(4)

#Edge of Tomorrow

dis.pre[1] = predict(model1, newdata=data.frame(N.theaters=3490, WP.initiation=331, WP.view=1706983), type='response', control=list(maxit=50))

#The Faults in Our Stars
dis.pre[2] = predict(model1, newdata=data.frame(N.theaters=3173, WP.initiation=379, WP.view=1227000), type='response', control=list(maxit=50))
# w/ book
dis.pre[3] = predict(model1, newdata=data.frame(N.theaters=3173, WP.initiation=379, WP.view=3558691), type='response', control=list(maxit=50))

#22 Jump Street

dis.pre[4] = predict(model1, newdata=data.frame(N.theaters=4000, WP.initiation=370, WP.view=1208996), type='response', control=list(maxit=50))

