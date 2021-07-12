plot(seq(0.1,2.5,0.1), exp(seq(0.1,2.5,0.1)), type="b", pch="×"); abline(h=5)
log(5) #exp(1)^1.6

5^(1/exp(1))





plot(density(
  rpois(100, lambda = 1)
))
rpois(100, lambda = 1)

lambda <- seq(0.1,2,0.1)
x <- 5

#Poission
#chance that x equals to 5 at different lambdas
#lambda describes the poisson distr
plot(lambda,
lambda^x / factorial(x) * exp(1)^-lambda
)

#confint for poisson! confint
lambda <- 14 ; n <- 1000
x <- rpois(n, lambda = lambda)
exp(confint(glm(x ~ 1, family=poisson)))
mean(x)
#formula?
exp(log(lambda-sqrt(1/(n*lambda))))
exp(log(lambda+sqrt(1/(n*lambda))))
#simplified formula - close to the confint function
lambda - 1.96*sqrt(lambda/n) ; lambda + 1.96*sqrt(lambda/n)



#Bernoulli
p <- 0.4
p <- seq(0.01,0.99,0.01)
n <- seq(100,10000,100)
#error margin of a bernoulli
plot(sqrt(p*(1-p)/n))
plot(sqrt(p*(1-p)/n[10]))



N<-10000 ; p<-0.7
for (i in c(1:10)) {
  print(sd(rbinom(N,1,p))^2)
}

(1-p)*p


sd(0:2)
mad(c(1:9))


ser <- sort(rnorm(10^6,20,5))
plot(ser, pch=".")
points(ser^2, pch=".", col="blue")
points(ser^3, pch=".", col="red")
points(ser^4, pch=".", col="yellow")
mean(ser)
mean(ser^2)
mean(ser^3)
mean(ser^4)



sum(ser^2) / sum(ser) 
sum(ser)   / 10^6
sum(ser^2) / 10^6
sum(ser^3) / 10^6
sum(ser^4) / 10^6

sd(ser)
sd(ser^2)
sd(ser^3)
sd(ser^4)

ser <- c(2,9,16)
mean(ser) ; sd(ser)
mean(ser^2)

N<-10^6 ; mu <- 20 ; sigma <- 5
ser <- sort(rnorm(N,mu,sigma))
mean(ser)

#https://en.wikipedia.org/wiki/Normal_distribution #Hermite polynomials 1-8 orders
mean(ser^2)
mu^2 + sigma^2



mean(ser^3)
mu^3 + 3*mu*sigma^2

mean(ser^4)
mu^4 + 6*mu^2*sigma^2 + 3*sigma^4



N<-10^6 ; mu <- 0 ; sigma <- 3
ser <- sort(rnorm(N,mu,sigma))
mean(ser)
mean(ser^2)
sd(ser)
sd(ser^2)
var(ser^2)
2^5
3^5

sd(ser^3)
sd(ser^4)



N1 <- rnorm(10^5,0,1)
N2 <- rnorm(10^5,0,1)

var(N1+N2)
var(N1*N2)
cov(N1,N1+N2)
cov(N1,N1*N2)


var(N1) + var(N2)
var(N1+N2)

min(N1) ; max(N1)


mu <- (2 + 8.5)/2
sigma <- 1
ser <- rnorm(10^7, mu, sigma)


length(ser[ser>4]) / 10^7
ser[log(ser)<=1] / 10^7

res <- log(ser)<=1
res[is.nan(res)]
sum(res,na.rm = T) / length(ser)
sum(log(ser)<=1, na.rm = T)


plot(sort(runif(10^5, 2, 8.5)))

var(runif(10^8, 2, 8.5))

ser <- runif(4*10^8, 2, 8.5)
sum(ser>4)/10^8
sum(log(ser)<=1)/10^8
var(ser)




U1 <- runif(10^7, 0, 1)
U2 <- runif(10^7, 0, 1)
mean(abs(U1-U2))

sum(U1==U2)
sum(U1<=U2)

U1 <- runif(10^5, 0, 1)
U2 <- runif(10^5, 0, 1)
plot(density(U1))
plot(density(U1+U2), ylim=c(0,2))
Umax <- ifelse(U1>U2,U1,U2)
points(density(Umax))

base <- 226
yoygrowth <- 1.15
years <- c(0:11)
plot(c(base*yoygrowth^years), type="b", col="blue", ylim=c(0,1200))
1*yoygrowth^years #duplicates every fifth year

#https://www.statista.com/statistics/203428/total-enterprise-software-revenue-forecast/
points(c(226,245,269,285,297,310,310,326,369,419,456,503), type="b", col="red")
#duplicated in 10 years; 60% growth in last 5 years
1.15^5; 1.6^-5; 1.099^5; 1.1^5



