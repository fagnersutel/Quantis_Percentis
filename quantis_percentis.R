lista = c(2,3,6,7,9,10,13,15,24,25,30,37)
quantile(lista, probs = c(0, 0.25, 0.50, 0.75)) 
quantile(lista, probs = c(0, 0.20, 0.40, 0.60, 0.80)) 
quantile(lista, probs = 0.82) 
hist(lista)
boxplot(lista)

lista = rnorm(250)
head(lista, 25)
quantile(lista, probs = c(0, 0.25, 0.50, 0.75)) 
quantile(lista, probs = c(0, 0.20, 0.40, 0.60, 0.80)) 
quantile(lista, probs = 0.82) 
quantile(lista)
hist(lista)


set.seed(1)
Fund <- matrix(rnorm(20*10), ncol=20, nrow=10)
Fund
qs <- apply(Fund, 2, quantile, probs=c(0.05, 0.5, 0.95))
qs
ylim=range(qs)
plot(seq(ncol(Fund)), qs[1,], t="l", lty=2, ylim=ylim) #5%
lines(seq(ncol(Fund)), qs[2,], lty=1, lwd=2) #50%
lines(seq(ncol(Fund)), qs[3,], lty=2, col=2)  #95%
legend("topleft", legend=rev(rownames(qs)), lwd=c(1,2,1), col=c(2,1,1), lty=c(2,1,2))

boxplot(lista)

library(car)
x<-rchisq(100, df=2)
qqPlot(x)
qqPlot(x, dist="chisq", df=2)
qqPlot(lista)

qqnorm(x, pch = 1, frame = FALSE)
qqline(x, col = "steelblue", lwd = 2)

qqnorm(lista, pch = 1, frame = FALSE)
qqline(lista, col = "steelblue", lwd = 2)

install.packages('VGAM')
library(VGAM)
fit4 <- vgam(BMI ~ s(age, df = c(4, 2)), lms.bcn(zero = 1), data = bmi.nz, trace = TRUE)
qtplot(fit4, percentiles = c(5,50,90,99), main = "Quantiles", las = 1, 
       xlim = c(15, 90), ylab = "BMI", lwd = 2, lcol = 4, 
       pcol.arg="transparent")


#https://data.library.virginia.edu/getting-started-with-quantile-regression/
x <- seq(0,100,length.out = 100)        # independent variable
sig <- 0.1 + 0.05*x                     # non-constant variance
b_0 <- 6                                # true intercept
b_1 <- 0.1                              # true slope
set.seed(1)                             # make the next line reproducible
e <- rnorm(100,mean = 0, sd = sig)      # normal random error with non-constant variance
y <- b_0 + b_1*x + e                    # dependent variable
dat <- data.frame(x,y)
library(ggplot2)
ggplot(dat, aes(x,y)) + geom_point()
ggplot(dat, aes(x,y)) + geom_point() + geom_smooth(method="lm")
