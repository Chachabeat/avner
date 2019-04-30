n <- ncol(RAA)
m <- nrow(RAA)
cdf <- c()

for(i in 1:(n-1){
  cdf <- c(cdf, sum(RAA[1:(m-i), i+1]) / sum(RAA[1:(m-i), i]))
}

f <- sapply(1:(n-1),
            function(i){
              sum(RAA[c(1:(n-i)),i+1])/sum(RAA[c(1:(n-i)),i])
            }
)

dev_period <- 1:(n-1)
plot(log(f-1) ~ dev_period, main = "Log-linear cumulative development factor v. development period")
tail <- lm(log(f-1) ~ dev_period)
abline(tail)
co <- coef(tail)
tail_ext <- exp(co[1] + c(n:(n + 100)) * co[2]) + 1
f_tail <- prod(tail_ext)

#Plot cumulative development factors
plot(100*(rev(1/cumprod(rev(c(f, tail[tail>1.0001]))))), t="b",
     main="Expected claims development pattern",
     xlab="Dev. period", ylab="Development % of ultimate loss")
