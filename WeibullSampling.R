# Let h represent lambda
weibullinverse<-function(n,k,h) {
  
  if (n<=0 | k<=0 | h<=0) {
    stop("n,k,and h have to be greater than 0")
  }
  
  U<-runif(n)
  X<-h*(-log(1-U))^(1/k)
  return(X)
}

n<- 50000
k<- 3
h<- 7

samples<-weibullinverse(n,k,h)
builtinsamples<-rweibull(n, shape = k, scale = h)

builtinmean<- mean(rweibull(n, shape = k, scale = h))  
builtinvar<- var(rweibull(n, shape = k, scale = h))    

samplemean <- mean(samples)
samplevar <- var(samples)

message("Mean using built-in:", builtinmean, "\n")
message("Mean using sample:", samplemean, "\n")
message("Variance using built-in:", builtinvar, "\n")
message("Variance using sample:", samplevar, "\n")

#Comparing both mean and variance values calculated through the sample function and the built-in R function shows that my code is correct because the numbers are relatively close.

quantiles<- seq(0.1, 0.9, by = 0.1)  

quantilessample <- quantile(weibullinverse(n, k, h), quantiles)
quantilesbuiltin <- qweibull(quantiles, k, h)  

table<-data.frame(
  Quantiles = quantiles,
  Sample = quantilessample,
  Theoretical = quantilesbuiltin
)

print(table)

#Comparing the quantiles from the sample and the quantiles from the built-in also show that my code is correct with similar values again.

hist(samples, probability = TRUE, breaks = 100, col = "lightgreen", border = "black",
     main = "Weibull Samples",
     xlab = "x", ylab = "Density")
curve(dweibull(x, shape = k, scale = h), col = "darkblue", lwd = 3, add = TRUE) 


#The histogram shows the sample density and the curve shows the density using the built-in R function. The curve (PDF) overlaps the histogram (CDF) and shows that the code is correct for the sample.

