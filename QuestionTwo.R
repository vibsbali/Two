#Student Vaibhav Bali 01086800 
#Question 2

install.packages("data.table")
require("data.table")

#the following is to remove scientific notation
options(scipen=1)

#read five_min_prices.rds
five_min_prices <- readRDS("five_min_prices.rds")
#--------------------------------
#part a ends here
#--------------------------------

#convert machine times to calendar time objects
time <- five_min_prices[["TIME"]]
time_stamps <- as.POSIXct(time, origin="1970-01-01", tz = "GMT")

# set up the plot 
plot(time_stamps, five_min_prices[["BTC_ETH_last"]], type = "l", 
     xlab = "Time", ylab = "Prices", main = "Etherium Price Plot",
     cex.main =2, cex.lab=1.5, cex.axis=0.7)
#--------------------------------
#part b ends here
#--------------------------------

#install packages and calculate mean, variance.. etc
install.packages("e1071")
require("e1071")

diff_price_BTC_ETH_last <- diff(five_min_prices[["BTC_ETH_last"]])
print(mean(diff_price_BTC_ETH_last))
print(var(diff_price_BTC_ETH_last))
print(skewness(diff_price_BTC_ETH_last))
print(kurtosis(diff_price_BTC_ETH_last))
#--------------------------------
#part c ends here
#--------------------------------

#Plot empirical quantiles with standard normal 
qqnorm(diff_price_BTC_ETH_last, main = "Q-Q Plot", xlab = "Standard Normal Quantiles",
       ylab = "Emperical Quantiles")
qqline(diff_price_BTC_ETH_last, col=c("red"), lwd=2)
#--------------------------------
#part d ends here
#--------------------------------

#Determine the 0.001 and 0.999 emperical quantile
qLowerEnd <- quantile(diff_price_BTC_ETH_last, 0.001)
qUpperEnd <- quantile(diff_price_BTC_ETH_last, 0.999)
plot(density(diff_price_BTC_ETH_last), xlim=c(qLowerEnd, qUpperEnd),
     main="Empirical and Normal Density", lwd=2, col="black", xlab="Prices")

#x <- rnorm(length(diff_price_BTC_ETH_last), mean = mean(diff_price_BTC_ETH_last), sd = sd(diff_price_BTC_ETH_last))
x <- rnorm(10000000, mean = mean(diff_price_BTC_ETH_last), sd = sd(diff_price_BTC_ETH_last))
lines(density(x), col="red", lwd=1)

legend(0, 6000, c("Empirical"), col=cols, bty="n", text.col = "black")
legend(0.0001, 2000, c("Normal"), col=cols, bty="n", text.col = "red")

#average of all increments, which exceed 0.99 quantile
mean(diff_price_BTC_ETH_last > quantile(diff_price_BTC_ETH_last, 0.99))
quantile(diff_price_BTC_ETH_last, 0.99)


