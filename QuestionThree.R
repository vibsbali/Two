
#-- Provided Code
set.seed(10) #initialize pseudo random generator for reproducibility
n<-3000; p=100
basis_functions<-list(sin, cos, dnorm, pnorm)
arguments<-seq(from=0, to=1, length=p)
function_values<-matrix(data=0, nrow=length(basis_functions), ncol=length(arguments))
for (i in 1:length(basis_functions)) function_values[i,]<-basis_functions[[i]](arguments)
random_coefficients<-matrix(data=0, nrow=n, ncol=length(basis_functions))
random_coefficients[,]<-rnorm(nrow(random_coefficients)*ncol(random_coefficients), mean=3, sd=1)
X<-random_coefficients%*%function_values
#-- Provided code ends here

plot(x = arguments, y = X[1,], ylim=c(2, 10), type='l', col = "deepskyblue"
     , xlab = "Arguments", ylab = "First 20 rows", main = "Arguments / Rows")
lines(y = X[2,], type = 'l', x = arguments, col = "aquamarine4", lwd=2)
lines(y = X[3,], type = 'l', x = arguments, col = "deepskyblue2", lwd=2)
lines(y = X[4,], type = 'l', x = arguments, col = "bisque3",lwd=2)
lines(y = X[5,], type = 'l', x = arguments, col = "deepskyblue3",lwd=2)
lines(y = X[6,], type = 'l', x = arguments, col = "dodgerblue",lwd=2)
lines(y = X[7,], type = 'l', x = arguments, col = "dodgerblue1",lwd=2)
lines(y = X[8,], type = 'l', x = arguments, col = "pink",lwd=2)
lines(y = X[9,], type = 'l', x = arguments, col = "red",lwd=2)
lines(y = X[10,], type = 'l', x = arguments, col = "purple",lwd=2)
lines(y = X[11,], type = 'l', x = arguments, col = "cyan",lwd=2)
lines(y = X[12,], type = 'l', x = arguments, col = "blueviolet",lwd=2)
lines(y = X[13,], type = 'l', x = arguments, col = "deepskyblue3",lwd=2)
lines(y = X[14,], type = 'l', x = arguments, col = "coral4",lwd=2)
lines(y = X[15,], type = 'l', x = arguments, col = "darkgreen",lwd=2)
lines(y = X[16,], type = 'l', x = arguments, col = "black",lwd=2)
lines(y = X[17,], type = 'l', x = arguments, col = "deepskyblue3",lwd=2)
lines(y = X[18,], type = 'l', x = arguments, col = "grey",lwd=2)
lines(y = X[19,], type = 'l', x = arguments, col = "darkcyan",lwd=2)
lines(y = X[20,], type = 'l', x = arguments, col = "darkgoldenrod4",lwd=2)
#--------------------------------
#part a ends here
#--------------------------------

#-- Transposing matrix X
y <- t(X)%*%X
dim(y)

vectorY <- eigen(y)

diff(vectorY$values)