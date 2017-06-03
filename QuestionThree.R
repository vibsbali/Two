install.packages("geometry")
require("geometry")
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
#-- Get eigenvalues and vectors (These vectors are principal components) --
vectorY <- eigen(y)
eigenvectors <- vectorY$vectors
#-- Plot 3 first principle axes and plot them against arguments
plot(x = arguments, y= eigenvectors[,1:1], type = 'l', ylim=c(-.2,.2), col = "deepskyblue",
     xlab = "Arguments", ylab = "First 3 Principal Axis", main = "Arguments / Principal Axis")
lines(x = arguments, y = eigenvectors[,2:2], col = "aquamarine4")
lines(x = arguments, y = eigenvectors[,3:3], col = "dodgerblue")

#--------------------------------
#part b ends here
#--------------------------------

#-- I am also creating a covariance matrix and creating principal components --
# -- My Alternative Solution to the problem ----------
covar <- cov(X)
eigenOfCovar <- covar$vector
plot(x = arguments, y= eigenOfCovar$vector[,1:1], type = 'l', ylim=c(-.2,.2), col = "deepskyblue",
     xlab = "Arguments", ylab = "First 3 Principal Axis", main = "Arguments / Principal Axis")
lines(x = arguments, y= eigenOfCovar$vector[,2:2], col = "aquamarine4")
lines(x = arguments, y= eigenOfCovar$vector[,3:3], col = "dodgerblue")
# ---------------------------------

#--------------------------------
firstRow <- X[1,]
dot1 <- dot(firstRow, eigenvectors[,1:1])
dot2 <- dot(firstRow, eigenvectors[,2:2])
dot3 <- dot(firstRow, eigenvectors[,3:3])

projection1 <- dot1 * eigenvectors[,1:1]
projection2 <- dot1 * eigenvectors[,1:1] + dot2 * eigenvectors[,2:2]
projection3 <- dot1 * eigenvectors[,1:1] + dot2 * eigenvectors[,2:2] + dot3 * eigenvectors[,3:3]


plot(x = arguments, y = projection1, type = 'l', col = "black",
     xlab = "Arguments", ylab = "First 3 Projections", main = "Arguments / Projections")
lines(x = arguments, y = projection2, type = 'l', col = "red")
lines(x = arguments, y = projection3, type = 'l', col = "blue")

legend(0.8, 6.5, c("1st Projection in black"), col=cols, bty="n", text.col = "black")
legend(0.8, 6.25, c("2nd Projection in red"), col=cols, bty="n", text.col = "red")
legend(0.8, 6.0, c("3rd Projection in blue"), col=cols, bty="n", text.col = "blue")
#--------------------------------
#part c ends here
#--------------------------------

