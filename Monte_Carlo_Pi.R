#Estimating pi using Monte Carlo Method

#setting up required packages

install.packages("data.table")
library("data.table")

#Set a random seed

set.seed(0.234234)

#defining pi_estimate and n

pi_estimate=0
n = 0
m = 0

# Set plot parameters
par(mfrow = c(3, 2))

#Repeat code 5 times

for (i in 1:5) {
  
  #set n to 10, 100, 1000, 10000, 100000
  
  n[i] = 10^i
  
  #both x and y contain n random numbers
  
  x=runif(n[i])
  y=runif(n[i])
  
  #Setting z to the radius of a circle with center (0,0) also known as the origin
  
  z=sqrt(x^2+y^2)
  
  m[i]=length(which(z<=1))
  
  #Counts the number of points which are less than 1, divides by the number
  #of points to estimate pi/4, then multiplies by 4 to estimate pi.
  
  pi_estimate[i]=(m[i]/length(z))*4
  
  #plot all 5 graphs on one page, red points are within a unit of the origin
  #and blue points are outwith a unit of the origin
  
  #par(mfrow = c(2, 3)) <- this should be outside the loop
  dev.new(width=4, height=4, unit="cm") 
  plot(x[which(z<=1)],y[which(z<=1)],xlab="X",ylab="Y",main= paste("Monte Carlo,
  n=", n[i]), col='red')
  
  points(x[which(z>1)],y[which(z>1)],col='blue')
  
  #calculate the difference between the estimated pi and true value of pi
  
  pi_difference = pi - pi_estimate}

#Put the data into a table to view with ease

pi_data = data.table(n,pi_estimate,pi_difference)

