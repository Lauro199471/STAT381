# number 1
data = c(78,66,65,63,60,60,58,56,52,50);
print(data);

# number 2
raw_car_matrix<-matrix( c(
                        1.8,1.5,2.0,2.5,1.8,2.5,1.6,1.5,  # Cylinder Volume
                        51,51,115,150,126,150,118,106     # Horsepower
                        ), ncol=2)
MD<-as.data.frame(raw_car_matrix)
rownames(MD)<-c("Honda Civic","Toyota Pruis","VW Golf"
                ,"VW Beatle","Toyota Corolla","VW Jetta",
                "Mini Cooper","Toyota Yaris")
colnames(MD)<-c("Cylinder Volume","Horsepower")
print(MD)


# number 3
# The CI_CheckU function is designed to create "M" confidence intervals for theta in the uniform distribution uses in question 4 of exam 1 for two purposes: 
# First, based on independent Uniform(0,theta) samples, take a look at what a sample of these intervals look like
# Second, identify what proportion of the "M" intervals actually contain the true value of theta, and compare that to the nominal confidence level, "1-alpha"
# The function has four inputs: "M"=number of intervals; "n"= sample size, it is fixed at one; "alpha"= complement of confidence level; "theta"=true value of parameter

CI_CheckU<-function(M,n=1,alpha,theta)
{
  #Initialize LB and UB as vectors with the c() notation
  LB<-c()
  UB<-c()
  #The "for loop" will enable us to automate the creation of intervals based on repeated samples of size n
  for (i in 1:M)
  {
    Y<-runif(n,0,theta) #This is our data, generates a random value from the uniform
    lb<-Y/(1-alpha/2) #As in question 4.b on Exam1, this is our lower bound
    ub<-Y/(alpha/2)
    LB<-c(LB,lb) #As the loop progresses from i=1 to i=M, the vector gets appended with lower bounds from successive intervals
    UB<-c(UB,ub) #Same as above, but for upper bounds
  }
  Theta_vec<-c(rep(theta,M)) #This is a vector that simply repeats the constant value of theta, M times
  a<-c(LB<Theta_vec) #A logical vector with value 1 when the inequality is true, 0 otherwise
  b<-c(UB>Theta_vec) 
  d<-a*b #A logical vector checking when both statements above are true
  ecl<-sum(d)/M #Empirical confidence level
  
  CI<-matrix(c(LB,UB,d),ncol=3) #Create a matrix with three columns: lower bounds, upper bounds, and yes/no indicator of whether interval captured theta
  CI_DataFrame<-as.data.frame(CI) #A nicer way to display your results are as a "data frame": Essentially a matrix with data and column headings
  colnames(CI_DataFrame)<-c("Lower Confidence Limit","Upper Confidence Limit","Captured?")
  
  print(c("Nominal Confidence Level",1-alpha)) #This will print a side-by-side comparison of the nominal confidence level (1-alpha), and the empirical one (i.e. what proportion of simulated intervals contained the true value of theta?)
  print(c("Empirical Confidence Level",ecl)) #This will print a side-by-side comparison of the nominal confidence level (1-alpha), and the empirical one (i.e. what proportion of simulated intervals contained the true value of theta?)
  
  head(CI_DataFrame)
}

# To create an exact 95% confidence interval for the mean mu from a normal population, 
#we set the values of constants under our control and compute according to known formula. 
# In addition to "M", these four will be your inputs for a fubction you will create called "CI_CheckN"

n  = 15000
mu = 10
sigma = 12
alpha = 0.05

xn<-rnorm(n,mu,sigma) #generates a 15-element vector of normally distributed random numbers

#CI
LB<-mean(xn)-qt((1-alpha/2),n-1)*sd(xn) #Lower Bound
UB<-mean(xn)+qt((1-alpha/2),n-1)*sd(xn) # Upper Bound

# Plot data 
hist(xn, col = "lightblue", main = "rnorm")
abline(v = mean(xn), col="red", lwd=4, lty=2)
abline(v = mean(LB), col="green", lwd=4, lty=2)
abline(v = mean(UB), col="green", lwd=4, lty=2)

c(LB,UB)

CI_CheckU(M = 2,n=15000,alpha = 0.05, theta = 12)
