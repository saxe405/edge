# the distribution of the arrival process
ArrivalGF <- function(z,lambda)
{
  p = prob_poisson();
  ArrivalGF = p*exp(lambda*(z-1)); 
  gamma = gammaparam()
  ArrivalGF = ArrivalGF + (1-p)*Liz(z,gamma)/Liz(1,gamma)
}

#############################################
# Return first derivative of arrival gf
#############################################
ArrivalGFDerivative <- function(z, lambda)
{
  p = prob_poisson()
  gamma = gammaparam();
  ArrivalGFDerivative = p*lambda*exp(lambda*(z-1))+(1-p)*Lizprime(z,gamma)/Liz(1,gamma)
}
#############################################
# Return second derivative of arrival gf
#############################################

ArrivalGF2d <- function(z,lambda)
{
  gamma = gammaparam();
  p = prob_poisson();
  ArrivalGF2d=p*(lambda^2)*exp(lambda*(z-1))+(1-p)*Liz2prime(z,gamma)/Liz(1,gamma)
}  

#############################################
# Return factorial of argument n
#############################################
factorial = function(n)
{
  if(n==0||n==1)
    factorial= 1
  else
    factorial = (n*factorial(n-1));
}

#probability of n arrivals in a single slot
prob_n_arrivals<- function(n, lambda)
{
  prob_n_arrivals = 0;
  if(n >= 0)
  {
    a = starting_powerlaw();
    p = prob_poisson();
    prob_n_arrivals = p*(exp(-lambda)*(lambda^n))/(factorial(n));
    gamma = gammaparam();
    if(n >=a )
    {
      prob_n_arrivals = prob_n_arrivals + (1/(n^gamma*Liz(1,gamma)))
    }
  }
  prob_n_arrivals
}

#probability of n departures in a single slot if j in service
prob_n_departures <- function(j, n)
{
  prob_n_departures = 0;
  if(n > 0)
  {
    prob_n_departures = j/n;
  }
  prob_n_departures
}
############################################
# Returns Li_gamma (z) the zeta function for power law distribution
############################################
Liz <- function(z,gamma)
{
  Liz = 0
  a = starting_powerlaw()
  for( i in (a:200)) #should go to infinity rather than 100 but 100 should be enought as abs(z) < 1
  {
    Liz = Liz + z^i/(i^gamma)
  }
  Liz
}
#############################################
# Lizprime derivative of Liz
#############################################
Lizprime <- function(z,gamma)
{
  Lizprime = 0
  a = starting_powerlaw()
  for( i in (a:200)) #should go to infinity rather than 100 but 100 should be enought as abs(z) < 1
  {
    Lizprime = Lizprime + i*z^(i-1)/(i^gamma)
  }
  Lizprime
}
#############################################
# Liz2prime second derivative of Liz
#############################################
Liz2prime <- function(z,gamma)
{
  Liz2prime = 0
  a = starting_powerlaw();
  for( i in (a:200)) #should go to infinity rather than 100 but 100 should be enought as abs(z) < 1
  {
    Liz2prime = Liz2prime + i*(i-1)*z^(i-2)/(i^gamma)
  }
  Liz2prime
}

#poisson probability for mixed distribution
poisson_prob <- function()
{
  poisson_prob = 0.995
}

# the starting point of the power law distribution ,i.e., a
starting_powerlaw <- function()
{
  starting_powerlaw = 25
}
gammaparam <- function()
{
  gammaparam = 3.5 # has to more than 3 for finite mean and variance
}

