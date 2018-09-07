source('system_definitions.R')
################################################################
#returns F_i
################################################################
forward_matrix <-function(lambda,i,m)
{
  forward_matrix = matrix(0,nrow = m+1, ncol =m+1)
  for(j in 1:(m+1) )
  {
    j0 = j-1 #there is no index 0 in R so everything is moved by 1 
    val = 0;
    for(n in 0:j)
    {
      val = val + prob_n_arrivals(n+i+m-j0,lambda)*prob_n_departures(j0,n);
    }
    forward_matrix[j,m+1] = val;
  }
  forward_matrix
}

################################################################
# returns B_i
################################################################
backward_matrix <- function(lambda,i,m)
{
  backward_matrix = matrix(0,nrow= m+1, ncol =m+1)
  for(j in 1:(m+1))
  {
    j0 = j-1
    val = 0;
    for(n in 0:m)
    {
      val = val + prob_n_arrivals(n-m-i+j0,lambda)*prob_n_departures(m,n);
    }
    backward_matrix[m+1,j] = val;
  }
  backward_matrix
}
################################################################## 
# S_k matrix
###################################################################
S_matrix <- function(lambda,k,m)
{
  S_matrix = matrix(0,nrow=m+1, ncol = m+1);
  for(n in 0:m)
  {
    S_matrix[m+1,m+1] = S_matrix[m+1,m+1] + prob_n_arrivals(n+k,lambda)*prob_n_departures(m,n)
  }
  S_matrix
}

#####################################################################
# R0 matrix
######################################################################

R0_matrix <- function(lambda,m)
{
  R0_matrix = matrix(0,nrow=m+1,ncol=m+1)
  for(j10 in 1:(m+1))
  {
    for(j20 in 1:(m+1))
    {
      j1 = j10-1;
      j2 = j20-1
      val = 0;
      for(n in 0:j1)
      {
        val = val + prob_n_arrivals(n+j2-j1,lambda)*prob_n_departures(j1,n);
      }
      R0_matrix[j10,j20] = val;
    }
  }
  R0_matrix
}

######################################################################
# transition probability from state 'from' to state 'to'
######################################################################
prob_transition <- function(from,to,lambda,m,matrix_size,cumulative_prob )
{
  prob_transition = 0;
  in_service = min(from+to,m);
  in_waiting = max(0,from+to-m);
  if(to < matrix_size)
  {
    for(n in 0:in_service)
    {
      prob_transition = prob_transition + prob_n_departures(in_service,n)*prob_n_arrivals(n+to-from,lambda);
    }
  } else
  {
    for(n in 0:in_service)
    {
      temp = prob_n_departures(in_service,n)*(1-cumulative_prob[n+to-from]);
      prob_transition = prob_transition + temp;
    }
  }
    
  prob_transition
}