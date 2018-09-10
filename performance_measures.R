systemContentDistribution <- function(lambda,m,buffer_size)
{
  #a total of buffer_size + m jobs can be in the system, at max m can be in service in at any moment.
  matrix_size = m+buffer_size+1;
  P = matrix(0, nrow = matrix_size, ncol = matrix_size);
  
  # index i contains probability that there are less than i arrivals (not less than equal to)
  # i.e. cumulative_prob[i] = Pr(X < i)
  cumulative_prob = rep_len(0,2*matrix_size) 
  cumulative_prob[1] = prob_n_arrivals(0,lambda)
  for( i in 2:length(cumulative_prob))
  {
    cumulative_prob[i] = cumulative_prob[i-1] + prob_n_arrivals(i-1,lambda);
  }
  
  for(from in 1:matrix_size)
  {
    for( to in 1:matrix_size)
    {
      P[from,to] = prob_transition(from,to,lambda,m, matrix_size, cumulative_prob);
    }
  }
  
  nthP = P%^%1000;
  systemContentDistribution = nthP[1,1:matrix_size]
}

eSystemContent <- function(distribution)
{
  eSystemContent = 0;
  for(i in 1:length(distribution))
  {
    eSystemContent = eSystemContent + (i-1)*distribution[i];
  }
  eSystemContent = eSystemContent
}

eResponseTime <- function(lambda,distribution)
{
  blocking_probability = distribution[length(distribution)];
  eResponseTime = eSystemContent(distribution)/(lambda*(1-blocking_probability));
}

eBlockingProbability <-function(lambda,distribution)
{
  eBlockingProbability = distribution[length(distribution)];
}