# The aim is to find the stationary distribution
require("markovchain")
source('system_definitions.R')
source('matrix_definitions.R')
#lambda_vec = seq(0.2,0.6,length.out = 10);

#for(lambda in lambda_vec)
#{
  
#}
lambda = 0.3

buffer_size = 40
m = 10 # a total of buffer_size + m jobs can be in the system, at max m can be in service in at any moment.
matrix_size = m+buffer_size+1;
P = matrix(0, nrow = matrix_size, ncol = matrix_size);
cumulative_prob = rep_len(0,2*matrix_size) # index i contains probability that there are less than i arrivals (not less than equal to)
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

A = diag(matrix_size) - P;
b = matrix(0,nrow = matrix_size, 1)
x = solve(A,b)

