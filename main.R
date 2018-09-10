# The aim is to find the stationary distribution
source('system_definitions.R')
source('matrix_definitions.R')
source('performance_measures.R')

lambda_vec = seq(0.3,0.8,length.out = 20);
buffer_size = 30
m = 10

eSystemC = {}
eResponseT = {}
eBlockingProb = {}
for(lambda in lambda_vec)
{
  distribution = systemContentDistribution(lambda,m,buffer_size)
  eSystemC = c(eSystemC, eSystemContent(distribution))
  eResponseT = c(eResponseT, eResponseTime(lambda,distribution) )
  eBlockingProb = c(eBlockingProb, eBlockingProbability(lambda,distribution))
}

filename = "SystemContent_lambda.pdf";
pdf(filename);
ylim = c(0,max(eSystemC));
plot(lambda_vec,eSystemC, type = "l", xlab = "arrival rate of requests", ylab = "Number of unserved requests",col= "blue", cex.lab=1.5, lwd=3.5, ylim = ylim );
#lines(rhos,result_vec, type = "l", xlab = "load", ylab = measure,col= plot_colors[j], cex.lab=1.5, lwd=2.5, lty = lty );
#legend(legend_loc[i],legend=tau_names,col=plot_colors, lty=1:3, cex=0.8, title="", text.font=4.5, bg='lightblue');
dev.off();

filename = "ResponseTime_lambda.pdf";
pdf(filename);
ylim = c(0,max(eResponseT));
plot(lambda_vec,eResponseT, type = "l", xlab = "arrival rate of requests", ylab = "Latency (s)",col= "blue", cex.lab=1.5, lwd=3.5, ylim = ylim );
dev.off()

filename = "Blockingprobability_lambda.pdf";
pdf(filename);
ylim = c(0,max(eBlockingProb));
plot(lambda_vec,eBlockingProb, type = "l", xlab = "arrival rate of requests", ylab = "Probability request rejected",col= "red", cex.lab=1.5, lwd=3.5, ylim = ylim );
dev.off()