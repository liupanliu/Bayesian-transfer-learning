library(MASS)			
library(rootSolve)			### solve roots for n non-linear equations
library(truncnorm)			### proposal dist. in Motropolis Hastings is truncated normal
library(Matrix)
library(glmnet)			    ### Ridge regression and logistic regression
library(SuppDists)      ### generate the invgauss 
library(mvtnorm)        ### generate the multivariate gauss distribution
#library(coda)   		  	### use Gelman-Rubin diagnostic to check the MCMC convergence
#library(MCMCpack)		  ### Gibbs sampling


### generate inverse gamma random numbers
rinvgamma = function (n, shape, scale = 1) 
{
  return(1/rgamma(n = n, shape = shape, rate = scale))
}


#### Generate AR(rho) covariance matrix
#### p: dimensionality of matrix
AR = function(rho, p)
{
  cov_design = array(0, dim = c(p,p))    #p*p 0 matrix    
  for(i in 1:p)
  {
    for(j in 1:i)
    {
      cov_design[i,j] = rho^(abs(i-j))
      cov_design[j,i] = cov_design[i,j]
    }
  }
  return(cov_design)                       
}



### Conduct Metropolis Hastings Algorithm to sample \eta (high dimension!!)
### f: a target function object. the sampling probility density function 
### value: the value form last step (old value)
### return a new value through Metropolis Hastings
M_eta = function(f, value)
{
  cand = mvrnorm(1, mu = value, Sigma = 0.0007*diag(length(value)))	    ###7sigma???
 
  while(f(cand) == 'NaN')
  {
    cand = mvrnorm(1, mu = value, Sigma = 0.0007*diag(length(value)))
  }
  #alpha = min(1, f(cand)/f(value))
  alpha = min(1, exp(f(cand)-f(value)))
  if(runif(1, min=0, max=1) < alpha) 
    { new_value = cand }else { new_value = value }
  return(new_value)
}



###transfer step
###X,y,Z denote the transferable data
TranBayes = function(X, y, Z, m, burn, tau1_0 = rep(1,ncol(X)+1), 
                    tau2_0 =rep(1,ncol(X)+1), tau3_0 = rep(1,ncol(X)+1), 
                    lambda1_0=1, lambda2_0=1, lambda3_0=1,alpha=0.05)      ###0.05
{
  n = nrow(X)
  if (!is.matrix(X)) {X = as.matrix(X)}
  p = ncol(X) + 1
  
  if(n != length(y) || n != length(Z))
  {
    print("number of data matrix rows is NOT equal to the number of responses")
    return(NULL)
  }
  if (m <= burn) 
  {
    print("Number of Gibbs sampling should be larger than burn number.")
    return(NULL)
  }
  index = which(Z == 1)				     ### the positions of 1 in vector Z
  
  ### re-arrange!! data matrix X and y so that the first n1 obs. correspond to Z = 1
  X = rbind(X[index,], X[-index,])
  X = cbind(rep(1, n), X)
  y = c(y[index], y[-index])
  
  n1 = length(index)				        ### number of obs. for discrete response equal to 1
  Z = c(rep(1, n1), rep(0, n-n1))	  ### re-arrange Z so that the first n1 obs. are 1
  mu1 = mean(y[1:n1])    			      # the sample mean for Z==1
  mu2 = mean(y[(n1+1):n]) 		     	# the sample mean for Z==0
  y[1:n1] = y[1:n1]-mu1      # only centered response y ??
  y[(n1+1):n] = y[(n1+1):n]-mu2
  
  ### Set matrices V1 and V2
  V1 = diag(Z)
  V2 = diag(rep(1, n)) - V1
  
  ### all parameters 
  sigma2 = numeric(m)  #vector with length m 
  lambda1=numeric(m)
  lambda2=numeric(m)
  lambda3=numeric(m)
  
  tau1=matrix(data = 0,nrow = m,ncol = p)
  tau2=matrix(data = 0,nrow = m,ncol = p)
  tau3=matrix(data = 0,nrow = m,ncol = p)
  beta1 = matrix(data = 0, nrow = m, ncol = p)  #have m rows, m iterations
  beta2 = matrix(data = 0, nrow = m, ncol = p) 
  eta = matrix(data = 0, nrow = m, ncol = p)
  
  
  ### Set initial values for parameters tau1, tau2 and tau3
  tau1[1,] = tau1_0
  tau2[1,] = tau2_0
  tau3[1,] = tau3_0
  
  ### Set initial values for parameters lambda1, lambda2 and lambda3
  lambda1[1]=lambda1_0
  lambda2[1]=lambda2_0
  lambda3[1]=lambda3_0
  
  ### Set initial values for parameters beta1, beta2 and sigma2(which is sigma^2 in the notes)
  #if(n1 > p)		### LSE
  #{
   # beta1[1,] = solve(t(X[1:n1,]) %*% X[1:n1,]) %*% t(X[1:n1,]) %*% y[1:n1]
  #}
  #if(n1 <= p)    	### use Ridge           
  #{
    glmmod1 = glmnet(X[1:n1,-1], y[1:n1], family = 'gaussian', alpha = 0)
    lambda_beta1 = cv.glmnet(X[1:n1,-1], y[1:n1], nfolds = 5, alpha = 0)$lambda.min
    beta1[1,] = as.vector(coef(glmmod1, s = lambda_beta1))
  #}                         
  
  #if((n - n1) > p)			### LSE
  #{
   #beta2[1,] = solve(t(X[-(1:n1),]) %*% X[-(1:n1),]) %*% t(X[-(1:n1),]) %*% y[-(1:n1)]
  #}
  #if((n - n1) <= p)			### use Ridge
  #{
    glmmod2 = glmnet(X[-(1:n1),-1], y[-(1:n1)], family = 'gaussian', alpha = 0)
    lambda_beta2 = cv.glmnet(X[-(1:n1),-1], y[-(1:n1)], nfolds = 5, alpha = 0)$lambda.min
    beta2[1,] = as.vector(coef(glmmod2, s = lambda_beta2))
  #}
  
    sigma2[1] = 1/n * (sum((y[1:n1] - X[1:n1,] %*% beta1[1,])^2) + sum((y[-(1:n1)] - X[-(1:n1),] %*% beta2[1,])^2))
  
  ### Set initial values for parameter eta
  glmmod3 = glmnet(X[,-1], Z, family="binomial", alpha = 0)
  lambda_eta = cv.glmnet(X[,-1], Z, family="binomial", nfolds = 5)$lambda.min
  eta[1,] = as.vector(coef(glmmod3, s = lambda_eta))	
  
  
  for(l in 2:m)
  {
    ### update parameters beta1 and beta2
    D1 = diag(tau1[l-1,])                      
    D2 = diag(tau2[l-1,])
    D3 = diag(tau3[l-1,])
    
    H1 = t(X) %*% V1 %*% X + diag(1/tau1[l-1,])
    H2 = t(X) %*% V2 %*% X + diag(1/tau2[l-1,])
    Vn.1 = rbind(H1, matrix(0, p, p))
    Vn.2 = rbind(matrix(0, p, p), H2)
    Vn = solve(cbind(Vn.1, Vn.2))
    post.Sigma = sigma2[l-1] * Vn			### the cov. matrix of the normal dist.
    post.mean = Vn %*% c(t(X) %*% V1 %*% y, t(X) %*% V2 %*% y)
    new_mu_beta = as.vector(mvrnorm(1, mu = post.mean, Sigma = post.Sigma))
    beta1[l,] = new_mu_beta[1:p]
    beta2[l,] = new_mu_beta[(p+1):(2*p)]
    
    ### update parameter sigma2 (sigma^2)
    temp1 = V1 %*% X %*% beta1[l,] + V2 %*% X %*% beta2[l,]
    temp2= beta1[l,]%*%diag(1/tau1[l-1,])%*%beta1[l,]+ beta2[l,]%*%diag(1/tau2[l-1,])%*%beta2[l,]
    sigma2[l] = rinvgamma(1, 0.5 * (n+2*p), 0.5 * (temp2+ sum((y - temp1)^2)))
    
    
    ### update parameter eta
    pdf_eta = function(eta)
    {
      temp = sum(Z*(X %*% eta)) - sum(log(1 + exp(X %*% eta)))
      #exp(-0.5 * (t(eta) %*% diag(1/tau3[l-1,]) %*% eta) +temp )
      (-0.5 * (t(eta) %*% diag(1/tau3[l-1,]) %*% eta)+temp) 
    }
    eta[l,] = M_eta(pdf_eta, eta[l-1,])

    #update the lambda  
    
    tempE1=colSums(tau1)
    tempE2=colSums(tau2)
    tempE3=colSums(tau3)
    
    lambda1[l]=sqrt(2*p/sum(tempE1/(l-1)) )
    lambda2[l]=sqrt(2*p/sum(tempE2/(l-1)) )
    lambda3[l]=sqrt(2*p/sum(tempE3/(l-1)) )
    
    
    ### update hyper parameters tau1, tau2 and tau3  
    for (i in 1:p)
    {
      tau1[l,i]=1/rinvGauss(1,lambda1[l]*sqrt(sigma2[l])/abs(beta1[l,i]),lambda1[l]^2)
      tau2[l,i]=1/rinvGauss(1,lambda2[l]*sqrt(sigma2[l])/abs(beta2[l,i]),lambda2[l]^2)
      tau3[l,i]=1/rinvGauss(1,lambda3[l]/abs(eta[l,i]),lambda3[l]^2)

	  while(!is.finite(tau1[l,i]))
	  {
		tau1[l,i]=1/rinvGauss(1,lambda1[l]*sqrt(sigma2[l])/abs(beta1[l,i]),lambda1[l]^2)
	  }
	  while(!is.finite(tau2[l,i]))
	  {
		tau2[l,i]=1/rinvGauss(1,lambda2[l]*sqrt(sigma2[l])/abs(beta2[l,i]),lambda2[l]^2)
	  }
	  while(!is.finite(tau3[l,i]))
	  {
		tau3[l,i]=1/rinvGauss(1,lambda3[l]/abs(eta[l,i]),lambda3[l]^2)
	  }
    }
    
  }
  
  beta1[,1] = beta1[,1] + mu1                                                  
  beta2[,1] = beta2[,1] + mu2 
  
  ###variable selection  
    nonsig_beta1 = nonsig_beta2 = nonsig_eta = NULL                          
    for(i in 2:p)
    {
      if(as.vector(unlist(apply(beta1[burn:m,],2,quantile,alpha/2)))[i] < 0 && as.vector(unlist(apply(beta1[burn:m,],2,quantile,1-alpha/2)))[i] > 0)
      {
        nonsig_beta1 = c(nonsig_beta1, i)   #record the 0 position
      }
      if(as.vector(unlist(apply(beta2[burn:m,],2,quantile,alpha/2)))[i] < 0 && as.vector(unlist(apply(beta2[burn:m,],2,quantile,1-alpha/2)))[i] > 0)
      {
        nonsig_beta2 = c(nonsig_beta2, i)
      }
      if(as.vector(unlist(apply(eta[burn:m,],2,quantile,alpha/2)))[i] < 0 && as.vector(unlist(apply(eta[burn:m,],2,quantile,1-alpha/2)))[i] > 0)
      {
        nonsig_eta = c(nonsig_eta, i)
      }
    }
    ###estimates of beta/eta
    beta1_hat=colMeans(beta1[burn:m,])
    beta2_hat=colMeans(beta2[burn:m,])
    eta_hat=colMeans(eta[burn:m,])
    
    beta1_hat[nonsig_beta1] = 0         #set nonsig variables as 0
    beta2_hat[nonsig_beta2] = 0
    eta_hat[nonsig_eta] = 0
    
  
  post.sample1 = list(mean_y = c(mu1, mu2),beta1_est=beta1_hat,beta2_est=beta2_hat,eta_est=eta_hat,  all_beta1 = beta1, all_beta2 = beta2, all_sigma2 = sigma2,all_eta = eta,all_tau1=tau1, all_tau2=tau2,all_tau3=tau3, all_lambda1=lambda1,all_lambda2=lambda2,all_lambda3=lambda3)
  post.sample1$call = match.call()                                           
  class(post.sample1) = "gibbs"
  return(post.sample1)   
}


###debias step
###Algorithm 2 - debiasing (X,y,Z denote target data, post.sample1 is the result of source)
TranDebias=function(X, y, Z, m, burn, post.sample1, xi1_0 = rep(1,ncol(X)+1), 
                   xi2_0 =rep(1,ncol(X)+1), xi3_0 = rep(1,ncol(X)+1), 
                   rho1_0=1, rho2_0=1, rho3_0=1,alpha=0.05)
{   n = nrow(X)
if (!is.matrix(X)) {X = as.matrix(X)}
p = ncol(X) + 1

if(n != length(y) || n != length(Z))
{
  print("number of data matrix rows is NOT equal to the number of responses")
  return(NULL)
}
if (m <= burn) 
{
  print("Number of Gibbs sampling should be larger than burn number.")
  return(NULL)
}
index = which(Z == 1)			       ### the positions of 1 in vector Z

### re-arrange!! data matrix X and y so that the first n1 obs. correspond to Z = 1
X = rbind(X[index,], X[-index,])
X = cbind(rep(1, n), X)
y = c(y[index], y[-index])

n1 = length(index)				        ###number of obs. for discrete response equal to 1
Z = c(rep(1, n1), rep(0, n-n1))	  ### re-arrange Z so that the first n1 obs. are 1


### Set matrices V1 and V2
V1 = diag(Z)
V2 = diag(rep(1, n)) - V1

#error with the parameters in algorithm 1   
error=y-V1%*%X%*%post.sample1$beta1_est-V2%*%X%*%post.sample1$beta2_est
mu1 = mean(error[1:n1])       # the sample mean for Z==1
mu2 = mean(error[(n1+1):n])      	# the sample mean for Z==0
error[1:n1] = error[1:n1] -mu1     
error[(n1+1):n] = error[(n1+1):n]-mu2

### all parameters 
varphi2 = numeric(m)  #vector with length m 
rho1=numeric(m)
rho2=numeric(m)
rho3=numeric(m)

xi1=matrix(data = 0,nrow = m,ncol = p)
xi2=matrix(data = 0,nrow = m,ncol = p)
xi3=matrix(data = 0,nrow = m,ncol = p)
delta1 = matrix(data = 0, nrow = m, ncol = p)  #have m rows, m iterations
delta2 = matrix(data = 0, nrow = m, ncol = p) 
deltaeta = matrix(data = 0, nrow = m, ncol = p)


### Set initial values for parameters xi1, xi2 and xi3
xi1[1,] = xi1_0
xi2[1,] = xi2_0
xi3[1,] = xi3_0

### Set initial values for parameters rho1, rho2 and rho3
rho1[1]=rho1_0
rho2[1]=rho2_0
rho3[1]=rho3_0

### Set initial values for parameters delta1, delta2 and varphi2(which is varphi^2 in the notes)
#if(n1 > p)		### LSE
#{
 # delta1[1,] = solve(t(X[1:n1,]) %*% X[1:n1,]) %*% t(X[1:n1,]) %*% error[1:n1]
#}
#if(n1 <= p)    	### use Ridge           
#{
  glmmod_1 = glmnet(X[1:n1,-1], error[1:n1], family = 'gaussian', alpha = 0)
  lambda_1 = cv.glmnet(X[1:n1,-1], error[1:n1], nfolds = 5, alpha = 0)$lambda.min
  delta1[1,] = as.vector(coef(glmmod_1, s = lambda_1))
#}                         

#if((n - n1) > p)			### LSE
#{
 # delta2[1,] = solve(t(X[-(1:n1),]) %*% X[-(1:n1),]) %*% t(X[-(1:n1),]) %*% error[-(1:n1)]
#}
#if((n - n1) <= p)			### use Ridge
#{
  glmmod_2 = glmnet(X[-(1:n1),-1], error[-(1:n1)], family = 'gaussian', alpha = 0)
  lambda_2 = cv.glmnet(X[-(1:n1),-1], error[-(1:n1)], nfolds = 5, alpha = 0)$lambda.min
  delta2[1,] = as.vector(coef(glmmod_2, s = lambda_2))
#}
varphi2[1] = 1/n * (sum((error[1:n1] - X[1:n1,] %*% delta1[1,])^2) + sum((error[-(1:n1)] - X[-(1:n1),] %*% delta2[1,])^2))

### Set initial values for parameter deltaeta
glmmod_3 = glmnet(X[,-1], Z, family="binomial", alpha = 0)
lambda_3 = cv.glmnet(X[,-1], Z, family="binomial", nfolds = 5)$lambda.min
deltaeta[1,] = as.vector(coef(glmmod_3, s = lambda_3))
  

for(l in 2:m)
{
  ### update parameters delta1 and delta2
  varxi1 = diag(xi1[l-1,])                      
  varxi2 = diag(xi2[l-1,])
  varxi3 = diag(xi3[l-1,])          
  H1 = t(X) %*% V1 %*% X + diag(1/xi1[l-1,]) 
  H2 = t(X) %*% V2 %*% X + diag(1/xi2[l-1,]) 
  Vn.1 = rbind(H1, matrix(0, p, p))
  Vn.2 = rbind(matrix(0, p, p), H2)
  Vn = solve(cbind(Vn.1, Vn.2))
  post.Sigma = varphi2[l-1] * Vn			### the cov. matrix of the normal dist.
  post.mean = Vn %*% c(t(X) %*% V1 %*% error, t(X) %*% V2 %*% error)
  new_mu_beta = as.vector(mvrnorm(1, mu = post.mean, Sigma = post.Sigma))
  delta1[l,] = new_mu_beta[1:p]
  delta2[l,] = new_mu_beta[(p+1):(2*p)]
  
  ### update parameter varphi2 (varphi^2)            
  temp1 = V1 %*% X %*% delta1[l,] + V2 %*% X %*% delta2[l,]
  temp2= delta1[l,]%*%diag(1/xi1[l-1,])%*%delta1[l,]+ delta2[l,]%*%diag(1/xi2[l-1,]) %*%delta2[l,]
  varphi2[l] = rinvgamma(1, 0.5 * (n+2*p), 0.5 * (temp2+ sum((error - temp1)^2)))
  
  
  ### update parameter deltaeta   
  pdf_deltaeta = function(deltaeta)
  {
    temp = sum(Z*(X %*% deltaeta)) - sum(log(1 + exp(X %*% (deltaeta+post.sample1$eta_est))))
    #exp(-0.5 * (t(deltaeta) %*% diag(1/xi3[l-1,])  %*% deltaeta) + temp)
    (-0.5 * (t(deltaeta) %*% diag(1/xi3[l-1,])  %*% deltaeta) + temp)
  }
  deltaeta[l,] = M_eta(pdf_deltaeta, deltaeta[l-1,])
  
  
  #update the rho
  
  tempE1=colSums(xi1)
  tempE2=colSums(xi2)
  tempE3=colSums(xi3)
  
  rho1[l]=sqrt(2*p/sum(tempE1/(l-1)) )
  rho2[l]=sqrt(2*p/sum(tempE2/(l-1)) )
  rho3[l]=sqrt(2*p/sum(tempE3/(l-1)) )
  
  
  ### update hyper parameters xi1, xi2 and xi3  
  for (i in 1:p)
  {
    xi1[l,i]=1/rinvGauss(1,rho1[l]*sqrt(varphi2[l])/abs(delta1[l,i]),rho1[l]^2)
    xi2[l,i]=1/rinvGauss(1,rho2[l]*sqrt(varphi2[l])/abs(delta2[l,i]),rho2[l]^2)
    xi3[l,i]=1/rinvGauss(1,rho3[l]/abs(deltaeta[l,i]),rho3[l]^2)

    while(!is.finite(xi1[l,i]))
	  {
		xi1[l,i]=1/rinvGauss(1,rho1[l]*sqrt(varphi2[l])/abs(delta1[l,i]),rho1[l]^2)
	  }
	  while(!is.finite(xi2[l,i]))
	  {
		xi2[l,i]=1/rinvGauss(1,rho2[l]*sqrt(varphi2[l])/abs(delta2[l,i]),rho2[l]^2)
	  }
	  while(!is.finite(xi3[l,i]))
	  {
		xi3[l,i]=1/rinvGauss(1,rho3[l]/abs(deltaeta[l,i]),rho3[l]^2)
	  }

  }
  
}

delta1[,1] = delta1[,1]+mu1                                             
delta2[,1] = delta2[,1]+mu2     

###variable selection
nonsig_delta1 = nonsig_delta2 = nonsig_deltaeta = NULL                          
for(i in 1:p)
{
  if(as.vector(unlist(apply(delta1[burn:m,],2,quantile,alpha/2)))[i] < 0 && as.vector(unlist(apply(delta1[burn:m,],2,quantile,1-alpha/2)))[i] > 0)
  {
    nonsig_delta1 = c(nonsig_delta1, i)   #record the 0 position
  }
  if(as.vector(unlist(apply(delta2[burn:m,],2,quantile,alpha/2)))[i] < 0 && as.vector(unlist(apply(delta2[burn:m,],2,quantile,1-alpha/2)))[i] > 0)
  {
    nonsig_delta2 = c(nonsig_delta2, i)
  }
  if(as.vector(unlist(apply(deltaeta[burn:m,],2,quantile,alpha/2)))[i] < 0 && as.vector(unlist(apply(deltaeta[burn:m,],2,quantile,1-alpha/2)))[i] > 0)
  {
    nonsig_deltaeta = c(nonsig_deltaeta, i)
  }
}
###estimates of delta/deltaeta
delta1_hat=colMeans(delta1[burn:m,])
delta2_hat=colMeans(delta2[burn:m,])
deltaeta_hat=colMeans(deltaeta[burn:m,])


delta1_hat[nonsig_delta1] = 0         #set nonsig variables as 0
delta2_hat[nonsig_delta2] = 0
deltaeta_hat[nonsig_deltaeta] = 0



post.sample = list(mean_y = c(mu1, mu2),delta1_est=delta1_hat,delta2_est=delta2_hat,deltaeta_est=deltaeta_hat,  all_delta1 = delta1, all_delta2 = delta2, all_deltaeta = deltaeta, all_varphi2 = varphi2, all_xi1=xi1, all_xi2=xi2, all_xi3=xi3, all_rho1=rho1,all_rho2=rho2,all_rho3=rho3)             
post.sample$call = match.call()                                           
class(post.sample) = "gibbs"
return(post.sample)   
  
  
}



###Source detection step (X,y,z denote target data!  X1,y1,z1 denote source data!!!  M denotes threshold)
##post.sample denotes estimates of target data!!!
istransferable=function(post.sample,X,y,z,X1,y1,z1,m,burn,M,alpha = 0.05)
{
result=numeric(5) 
#estimate_t=TranBayes(X,y,z,m,burn,tau1_0 = rep(1,ncol(X)+1), 
 #                    tau2_0 =rep(1,ncol(X)+1), tau3_0 = rep(1,ncol(X)+1), 
  #                   lambda1_0=1, lambda2_0=1, lambda3_0=1,alpha = 0.05)
beta_t1=post.sample$beta1_est
beta_t2=post.sample$beta2_est
eta_t=post.sample$eta_est
sigma2_t=mean(post.sample$all_sigma2[burn:m])
p=length(beta_t1)

##estimates of the coefficients of source data
estimate_s=TranBayes(X1,y1,z1,m,burn,tau1_0 = rep(1,ncol(X1)+1), 
                     tau2_0 =rep(1,ncol(X1)+1), tau3_0 = rep(1,ncol(X1)+1), 
                     lambda1_0=1, lambda2_0=1, lambda3_0=1,alpha = 0.05)
beta_s1=estimate_s$beta1_est
beta_s2=estimate_s$beta2_est
eta_s=estimate_s$eta_est
sigma2_s=mean(estimate_s$all_sigma2[burn:m])

### Set matrices V1 and V2
n = nrow(X)
X = cbind(rep(1, n), X)
X=as.matrix(X)
V1 = diag(z)
V2 = diag(rep(1, n)) - V1

##loss function
#loss=function(beta1,beta2,eta,sigma2) {
#  p1=dmvnorm(y, V1%*%X%*%beta1+V2%*%X%*%beta2, sigma2*diag(rep(1,n)))   ###????
#  p2=exp(sum(z*(X %*% eta)) - sum(log(1 + exp(X %*% eta))) )
#  return(-log(p1*p2))
#}

sigma2=mean(post.sample$all_sigma2[burn:m])
loss=function(beta1,beta2,eta) {
  temp=V1%*%X%*%beta1+V2%*%X%*%beta2
  p1=(n/2)*(log(2*3.14159)+log(sigma2))+(0.5/sigma2)*sum(c(y-temp)^2)
  p2=-sum(z*(X %*% eta)) + sum(log(1 + exp(X %*% eta)))
  return(p1+p2)
}


##compare the contrast in coefficients of two types data
result[3]=loss(beta_s1,beta_s2,eta_s)/(n*p)
result[4]=loss(beta_t1,beta_t2,eta_t)/(n*p) 
result[1]=result[3]-result[4]           #?????? inf-inf=nan
result[5]=result[1]/result[4]


if(result[1]<=M)
{
  result[2]=1   #"yes"
}else {
  result[2]=0   #"no"
}

return(result)
}




### conduct prediction using the estimated parameters after variable selection
### post.sample: object of class 'gibbs' (obtained from function TranBayes)
### newX: test data set without intercept
vs.predict.gibbs = function(beta1_est,beta2_est,eta_est, newX, burn=2000,m=10000,alpha=0.05)
{
  p = length(beta1_est)
  
  if(ncol(newX) != p - 1)
  {
    print("dimension of test data is NOT consistent with the dimension of train data")
    return(NULL)
  }
  
  n_pred = nrow(newX)				                   ### the size of test data
  newX = cbind(rep(1, n_pred), newX)
  #colnames(newX)[1] = 'intercept'
  if (!is.matrix(newX)) {newX = as.matrix(newX)}
  
  
  Z_pred = y_pred = prob = numeric(n_pred)
  
    for (j in 1:n_pred)
    {
      prob[j] = 1 / (1 + exp(-newX[j,] %*% eta_est))       
      #if(runif(1, min=0, max=1) < prob[i,j]) { Z_pred[i] = 1 }
      #Z_pred[j] = rbinom(1, 1, prob=prob[j])????
      Z_pred[j]=ifelse(prob[j] > 0.5, 1, 0)
    }
    y_temp = numeric(n_pred)
    index = which(Z_pred==1)
    y_temp[index] = newX[index,] %*% beta1_est      
    y_temp[-index] = newX[-index,] %*% beta2_est
    y_pred = y_temp
  
   
  #predy = data.frame(mean.y = colMeans(y_pred), median.y = apply(y_pred,2,median), lower.y = apply(y_pred,2,quantile,probs=alpha/2), upper.y = apply(y_pred,2,quantile,probs=1-alpha/2))
  
  #mean.Z = colMeans(Z_pred)
  #median.Z = apply(Z_pred, 2, median)
  #Z.mean.value = as.numeric(mean.Z >= 0.5)			### cut-off point is 0.5.
  #Z.median.value = as.numeric(median.Z >= 0.5)
  #predZ = data.frame(mean.Z = mean.Z, median.Z = median.Z, Z.mean.value = Z.mean.value, Z.median.value = Z.median.value)
  
  #predprob = data.frame(mean.prob = colMeans(prob), median.prob = apply(prob,2,median), lower.prob = apply(prob,2,quantile,probs=alpha/2), upper.prob = apply(prob,2,quantile,probs=1-alpha/2))
  
  pred = list(predy = y_pred, predZ = Z_pred )
  return(pred)
}  


###predict y z, and output all y in the mcmc chain
vs.predict.gibbs1 = function(beta1_est,beta2_est,eta_est,all_beta1,all_beta2,all_eta,newX, burn=2000,m=10000,alpha=0.05)
{
  p = length(beta1_est)
  
  
  if(ncol(newX) != p - 1)
  {
    print("dimension of test data is NOT consistent with the dimension of train data")
    return(NULL)
  }
  
  n_pred = nrow(newX)				                   ### the size of test data
  newX = cbind(rep(1, n_pred), newX)
  #colnames(newX)[1] = 'intercept'
  if (!is.matrix(newX)) {newX = as.matrix(newX)}
  
  
  Z_pred = prob=y_pred = numeric(n_pred)
  ally_pred=matrix(0,nrow=n_pred,ncol=nrow(all_beta1),byrow=FALSE)
  
  for (j in 1:n_pred)
  {
    prob[j] = 1 / (1 + exp(-newX[j,] %*% eta_est))       
    #if(runif(1, min=0, max=1) < prob[i,j]) { Z_pred[i] = 1 }
    #Z_pred[j] = rbinom(1, 1, prob=prob[j])????
    Z_pred[j]=ifelse(prob[j] > 0.5, 1, 0)
  }
  ally_temp =matrix(0,nrow=n_pred,ncol=nrow(all_beta1),byrow=FALSE)
  y_temp = numeric(n_pred)
  index = which(Z_pred==1)
  
  y_temp[index] = newX[index,] %*% beta1_est      
  y_temp[-index] = newX[-index,] %*% beta2_est
  y_pred = y_temp
  
  ally_temp[index,] = newX[index,] %*% t(all_beta1)    
  ally_temp[-index,] = newX[-index,] %*% t(all_beta2)
  ally_pred = ally_temp
  
  
  pred = list(predy = y_pred, predZ = Z_pred, allpredy=ally_pred )
  return(pred)
} 





### provide inference on the parameters after burn-up period
summary.gibbs = function(post.sample,burn=2000,m=10000, alpha=0.05)          
{
  beta1 = data.frame(Mean=colMeans(post.sample$all_beta1[burn:m,]),lower=apply(post.sample$all_beta1[burn:m,],2,quantile,alpha/2),upper=apply(post.sample$all_beta1[burn:m,],2,quantile,1-alpha/2))
  
  beta2 = data.frame(Mean=colMeans(post.sample$all_beta2[burn:m,]),lower=apply(post.sample$all_beta2[burn:m,],2,quantile,alpha/2),upper=apply(post.sample$all_beta2[burn:m,],2,quantile,1-alpha/2))
  
  eta = data.frame(Mean=colMeans(post.sample$all_eta[burn:m,]),lower=apply(post.sample$all_eta[burn:m,],2,quantile,alpha/2),upper=apply(post.sample$all_eta[burn:m,],2,quantile,1-alpha/2))
  
  
  beta1.sel = beta1$Mean
  beta2.sel = beta2$Mean
  eta.sel = eta$Mean
  for(i in 1:ncol(post.sample$all_beta1))
  {
    if(as.vector(unlist(beta1["lower"]))[i] < 0 && as.vector(unlist(beta1["upper"]))[i] > 0)
    {
      beta1.sel[i] = 0
    }
    if(as.vector(unlist(beta2["lower"]))[i] < 0 && as.vector(unlist(beta2["upper"]))[i] > 0)
    {
      beta2.sel[i] = 0
    }
    if(as.vector(unlist(eta["lower"]))[i] < 0 && as.vector(unlist(eta["upper"]))[i] > 0)
    {
      eta.sel[i] = 0
    }
  }
  beta1$Var.sel = beta1.sel       #beta1 after the selection 
  beta2$Var.sel = beta2.sel
  eta$Var.sel = eta.sel
  
  
  est = list(mean_y = post.sample$mean_y, beta1 = beta1, beta2 = beta2, eta = eta )
  est$call = match.call()
  class(est) = 'gibbs.est'
  return(est)
}




### conduct variable selection
### location_beta1: a vector indicating which variables corresponding with beta1 are significant 
### location_beta2: a vector indicating which variables corresponding with beta2 are significant
### location_eta:   a vector indicating which variables corresponding with eta are significant
### return: a vector, 1st element of which indicates the false selection of beta1
###			    2nd element of which indicates the false selection of beta2 
###	                3rd element of which indicates the false selection of eta

variable.sel = function(beta1_est,beta2_est,eta_est, beta1_true, beta2_true, eta_true, alpha = 0.05)
{
  p = length(beta1_est)
  
  
  FP_beta1 = FN_beta1 = FP_beta2 = FN_beta2 = FP_eta = FN_eta = 0
  for(i in 1:p)
  {
    if(as.vector(unlist(beta1_est))[i] == 0 && beta1_true[i] != 0)
    {
      FN_beta1 = FN_beta1 + 1
    }
    if(as.vector(unlist(beta1_est))[i] != 0 && beta1_true[i] == 0)
    {
      FP_beta1 = FP_beta1 + 1
    }
    if(as.vector(unlist(beta2_est))[i] == 0 && beta2_true[i] != 0)
    {
      FN_beta2 = FN_beta2 + 1
    }
    if(as.vector(unlist(beta2_est))[i] != 0 && beta2_true[i] == 0)
    {
      FP_beta2 = FP_beta2 + 1
    }
    if(as.vector(unlist(eta_est))[i] == 0 && eta_true[i] != 0)
    {
      FN_eta = FN_eta + 1
    }
    if(as.vector(unlist(eta_est))[i] != 0 && eta_true[i] == 0)
    {
      FP_eta = FP_eta + 1
    }
  }
  return(c(FP_beta1, FN_beta1, FP_beta2, FN_beta2, FP_eta, FN_eta))
}




