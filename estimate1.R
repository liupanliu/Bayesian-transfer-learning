#h=1, |A|=0
rm(list=ls())
source("TL-QQ.R")
m = 5000		# number of MCMC chain 10000
burn = 1001		# burn-in period     2001
N = 50			# number of replications
h=1         #number of changed coef components in A
A=0         #number of transferable datasets in source

##################################################################
############ TL-QQ model
#TLQQ's result
ParaEst1 = ParaEst2 = vector("list", N)
mis.error.set1 = mis.error.set2 = rmspe.set1 = rmspe.set2 = rep(0, N)               # measure model error
beta1.L2norm.set1 = beta2.L2norm.set1 = eta.L2norm.set1 = beta1.L2norm.set2 = beta2.L2norm.set2 = eta.L2norm.set2 = rep(0, N)    ### measure parameter estimation accuracy
FPFN.set1 = FPFN.set2 = matrix(data=0, nrow = N, ncol = 6)
colnames(FPFN.set1) = colnames(FPFN.set2) = c("FPbeta1", "FNbeta1", "FPbeta2", "FNbeta2","FPeta","FNeta")


#compared algorithm1 (only target) result
ParaEst1_t = ParaEst2_t = vector("list", N)
mis.error.set1_t = mis.error.set2_t = rmspe.set1_t = rmspe.set2_t = rep(0, N)			                 ### measure model error
beta1.L2norm.set1_t = beta2.L2norm.set1_t = eta.L2norm.set1_t = beta1.L2norm.set2_t = beta2.L2norm.set2_t = eta.L2norm.set2_t = rep(0, N)    ### measure parameter estimation accuracy
FPFN.set1_t = FPFN.set2_t = matrix(data=0, nrow = N, ncol = 6)
colnames(FPFN.set1_t) = colnames(FPFN.set2_t) = c("FPbeta1", "FNbeta1", "FPbeta2", "FNbeta2","FPeta","FNeta")


#compared algorithm1 with all source and target result
ParaEst1_s1 = ParaEst2_s1 = vector("list", N)
mis.error.set1_s1 = mis.error.set2_s1 = rmspe.set1_s1 = rmspe.set2_s1 = rep(0, N)			                 ### measure model error
beta1.L2norm.set1_s1 = beta2.L2norm.set1_s1 = eta.L2norm.set1_s1 = beta1.L2norm.set2_s1 = beta2.L2norm.set2_s1 = eta.L2norm.set2_s1 = rep(0, N)    ### measure parameter estimation accuracy
FPFN.set1_s1 = FPFN.set2_s1 = matrix(data=0, nrow = N, ncol = 6)
colnames(FPFN.set1_s1) = colnames(FPFN.set2_s1) = c("FPbeta1", "FNbeta1", "FPbeta2", "FNbeta2","FPeta","FNeta")



#compared proposed algorithm(all source) result
ParaEst1_s = ParaEst2_s = vector("list", N)
mis.error.set1_s = mis.error.set2_s = rmspe.set1_s = rmspe.set2_s = rep(0, N)			                 ### measure model error
beta1.L2norm.set1_s = beta2.L2norm.set1_s = eta.L2norm.set1_s = beta1.L2norm.set2_s = beta2.L2norm.set2_s = eta.L2norm.set2_s = rep(0, N)    ### measure parameter estimation accuracy
FPFN.set1_s = FPFN.set2_s = matrix(data=0, nrow = N, ncol = 6)
colnames(FPFN.set1_s) = colnames(FPFN.set2_s) = c("FPbeta1", "FNbeta1", "FPbeta2", "FNbeta2","FPeta","FNeta")

#compared separate model with only target result
ParaEst1_sp1 = ParaEst2_sp1 = vector("list", N)
mis.error.set1_sp1 = mis.error.set2_sp1=rmspe.set1_sp1 = rmspe.set2_sp1 = rep(0, N)			                 ### measure model error
beta1.L2norm.set1_sp1 = beta2.L2norm.set1_sp1 = eta.L2norm.set1_sp1 = beta1.L2norm.set2_sp1 = beta2.L2norm.set2_sp1 = eta.L2norm.set2_sp1 = rep(0, N)    ### measure parameter estimation accuracy
FPFN.set1_sp1 = FPFN.set2_sp1 = matrix(data=0, nrow = N, ncol = 6)
colnames(FPFN.set1_sp1) = colnames(FPFN.set2_sp1) = c("FPbeta1", "FNbeta1", "FPbeta2", "FNbeta2","FPeta","FNeta")

#compared separate model with all source and target result
ParaEst1_sp2 = ParaEst2_sp2 = vector("list", N)
mis.error.set1_sp2 = mis.error.set2_sp2=rmspe.set1_sp2 = rmspe.set2_sp2 = rep(0, N)			                 ### measure model error
beta1.L2norm.set1_sp2 = beta2.L2norm.set1_sp2 = eta.L2norm.set1_sp2 = beta1.L2norm.set2_sp2 = beta2.L2norm.set2_sp2 = eta.L2norm.set2_sp2 = rep(0, N)    ### measure parameter estimation accuracy
FPFN.set1_sp2 = FPFN.set2_sp2 = matrix(data=0, nrow = N, ncol = 6)
colnames(FPFN.set1_sp2) = colnames(FPFN.set2_sp2) = c("FPbeta1", "FNbeta1", "FPbeta2", "FNbeta2","FPeta","FNeta")

#parameters setting
n=80       #80  
n_test=100
p=99       #99
p1=5        
p2=p+1-p1      #the number of zero in beta

#results of all coefficients in all methods
Beta1_est=Beta2_est=Eta_est=Beta1_est_t=Beta2_est_t=Eta_est_t=Beta1_est_s=Beta2_est_s=Eta_est_s=Beta1_est_s1=Beta2_est_s1=Eta_est_s1=Beta1_est_sp1=Beta2_est_sp1=Eta_est_sp1=Beta1_est_sp2=Beta2_est_sp2=Eta_est_sp2=matrix(0,nrow=N,ncol=p+1)

##generate target data!!!!!
X=mvrnorm(n+n_test,rep(0,p),AR(0.7,p))
###real value of beta and eta
beta1_true=c(rep(5,p1),rep(0,p2))
beta2_true=c(5,rep(0,p2),rep(5,p1-1))
eta_true=c(rep(5,p1),rep(0,p2))

pi=numeric(n+n_test)
Z=numeric(n+n_test)
y=numeric(n+n_test)

for (i in 1:(n+n_test)){
  pi[i]=1/(1+exp(-1*cbind(rep(1, n+n_test), X)[i,]%*%eta_true))
  Z[i]=rbinom(1,1,pi[i])
  y[i]=rnorm(1,Z[i]*cbind(rep(1, n+n_test), X)[i,]%*%beta1_true+(1-Z[i])*cbind(rep(1, n+n_test), X)[i,]%*%beta2_true,1)    #the true value of sigma^2
}
###target data
X_t = X[1:n,]
y_t = y[1:n]
Z_t = Z[1:n]

###test data
X_test = X[-(1:n),]
Z_test = Z[-(1:n)]
y_test = y[-(1:n)]


###source data
n1=60       
num=10           #the number of xyz data set in each source data 10
X_sou=array(0,c(n1,p,num,N))
y_sou=array(0,c(num,n1,N))
Z_sou=array(0,c(num,n1,N))

##set different beta/eta values to generate source data
Beta1=matrix(rep(beta1_true,num),nrow=num,ncol=p+1,byrow = TRUE)
Beta2=matrix(rep(beta2_true,num),nrow=num,ncol=p+1,byrow = TRUE)
Eta=matrix(rep(eta_true,num),nrow=num,ncol=p+1,byrow = TRUE)

#random index of Hk
num_random1=num_random2=num_random3=numeric(h)
num_random4=num_random5=num_random6=numeric(20)
for (i in 1:num){
  if (i <=A){
    #set the true coef of transferable source
    num_random1=sample(1:(p+1),h)
    num_random2=sample(1:(p+1),h)
    num_random3=sample(1:(p+1),h)
    Beta1[i,num_random1]=Beta1[i,num_random1]-1
    Beta2[i,num_random2]=Beta2[i,num_random2]-1
    Eta[i,num_random3]=Eta[i,num_random3]-1
  }else{
    #set the true coef of non-transferable source
    num_random4=sample(2:(p+1),20)
    num_random5=sample(2:(p+1),20)
    num_random6=sample(2:(p+1),20)
    Beta1[i,num_random4]=Beta1[i,num_random4]-10
    Beta2[i,num_random5]=Beta2[i,num_random5]-10
    Eta[i,num_random6]=Eta[i,num_random6]-10
  }
}


pis=numeric(n1)
for (w in 1:N){
  for (i in 1:num) {
    X_sou[,,i,w]=mvrnorm(n1,rep(0,p),AR(0.7,p))        
    for (j in 1:n1){
      pis[j]=1/(1+exp(-1*cbind(rep(1, n1), X_sou[,,i,w])[j,]%*%Eta[i,]))
      Z_sou[i,j,w]=rbinom(1,1,pis[j])
      y_sou[i,j,w]=rnorm(1,Z_sou[i,j,w]*cbind(rep(1, n1), X_sou[,,i,w])[j,]%*%Beta1[i,]+(1-Z_sou[i,j,w])*cbind(rep(1, n1), X_sou[,,i,w])[j,]%*%Beta2[i,],1)
      
    }
  }
}



result=array(0,c(num,5,N))
lab=matrix(0,nrow = N,ncol = num)
#the threshold of detection !!!
M=1

##target data with proposed algorithm
ParaEst= TranBayes(X_t, y_t, Z_t, m, burn, tau1_0 = rep(1,ncol(X_t)+1),
                   tau2_0 =rep(1,ncol(X_t)+1), tau3_0 = rep(1,ncol(X_t)+1),                             lambda1_0=1, lambda2_0=1, lambda3_0=1)
##run the replications
for(w in 1:N)
{  
  ##source data and detection
  #w=1
  for (i in 1:num) {
    ##test
    #i=1
    #w=1
    #test
    X_c = X_sou[,,i,w]
    y_c = y_sou[i,,w]
    Z_c = Z_sou[i,,w]
    result[i,,w]=istransferable(ParaEst,X_t,y_t,Z_t,X_c,y_c,Z_c,m,burn,M)
    #result[i,1,w] denotes the contrast 
    
    if (result[i,2,w]==1){
      lab[w,i]=i
    }
  }
  
  ###transferable data
  X_s = NULL
  y_s = NULL
  Z_s = NULL
  for (i in lab[w,]){
    if (i>0){
      X_s=rbind(X_s,X_sou[,,i,w])
      y_s=c(y_s,y_sou[i,,w])
      Z_s=c(Z_s,Z_sou[i,,w])
    }
  }
  
  #all source data
  X_alls = NULL
  y_alls = NULL
  Z_alls = NULL
  for (i in 1:num){
    X_alls=rbind(X_alls,X_sou[,,i,w])
    y_alls=c(y_alls,y_sou[i,,w])
    Z_alls=c(Z_alls,Z_sou[i,,w])
  }
  
  X_alls1=rbind(X_alls,X_t)
  y_alls1=c(y_alls,y_t)
  Z_alls1=c(Z_alls,Z_t)
  
  
  
  #######################################################
  ############# 1st setting for the tunning parameters
  #######################################################
  
  ###proposed algorithm--TLQQ!!!
  if (is.null(X_s)){
    ###estimates of final beta/eta
    Beta1_est[w,]= ParaEst$beta1_est
    Beta2_est[w,]= ParaEst$beta2_est
    Eta_est[w,]= ParaEst$eta_est
  }else{
    ParaEst1[[w]] = TranBayes(X_s, y_s, Z_s, m, burn, tau1_0 = rep(1,ncol(X_s)+1),
                              tau2_0 =rep(1,ncol(X_s)+1), 
                              tau3_0 = rep(1,ncol(X_s)+1), 
                              lambda1_0=1, lambda2_0=1, lambda3_0=1)
    
    ParaEst2[[w]]=TranDebias(X_t, y_t, Z_t, m, burn, ParaEst1[[w]], 
                             xi1_0 = rep(1,ncol(X_t)+1), xi2_0 =rep(1,ncol(X_t)+1), 
                             xi3_0 = rep(1,ncol(X_t)+1), rho1_0=1, rho2_0=1, rho3_0=1)
    ###estimates of final beta/eta
    Beta1_est[w,]= ParaEst1[[w]]$beta1_est+ParaEst2[[w]]$delta1_est
    Beta2_est[w,]= ParaEst1[[w]]$beta2_est+ParaEst2[[w]]$delta2_est
    Eta_est[w,]= ParaEst1[[w]]$eta_est+ParaEst2[[w]]$deltaeta_est
  }
  
  ####### parameter estimation accuracy mse
  beta1.L2norm.set1[w] = mean((Beta1_est[w,] - beta1_true)^2)
  beta2.L2norm.set1[w] = mean((Beta2_est[w,] - beta2_true)^2)
  eta.L2norm.set1[w] = mean((Eta_est[w,] - eta_true)^2)
  
  ####### varialbe selection accuracy
  FPFN.set1[w,] = variable.sel(Beta1_est[w,],Beta2_est[w,],Eta_est[w,], beta1_true, beta2_true, eta_true)
  
  ############# prediction results  
  pred_result1 = vs.predict.gibbs(Beta1_est[w,],Beta2_est[w,],Eta_est[w,],X_test)
  ### misclassification error rate for qualitative response Z
  mis.error.set1[w] = mean(abs(pred_result1$predZ - Z_test))	
  ### mse for y
  rmspe.set1[w] = sqrt(mean((unlist(pred_result1$predy) - y_test)^2))
  
  
  
  ###compared algorithm1--Algorithm1 with only target!!!
  Beta1_est_t[w,]= ParaEst$beta1_est
  Beta2_est_t[w,]= ParaEst$beta2_est
  Eta_est_t[w,]= ParaEst$eta_est
  
  ####### parameter estimation accuracy mse
  beta1.L2norm.set1_t[w] = mean((Beta1_est_t[w,] - beta1_true)^2)
  beta2.L2norm.set1_t[w] = mean((Beta2_est_t[w,] - beta2_true)^2)
  eta.L2norm.set1_t[w] = mean((Eta_est_t[w,] - eta_true)^2)
  
  ####### varialbe selection accuracy
  FPFN.set1_t[w,] = variable.sel(Beta1_est_t[w,], Beta2_est_t[w,],Eta_est_t[w,], beta1_true, beta2_true, eta_true)
  
  ############# prediction results  
  pred_result1_t = vs.predict.gibbs(Beta1_est_t[w,],Beta2_est_t[w,],Eta_est_t[w,], X_test)
  ### misclassification error rate for qualitative response Z
  mis.error.set1_t[w] = mean(abs(pred_result1_t$predZ - Z_test))	
  ### mse for y
  rmspe.set1_t[w] = sqrt(mean((unlist(pred_result1_t$predy) - y_test)^2))
  
  
  
  ###compared algorithm2--Algorithm1 with target and all source!!!
  ParaEst1_s1[[w]] = TranBayes(X_alls1, y_alls1, Z_alls1, m, burn, 
                               tau1_0 = rep(1,ncol(X_alls1)+1),
                               tau2_0 =rep(1,ncol(X_alls1)+1), 
                               tau3_0 = rep(1,ncol(X_alls1)+1),
                               lambda1_0=1, lambda2_0=1, lambda3_0=1)
  
  ###estimates of final beta/eta
  Beta1_est_s1[w,]= ParaEst1_s1[[w]]$beta1_est
  Beta2_est_s1[w,]= ParaEst1_s1[[w]]$beta2_est
  Eta_est_s1[w,]= ParaEst1_s1[[w]]$eta_est
  
  ####### parameter estimation accuracy mse
  beta1.L2norm.set1_s1[w] = mean((Beta1_est_s1[w,] - beta1_true)^2)
  beta2.L2norm.set1_s1[w] = mean((Beta2_est_s1[w,] - beta2_true)^2)
  eta.L2norm.set1_s1[w] = mean((Eta_est_s1[w,] - eta_true)^2)
  
  ####### varialbe selection accuracy
  FPFN.set1_s1[w,] = variable.sel(Beta1_est_s1[w,],Beta2_est_s1[w,],Eta_est_s1[w,], beta1_true, beta2_true, eta_true)
  
  ############# prediction results  
  pred_result1_s1 = vs.predict.gibbs(Beta1_est_s1[w,],Beta2_est_s1[w,],Eta_est_s1[w,], X_test)
  ### misclassification error rate for qualitative response Z
  mis.error.set1_s1[w] = mean(abs(pred_result1_s1$predZ - Z_test))	
  ### mse for y
  rmspe.set1_s1[w] = sqrt(mean((unlist(pred_result1_s1$predy) - y_test)^2))  
  
  
  
  ###compared algorithm3--TLQQ with target and all source!!!
  ParaEst1_s[[w]] = TranBayes(X_alls, y_alls, Z_alls, m, burn, 
                              tau1_0 = rep(1,ncol(X_alls)+1),
                              tau2_0 =rep(1,ncol(X_alls)+1), 
                              tau3_0 = rep(1,ncol(X_alls)+1),
                              lambda1_0=1, lambda2_0=1, lambda3_0=1)
  
  ParaEst2_s[[w]]=TranDebias(X_t, y_t, Z_t, m, burn, ParaEst1_s[[w]], 
                             xi1_0 = rep(1,ncol(X_t)+1), xi2_0 =rep(1,ncol(X_t)+1), 
                             xi3_0 = rep(1,ncol(X_t)+1), rho1_0=1, rho2_0=1, rho3_0=1)
  ###estimates of final beta/eta
  Beta1_est_s[w,]= ParaEst1_s[[w]]$beta1_est+ParaEst2_s[[w]]$delta1_est
  Beta2_est_s[w,]= ParaEst1_s[[w]]$beta2_est+ParaEst2_s[[w]]$delta2_est
  Eta_est_s[w,]= ParaEst1_s[[w]]$eta_est+ParaEst2_s[[w]]$deltaeta_est
  
  ####### parameter estimation accuracy mse
  beta1.L2norm.set1_s[w] = mean((Beta1_est_s[w,] - beta1_true)^2)
  beta2.L2norm.set1_s[w] = mean((Beta2_est_s[w,] - beta2_true)^2)
  eta.L2norm.set1_s[w] = mean((Eta_est_s[w,] - eta_true)^2)
  
  ####### varialbe selection accuracy
  FPFN.set1_s[w,] = variable.sel(Beta1_est_s[w,],Beta2_est_s[w,],Eta_est_s[w,], beta1_true, beta2_true, eta_true)
  
  ############# prediction results  
  pred_result1_s = vs.predict.gibbs(Beta1_est_s[w,],Beta2_est_s[w,],Eta_est_s[w,], X_test)
  ### misclassification error rate for qualitative response Z
  mis.error.set1_s[w] = mean(abs(pred_result1_s$predZ - Z_test))	
  ### mse for y
  rmspe.set1_s[w] = sqrt(mean((unlist(pred_result1_s$predy) - y_test)^2))
  
  
  
  ###compared algorithm4--Separate Model with only target!!!
  ###estimate eta
  fit0=glmnet(X_t, Z_t, family="binomial", alpha = 1)		###Lasso penalty alpha = 1
  Eta_est_sp1[w,] = as.vector(coef(fit0, s = cv.glmnet(X_t, Z_t, family="binomial", nfolds = 5)$lambda.min))
  ###estimate beta1 and beta2
  index_0 = which(Z_t == 0)
  index_1 = which(Z_t == 1)
  X0 = X_t[index_0,]
  X1 = X_t[index_1,]
  y0 = y_t[index_0]
  y1 = y_t[index_1]
  ###estimates of final beta1,beta2
  fit1 = glmnet(X1, y1, family = 'gaussian', alpha = 1)
  Beta1_est_sp1[w,] = as.vector(coef(fit1, s = cv.glmnet(X1, y1, nfolds=5, alpha = 1)$lambda.min))
  fit2 = glmnet(X0, y0, family = 'gaussian', alpha = 1)
  Beta2_est_sp1[w,] = as.vector(coef(fit2, s = cv.glmnet(X0, y0, nfolds=5, alpha = 1)$lambda.min))
  
  ####### parameter estimation accuracy mse
  beta1.L2norm.set1_sp1[w] = mean((Beta1_est_sp1[w,] - beta1_true)^2)
  beta2.L2norm.set1_sp1[w] = mean((Beta2_est_sp1[w,] - beta2_true)^2)
  eta.L2norm.set1_sp1[w] = mean((Eta_est_sp1[w,] - eta_true)^2)
  
  
  ####### varialbe selection accuracy
  FPFN.set1_sp1[w,]=variable.sel(Beta1_est_sp1[w,],Beta2_est_sp1[w,],Eta_est_sp1[w,], beta1_true, beta2_true, eta_true)
  
  ### misclassification error rate for qualitative response Z
  newX = cbind(rep(1, nrow(X_test)), X_test)
  prob = as.vector(1 / (1 + exp(-newX %*% Eta_est_sp1[w,])))
  predZ = ifelse(prob>0.5, 1, 0)
  mis.error.set1_sp1[w] =mean(abs(predZ - Z_test))
  ### mse for y
  index0 = which(predZ == 0)
  index1 = which(predZ == 1)
  newX0 = newX[index0,]
  newX1 = newX[index1,]
  newy0 = y_test[index0]
  newy1 = y_test[index1]
  rmspe.set1_sp1[w] = sqrt((sum((as.vector(newX0 %*% Beta2_est_sp1[w,]) - newy0)^2) + sum((as.vector(newX1 %*% Beta1_est_sp1[w,]) - newy1)^2)) / length(y_test))
  
  
  
  ###compared algorithm5--Separate Model with target and all source!!!
  ###estimate eta
  fit0=glmnet(X_alls1, Z_alls1, family="binomial", alpha = 1)#Lasso penalty alpha=1
  Eta_est_sp2[w,] = as.vector(coef(fit0, s = cv.glmnet(X_alls1, Z_alls1, family="binomial", nfolds = 5)$lambda.min))
  ###estimate beta1 and beta2
  index_0 = which(Z_alls1 == 0)
  index_1 = which(Z_alls1 == 1)
  X0 = X_alls1[index_0,]
  X1 = X_alls1[index_1,]
  y0 = y_alls1[index_0]
  y1 = y_alls1[index_1]
  ###estimates of final beta1,beta2
  fit1 = glmnet(X1, y1, family = 'gaussian', alpha = 1)
  Beta1_est_sp2[w,] = as.vector(coef(fit1, s = cv.glmnet(X1, y1, nfolds=5, alpha = 1)$lambda.min))
  fit2 = glmnet(X0, y0, family = 'gaussian', alpha = 1)
  Beta2_est_sp2[w,] = as.vector(coef(fit2, s = cv.glmnet(X0, y0, nfolds=5, alpha = 1)$lambda.min))
  
  ####### parameter estimation accuracy mse
  beta1.L2norm.set1_sp2[w] = mean((Beta1_est_sp2[w,] - beta1_true)^2)
  beta2.L2norm.set1_sp2[w] = mean((Beta2_est_sp2[w,] - beta2_true)^2)
  eta.L2norm.set1_sp2[w] = mean((Eta_est_sp2[w,] - eta_true)^2)
  
  ####### varialbe selection accuracy
  FPFN.set1_sp2[w,]=variable.sel(Beta1_est_sp2[w,],Beta2_est_sp2[w,],Eta_est_sp2[w,], beta1_true, beta2_true, eta_true)
  
  ### misclassification error rate for qualitative response Z
  newX = cbind(rep(1, nrow(X_test)), X_test)
  prob = as.vector(1 / (1 + exp(-newX %*% Eta_est_sp2[w,])))
  predZ = ifelse(prob>0.5, 1, 0)
  mis.error.set1_sp2[w] =mean(abs(predZ - Z_test))
  ### mse for y
  index0 = which(predZ == 0)
  index1 = which(predZ == 1)
  newX0 = newX[index0,]
  newX1 = newX[index1,]
  newy0 = y_test[index0]
  newy1 = y_test[index1]
  rmspe.set1_sp2[w] = sqrt((sum((as.vector(newX0 %*% Beta2_est_sp2[w,]) - newy0)^2) + sum((as.vector(newX1 %*% Beta1_est_sp2[w,]) - newy1)^2)) / length(y_test))
  
  
  
  print(w)
  print(Sys.time())
  save.image("Result1.RData")
}









