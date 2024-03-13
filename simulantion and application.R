###############The part of simulation!!!
###For the first simulation setting h=1, |A|=0, we have total 12 settings to run. (Only one example is given here)
rm(list=ls())
###load the main algorithm function R code.
source("/Users/liu/Desktop/transfer learning/tex and code/TL-QQ.R")
m = 5000		# number of MCMC chain 10000
burn = 1001		# burn-in period     2001
N = 50			# number of replications
h=1         #number of changed coef components in A
A=0       #number of transferable datasets in source

##################################################################
############ BTLQQ model
#BTLQQ's result
ParaEst1 = ParaEst2 = vector("list", N)
mis.error.set1 = mis.error.set2 = rmspe.set1 = rmspe.set2 = rep(0, N)               # measure model error
beta1.L2norm.set1 = beta2.L2norm.set1 = eta.L2norm.set1 = beta1.L2norm.set2 = beta2.L2norm.set2 = eta.L2norm.set2 = rep(0, N)    ### measure parameter estimation accuracy
FPFN.set1 = FPFN.set2 = matrix(data=0, nrow = N, ncol = 6)
colnames(FPFN.set1) = colnames(FPFN.set2) = c("FPbeta1", "FNbeta1", "FPbeta2", "FNbeta2","FPeta","FNeta")


#QQ's result
ParaEst1_t = ParaEst2_t = vector("list", N)
mis.error.set1_t = mis.error.set2_t = rmspe.set1_t = rmspe.set2_t = rep(0, N)			                 ### measure model error
beta1.L2norm.set1_t = beta2.L2norm.set1_t = eta.L2norm.set1_t = beta1.L2norm.set2_t = beta2.L2norm.set2_t = eta.L2norm.set2_t = rep(0, N)    ### measure parameter estimation accuracy
FPFN.set1_t = FPFN.set2_t = matrix(data=0, nrow = N, ncol = 6)
colnames(FPFN.set1_t) = colnames(FPFN.set2_t) = c("FPbeta1", "FNbeta1", "FPbeta2", "FNbeta2","FPeta","FNeta")


#QQALL's result
ParaEst1_s1 = ParaEst2_s1 = vector("list", N)
mis.error.set1_s1 = mis.error.set2_s1 = rmspe.set1_s1 = rmspe.set2_s1 = rep(0, N)			                 ### measure model error
beta1.L2norm.set1_s1 = beta2.L2norm.set1_s1 = eta.L2norm.set1_s1 = beta1.L2norm.set2_s1 = beta2.L2norm.set2_s1 = eta.L2norm.set2_s1 = rep(0, N)    ### measure parameter estimation accuracy
FPFN.set1_s1 = FPFN.set2_s1 = matrix(data=0, nrow = N, ncol = 6)
colnames(FPFN.set1_s1) = colnames(FPFN.set2_s1) = c("FPbeta1", "FNbeta1", "FPbeta2", "FNbeta2","FPeta","FNeta")



#BTLQQALL's result
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

#random index of H_k
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

##the prob in the rbinom of every discrete response z, then generate the y and z of source data
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


##the result of source detection
result=array(0,c(num,5,N))
lab=matrix(0,nrow = N,ncol = num)
#the threshold of detection !!!
M=1

##target data with proposed algorithm BTLQQ
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
    #result[i,1,w] denotes the contrast,result[i,2,w] denotes istransferable or not 
    
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
  #######################################################
  
  ###proposed algorithm--BTLQQ!!!
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
  
  
  
  ###compared algorithm1--QQ
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
  
  
  
  ###compared algorithm2--QQALL
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
  
  
  
  ###compared algorithm3--BTLQQALL
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
  save.image("A0h1.RData")
}



##############The part of data visualization of simulation result(total 12 RData files)
rm(list=ls())
##load the RData. Here is a demonstration, a total of 12 RData files need to do this process
load("A0h1.RData")
a=sqrt(mean(beta1.L2norm.set1))
b=sqrt(mean(beta2.L2norm.set1))
c=sqrt(mean(eta.L2norm.set1))
d=mean(rmspe.set1)
e=mean(mis.error.set1)
a1=sqrt(mean(beta1.L2norm.set1_t))
b1=sqrt(mean(beta2.L2norm.set1_t))
c1=sqrt(mean(eta.L2norm.set1_t))
d1=mean(rmspe.set1_t)
e1=mean(mis.error.set1_t)
a2=sqrt(mean(beta1.L2norm.set1_s1))
b2=sqrt(mean(beta2.L2norm.set1_s1))
c2=sqrt(mean(eta.L2norm.set1_s1))
d2=mean(rmspe.set1_s1)
e2=mean(mis.error.set1_s1)
a3=sqrt(mean(beta1.L2norm.set1_s))
b3=sqrt(mean(beta2.L2norm.set1_s))
c3=sqrt(mean(eta.L2norm.set1_s))
d3=mean(rmspe.set1_s)
e3=mean(mis.error.set1_s)
res=matrix(c(a,b,c,d,e,a1,b1,c1,d1,e1,a2,b2,c2,d2,e2,a3,b3,c3,d3,e3),nrow=4,byrow = TRUE)
###Import the data needed for the visualization into the file "res.csv".
write.table (res, file ="/Users/liu/Desktop/transfer learning/Pan/res.csv",append=TRUE,sep =",", row.names =FALSE, col.names =FALSE)


####plot the result of simulation 
setwd("/Users/liu/Desktop/transfer learning/simulation data")
#import the "res.csv".
result=read.csv("/Users/liu/Desktop/transfer learning/simulation data/res.csv", header=F, na.strings=c("NA"))
library(ggplot2)
##########h=1
#beta1_l2norm
A=c(0,4,8,10)
beta1=result[,1][c(1,5,9,13)]
beta1_t=result[,1][c(2,6,10,14)]
beta1_s1=result[,1][c(3,7,11,15)]
beta1_s=result[,1][c(4,8,12,16)]

#c()
par(mfrow=c(1,1))
plot(A,beta1,pch = 16,lty=2,type = "o",col = "magenta", xlab = "|A|", ylab = "beta1.L2loss(h=1)",ylim = c(0,2.5))
lines(A,beta1_t,pch=15,lty=1,col="green",type = "o")
lines(A,beta1_s1,pch=17,lty=1,col="blue",type = "o")
lines(A,beta1_s,pch=18,lty=1,col="black",type = "o")
legend(1,0.8,c("TLQQ", "QQ with only target", "QQ with target and all source","TLQQ with target and all source"), pt.cex = 0.9, pch = c(16,15,17,18),col=c("magenta","green","blue","black"),cex = 0.5, bty = "n",xpd=TRUE,text.font=1,xjust = -2, yjust = -2.2)

##########h=3
#beta1_l2norm
beta1=result[,1][c(17,21,25,29)]
beta1_t=result[,1][c(18,22,26,30)]
beta1_s1=result[,1][c(19,23,27,31)]
beta1_s=result[,1][c(20,24,28,32)]
#c()
plot(A,beta1,pch = 16,lty=1,type = "o",col = "magenta", xlab = "|A|", ylab = "beta1.L2loss(h=3)",ylim = c(0,2.5))
lines(A,beta1_t,pch=15,lty=1,col="green",type = "o")
lines(A,beta1_s1,pch=17,lty=1,col="blue",type = "o")
lines(A,beta1_s,pch=18,lty=1,col="black",type = "o")
legend(1,0.8,c("TLQQ", "QQ with only target", "QQ with target and all source","TLQQ with target and all source"), pt.cex = 0.9, pch = c(16,15,17,18),col=c("magenta","green","blue","black"),cex = 0.5, bty = "n",xpd=TRUE,text.font=1,xjust = -2, yjust = -2.2)



##########h=5
#beta1_l2norm
beta1=result[,1][c(33,37,41,45)]
beta1_t=result[,1][c(34,38,42,46)]
beta1_s1=result[,1][c(35,39,43,47)]
beta1_s=result[,1][c(36,40,44,48)]
#c()
plot(A,beta1,pch = 16,lty=1,type = "o",col = "magenta", xlab = "|A|", ylab = "beta1.L2loss(h=5)",ylim = c(0,2.5))
lines(A,beta1_t,pch=15,lty=1,col="green",type = "o")
lines(A,beta1_s1,pch=17,lty=1,col="blue",type = "o")
lines(A,beta1_s,pch=18,lty=1,col="black",type = "o")
legend(1,0.8,c("TLQQ", "QQ with only target", "QQ with target and all source","TLQQ with target and all source"), pt.cex = 0.9, pch = c(16,15,17,18),col=c("magenta","green","blue","black"),cex = 0.5, bty = "n",xpd=TRUE,text.font=1,xjust = -2, yjust = -2.2)



##########h=1
#beta2-l2norm
beta2=result[,2][c(1,5,9,13)]
beta2_t=result[,2][c(2,6,10,14)]
beta2_s1=result[,2][c(3,7,11,15)]
beta2_s=result[,2][c(4,8,12,16)]
#c()
plot(A,beta2,pch = 16,lty=1,type = "o",col = "magenta", xlab = "|A|", ylab = "beta2.L2loss(h=1)",ylim = c(0,2.5))
lines(A,beta2_t,pch=15,lty=1,col="green",type = "o")
lines(A,beta2_s1,pch=17,lty=1,col="blue",type = "o")
lines(A,beta2_s,pch=18,lty=1,col="black",type = "o")
legend(1,0.8,c("TLQQ", "QQ with only target", "QQ with target and all source","TLQQ with target and all source"), pt.cex = 0.9, pch = c(16,15,17,18),col=c("magenta","green","blue","black"),cex = 0.5, bty = "n",xpd=TRUE,text.font=1,xjust = -2, yjust = -2.2)


##########h=3
#beta2-l2norm
beta2=result[,2][c(17,21,25,29)]
beta2_t=result[,2][c(18,22,26,30)]
beta2_s1=result[,2][c(19,23,27,31)]
beta2_s=result[,2][c(20,24,28,32)]
#c()
plot(A,beta2,pch = 16,lty=1,type = "o",col = "magenta", xlab = "|A|", ylab = "beta2.L2loss(h=3)",ylim = c(0,2.5))
lines(A,beta2_t,pch=15,lty=1,col="green",type = "o")
lines(A,beta2_s1,pch=17,lty=1,col="blue",type = "o")
lines(A,beta2_s,pch=18,lty=1,col="black",type = "o")
legend(1,0.8,c("TLQQ", "QQ with only target", "QQ with target and all source","TLQQ with target and all source"), pt.cex = 0.9, pch = c(16,15,17,18),col=c("magenta","green","blue","black"),cex = 0.5, bty = "n",xpd=TRUE,text.font=1,xjust = -2, yjust = -2.2)


#########h=5
#beta2-l2norm
beta2=result[,2][c(33,37,41,45)]
beta2_t=result[,2][c(34,38,42,46)]
beta2_s1=result[,2][c(35,39,43,47)]
beta2_s=result[,2][c(36,40,44,48)]
#c()
plot(A,beta2,pch = 16,lty=1,type = "o",col = "magenta", xlab = "|A|", ylab = "beta2.L2loss(h=5)",ylim = c(0,2.5))
lines(A,beta2_t,pch=15,lty=1,col="green",type = "o")
lines(A,beta2_s1,pch=17,lty=1,col="blue",type = "o")
lines(A,beta2_s,pch=18,lty=1,col="black",type = "o")
legend(1,0.8,c("TLQQ", "QQ with only target", "QQ with target and all source","TLQQ with target and all source"), pt.cex = 0.9, pch = c(16,15,17,18),col=c("magenta","green","blue","black"),cex = 0.5, bty = "n",xpd=TRUE,text.font=1,xjust = -2, yjust = -2.2)


########h=1
#eta-l2norm
eta=result[,3][c(1,5,9,13)]
eta_t=result[,3][c(2,6,10,14)]
eta_s1=result[,3][c(3,7,11,15)]
eta_s=result[,3][c(4,8,12,16)]
#c()
plot(A,eta,pch = 16,lty=1,type = "o",col = "magenta", xlab = "|A|", ylab = "eta.L2loss(h=1)",ylim = c(0.6,1.1))
lines(A,eta_t,pch=15,lty=1,col="green",type = "o")
lines(A,eta_s1,pch=17,lty=1,col="blue",type = "o")
lines(A,eta_s,pch=18,lty=1,col="black",type = "o")
legend(-6,0.3,c("TLQQ", "QQ with only target", "QQ with target and all source","TLQQ with target and all source"), pt.cex = 0.9, pch = c(16,15,17,18),col=c("magenta","green","blue","black"),cex = 0.5, bty = "n",xpd=TRUE,text.font=1,xjust = -1.8, yjust = -2.7)

########h=3
#eta-l2norm
eta=result[,3][c(17,21,25,29)]
eta_t=result[,3][c(18,22,26,30)]
eta_s1=result[,3][c(19,23,27,31)]
eta_s=result[,3][c(20,24,28,32)]
#c()
plot(A,eta,pch = 16,lty=1,type = "o",col = "magenta", xlab = "|A|", ylab = "eta.L2loss(h=3)",ylim = c(0.6,1.1))
lines(A,eta_t,pch=15,lty=1,col="green",type = "o")
lines(A,eta_s1,pch=17,lty=1,col="blue",type = "o")
lines(A,eta_s,pch=18,lty=1,col="black",type = "o")
legend(-6,0.3,c("TLQQ", "QQ with only target", "QQ with target and all source","TLQQ with target and all source"), pt.cex = 0.9, pch = c(16,15,17,18),col=c("magenta","green","blue","black"),cex = 0.5, bty = "n",xpd=TRUE,text.font=1,xjust = -1.8, yjust = -2.7)

########h=5
#eta-l2norm
eta=result[,3][c(33,37,41,45)]
eta_t=result[,3][c(34,38,42,46)]
eta_s1=result[,3][c(35,39,43,47)]
eta_s=result[,3][c(36,40,44,48)]
#c()
plot(A,eta,pch = 16,lty=1,type = "o",col = "magenta", xlab = "|A|", ylab = "eta.L2loss(h=5)",ylim = c(0.6,1.1))
lines(A,eta_t,pch=15,lty=1,col="green",type = "o")
lines(A,eta_s1,pch=17,lty=1,col="blue",type = "o")
lines(A,eta_s,pch=18,lty=1,col="black",type = "o")
legend(-6,0.3,c("TLQQ", "QQ with only target", "QQ with target and all source","TLQQ with target and all source"), pt.cex = 0.9, pch = c(16,15,17,18),col=c("magenta","green","blue","black"),cex = 0.5, bty = "n",xpd=TRUE,text.font=1,xjust = -1.8, yjust = -2.7)


##h=1
#rmspe
rmspe=result[,4][c(1,5,9,13)]
rmspe_t=result[,4][c(2,6,10,14)]
rmspe_s1=result[,4][c(3,7,11,15)]
rmspe_s=result[,4][c(4,8,12,16)]
#c()
plot(A,rmspe,pch = 16,lty=1,type = "o",col = "magenta", xlab = "|A|", ylab = "rmse(h=1)",ylim = c(3,30))
lines(A,rmspe_t,pch=15,lty=1,col="green",type = "o")
lines(A,rmspe_s1,pch=17,lty=1,col="blue",type = "o")
lines(A,rmspe_s,pch=18,lty=1,col="black",type = "o")
legend(1.5,9,c("TLQQ", "QQ with only target", "QQ with target and all source","TLQQ with target and all source"), pt.cex = 0.9, pch = c(16,15,17,18),col=c("magenta","green","blue","black"),cex = 0.5, bty = "n",xpd=TRUE,text.font=1,xjust = -1.8, yjust = -2.7)

##h=3
#rmspe
rmspe=result[,4][c(17,21,25,29)]
rmspe_t=result[,4][c(18,22,26,30)]
rmspe_s1=result[,4][c(19,23,27,31)]
rmspe_s=result[,4][c(20,24,28,32)]
#c()
plot(A,rmspe,pch = 16,lty=1,type = "o",col = "magenta", xlab = "|A|", ylab = "rmse(h=3)",ylim = c(3,30))
lines(A,rmspe_t,pch=15,lty=1,col="green",type = "o")
lines(A,rmspe_s1,pch=17,lty=1,col="blue",type = "o")
lines(A,rmspe_s,pch=18,lty=1,col="black",type = "o")
legend(1.5,9,c("TLQQ", "QQ with only target", "QQ with target and all source","TLQQ with target and all source"), pt.cex = 0.9, pch = c(16,15,17,18),col=c("magenta","green","blue","black"),cex = 0.5, bty = "n",xpd=TRUE,text.font=1,xjust = -1.8, yjust = -2.7)

##h=5
#rmspe
rmspe=result[,4][c(33,37,41,45)]
rmspe_t=result[,4][c(34,38,42,46)]
rmspe_s1=result[,4][c(35,39,43,47)]
rmspe_s=result[,4][c(36,40,44,48)]
#c()
plot(A,rmspe,pch = 16,lty=1,type = "o",col = "magenta", xlab = "|A|", ylab = "rmse(h=5)",ylim = c(3,30))
lines(A,rmspe_t,pch=15,lty=1,col="green",type = "o")
lines(A,rmspe_s1,pch=17,lty=1,col="blue",type = "o")
lines(A,rmspe_s,pch=18,lty=1,col="black",type = "o")
legend(1.5,9,c("TLQQ", "QQ with only target", "QQ with target and all source","TLQQ with target and all source"), pt.cex = 0.9, pch = c(16,15,17,18),col=c("magenta","green","blue","black"),cex = 0.5, bty = "n",xpd=TRUE,text.font=1,xjust = -1.8, yjust = -2.7)


####h=1
#miserror
miserror=result[,5][c(1,5,9,13)]
miserror_t=result[,5][c(2,6,10,14)]
miserror_s1=result[,5][c(3,7,11,15)]
miserror_s=result[,5][c(4,8,12,16)]
#c()
plot(A,miserror,pch = 16,lty=1,type = "o",col = "magenta", xlab = "|A|", ylab = "me(h=1)",ylim = c(0,0.5))
lines(A,miserror_t,pch=15,lty=1,col="green",type = "o")
lines(A,miserror_s1,pch=17,lty=1,col="blue",type = "o")
lines(A,miserror_s,pch=18,lty=1,col="black",type = "o")
legend(1.4,0.1,c("TLQQ", "QQ with only target", "QQ with target and all source","TLQQ with target and all source"), pt.cex = 0.9, pch = c(16,15,17,18),col=c("magenta","green","blue","black"),cex = 0.5, bty = "n",xpd=TRUE,text.font=1,xjust = -1.8, yjust = -2.7)

#####h=3
#miserror
miserror=result[,5][c(17,21,25,29)]
miserror_t=result[,5][c(18,22,26,30)]
miserror_s1=result[,5][c(19,23,27,31)]
miserror_s=result[,5][c(20,24,28,32)]
#c()
plot(A,miserror,pch = 16,lty=1,type = "o",col = "magenta", xlab = "|A|", ylab = "me(h=3)",ylim = c(0,0.5))
lines(A,miserror_t,pch=15,lty=1,col="green",type = "o")
lines(A,miserror_s1,pch=17,lty=1,col="blue",type = "o")
lines(A,miserror_s,pch=18,lty=1,col="black",type = "o")
legend(1.4,0.1,c("TLQQ", "QQ with only target", "QQ with target and all source","TLQQ with target and all source"), pt.cex = 0.9, pch = c(16,15,17,18),col=c("magenta","green","blue","black"),cex = 0.5, bty = "n",xpd=TRUE,text.font=1,xjust = -1.8, yjust = -2.7)

#####h=5
#miserror
miserror=result[,5][c(33,37,41,45)]
miserror_t=result[,5][c(34,38,42,46)]
miserror_s1=result[,5][c(35,39,43,47)]
miserror_s=result[,5][c(36,40,44,48)]
#c()
plot(A,miserror,pch = 16,lty=1,type = "o",col = "magenta", xlab = "|A|", ylab = "me(h=5)",ylim = c(0,0.5))
lines(A,miserror_t,pch=15,lty=1,col="green",type = "o")
lines(A,miserror_s1,pch=17,lty=1,col="blue",type = "o")
lines(A,miserror_s,pch=18,lty=1,col="black",type = "o")
legend(1.4,0.1,c("TLQQ", "QQ with only target", "QQ with target and all source","TLQQ with target and all source"), pt.cex = 0.9, pch = c(16,15,17,18),col=c("magenta","green","blue","black"),cex = 0.5, bty = "n",xpd=TRUE,text.font=1,xjust = -1.8, yjust = -2.7)




##############The part of application!!!
library(QCSIS)  #a package of variable correlation Ranking
library(caret)
library(dplyr)
library(factoextra)
library(cluster)
source("/Users/liu/Desktop/transfer learning/tex and code/TL-QQ.R")
##find the number of the same covariates
find=function(a,b,lab=NULL)
{
  for (i in a)
  {
    for (j in b){
      if (i==j){
        lab=c(lab,i)
      }
    }
  }
  return(lab)
}

#import target dataset: brain cortex
cortex=read.table("/Users/liu/Desktop/application data/gene_tpm_2017-06-05_v8_brain_cortex.gct",skip = 2, header = TRUE, sep = "\t",row.names = 1)
tar=cortex[,-(1:2)]
tar=t(tar)
tar=apply(tar,2,as.numeric)
tar_x=tar[,-51521]
tar_y=tar[,51521]
##Ranking of covariates and continuous response variable correlations
res=CQCSIS(tar_x, tar_y,5001)



#generate the discrete response variable
genedata=function(df,vec=numeric(nrow(df))){
  med=median(df[,1])
  vec[which(df[,1]<med)]=1
  df=cbind(vec,df)
  return(df)
}

##sample randomly from the dataset
sampledata=function(df,n,re=F){
  ind=sample(nrow(df),n,replace=re)
  df=df[ind,]
  return(df)
}



#df-non-transfer source 
selection=function(a,x,y,re=numeric(length(a)),n=100)
{
  for (i in (1:length(a))){
    re[i]=abs(cor(x[,a[i]],y))
  }
  ind=order(re)[1:n]
  return(a[ind])
}



#import the source datasets

blood=read.table("/Users/liu/Desktop/application data/gene_tpm_2017-06-05_v8_whole_blood.gct",skip = 2, header = TRUE, sep = "\t")
z=blood[,-(1:2)]
z=t(z)
z=apply(z,2,as.numeric)
z=na.omit(z)
z_x=z[,-51521]
z_y=z[,51521]
res_z=CQCSIS(z_x, z_y,56199)



muscle_skeletal=read.table("/Users/liu/Desktop/application data/gene_tpm_2017-06-05_v8_muscle_skeletal.gct",skip = 2, header = TRUE, sep = "\t")
a=muscle_skeletal[,-(1:2)]
a=t(a)
a=apply(a,2,as.numeric)
a=na.omit(a)
a_x=a[,-51521]
a_y=a[,51521]




stomach=read.table("/Users/liu/Desktop/application data/gene_tpm_2017-06-05_v8_stomach.gct",skip = 2, header = TRUE, sep = "\t")
b=stomach[,-(1:2)]
b=t(b)
b=apply(b,2,as.numeric)
b=na.omit(b)
b_x=b[,-51521]
b_y=b[,51521]



testis=read.table("/Users/liu/Desktop/application data/gene_tpm_2017-06-05_v8_testis.gct",skip = 2, header = TRUE, sep = "\t")
c=testis[,-(1:2)]
c=t(c)
c=apply(c,2,as.numeric)
c=na.omit(c)
c_x=c[,-51521]
c_y=c[,51521]
#res3=CQCSIS(c, c[,51521],101)


spleen=read.table("/Users/liu/Desktop/application data/gene_tpm_2017-06-05_v8_spleen.gct",skip = 2, header = TRUE, sep = "\t")
e=spleen[,-(1:2)]
e=t(e)
e=apply(e,2,as.numeric)
e=na.omit(e)
e_x=e[,-51521]
e_y=e[,51521]



b_hip=read.table("/Users/liu/Desktop/application data/gene_tpm_2017-06-05_v8_brain_hippocampus.gct",skip = 2, header = TRUE, sep = "\t")
g=b_hip[,-(1:2)]
g=t(g)
g=apply(g,2,as.numeric)
g=na.omit(g)
g_x=g[,-51521]
g_y=g[,51521]



bfc_cortex=read.table("/Users/liu/Desktop/application data/gene_tpm_2017-06-05_v8_brain_frontal_cortex_ba9.gct",skip = 2, header = TRUE, sep = "\t")
h=bfc_cortex[,-(1:2)]
h=t(h)
h=apply(h,2,as.numeric)
h=na.omit(h)
h_x=h[,-51521]
h_y=h[,51521]



brain_hemi=read.table("/Users/liu/Desktop/application data/gene_tpm_2017-06-05_v8_brain_cerebellar_hemisphere.gct",skip = 2, header = TRUE, sep = "\t")
i=brain_hemi[,-(1:2)]
i=t(i)
i=apply(i,2,as.numeric)
i=na.omit(i)
i_x=i[,-51521]
i_y=i[,51521]



###variable selection
##choose the covariates in source that are less correlated with the continuous response
s0=res_z$M[20000:56199]
s1=res_a$M[20000:56199]
#s2=res_b$M[5000:56199]
##choose the covariates in target that are highly correlated with the continuous response
tep0=find(res$M[1:3000],s0)
##choose the first 100 covariates in the intersection of tep0 and s1.
tep1=find(tep0,s1)
###the Serial number set of the selected covariates.
res_vari=tep1[1:100]





#####Organize into target data, test data and source data
Trandata=rbind(cbind(tar_y,tar_x[,res_vari]),cbind(g_y,g_x[,res_vari]),cbind(h_y,h_x[,res_vari]),cbind(i_y,i_x[,res_vari]))
##Data standardization
Trandata=scale(Trandata)
Trandata=genedata(Trandata) 

##generate the target data
target_train=Trandata[1:255,]

##generate the source datasets
source0=cbind(z_y,z_x[,res_vari])
source0=scale(source0)
source0=genedata(source0)
source0_data=sampledata(source0,200)
source0_z=source0_data[,1]
source0_y=source0_data[,2]
source0_x=source0_data[,-(1:2)]

source1=cbind(a_y,a_x[,res_vari])
source1=scale(source1)
source1=genedata(source1)
source1_data=sampledata(source1,200)
source1_z=source1_data[,1]
source1_y=source1_data[,2]
source1_x=source1_data[,-(1:2)]

source2=cbind(b_y,b_x[,res_vari])
source2=scale(source2)
source2=genedata(source2)
source2_data=sampledata(source2,200)
source2_z=source2_data[,1]
source2_y=source2_data[,2]
source2_x=source2_data[,-(1:2)]

source3=cbind(c_y,c_x[,res_vari])
source3=scale(source3)
source3=genedata(source3)
source3_data=sampledata(source3,200)
source3_z=source3_data[,1]
source3_y=source3_data[,2]
source3_x=source3_data[,-(1:2)]



source5=cbind(e_y,e_x[,res_vari])
source5=scale(source5)
source5=genedata(source5)
source5_data=sampledata(source5,200)
source5_z=source5_data[,1]
source5_y=source5_data[,2]
source5_x=source5_data[,-(1:2)]


source7=cbind(g_y,g_x[,res_vari])
source7=scale(source7)
source7=genedata(source7)
source7_data=sampledata(Trandata[256:452,],200,re=T)
source7_z=source7_data[,1]
source7_y=source7_data[,2]
source7_x=source7_data[,-(1:2)]

source8=cbind(h_y,h_x[,res_vari])
source8=scale(source8)
source8=genedata(source8)
source8_data=sampledata(Trandata[453:661,],200)
source8_z=source8_data[,1]
source8_y=source8_data[,2]
source8_x=source8_data[,-(1:2)]


source9=cbind(i_y,i_x[,res_vari])
source9=scale(source9)
source9=genedata(source9)
source9_data=sampledata(Trandata[662:876,],200)
source9_z=source9_data[,1]
source9_y=source9_data[,2]
source9_x=source9_data[,-(1:2)]





N=1
num=8     #number of source
n1=200
p=100
m = 5000		# number of MCMC chain 10000
burn = 1001
X_sou=array(0,c(n1,p,num,N))
y_sou=array(0,c(num,n1,N))
Z_sou=array(0,c(num,n1,N))
X_sou[,,1,1]=source0_x
X_sou[,,2,1]=source1_x
X_sou[,,3,1]=source2_x
X_sou[,,4,1]=source3_x
X_sou[,,5,1]=source5_x
X_sou[,,6,1]=source7_x
X_sou[,,7,1]=source8_x
X_sou[,,8,1]=source9_x




y_sou[1,,1]=source0_y
y_sou[2,,1]=source1_y
y_sou[3,,1]=source2_y
y_sou[4,,1]=source3_y
y_sou[5,,1]=source5_y
y_sou[6,,1]=source7_y
y_sou[7,,1]=source8_y
y_sou[8,,1]=source9_y



Z_sou[1,,1]=source0_z
Z_sou[2,,1]=source1_z
Z_sou[3,,1]=source2_z
Z_sou[4,,1]=source3_z
Z_sou[5,,1]=source5_z
Z_sou[6,,1]=source7_z
Z_sou[7,,1]=source8_z
Z_sou[8,,1]=source9_z





#TLQQ's result
ParaEst1 = ParaEst2 = vector("list", N)
mis.error.set1 = mis.error.set2 = rmspe.set1 = rmspe.set2 = rep(0, N)               # measure model error
beta1.L2norm.set1 = beta2.L2norm.set1 = eta.L2norm.set1 = beta1.L2norm.set2 = beta2.L2norm.set2 = eta.L2norm.set2 = rep(0, N)    ### measure parameter estimation accuracy
FPFN.set1 = FPFN.set2 = matrix(data=0, nrow = N, ncol = 6)
colnames(FPFN.set1) = colnames(FPFN.set2) = c("FPbeta1", "FNbeta1", "FPbeta2", "FNbeta2","FPeta","FNeta")


#QQ's result
ParaEst1_t = ParaEst2_t = vector("list", N)
mis.error.set1_t = mis.error.set2_t = rmspe.set1_t = rmspe.set2_t = rep(0, N)			                 ### measure model error
beta1.L2norm.set1_t = beta2.L2norm.set1_t = eta.L2norm.set1_t = beta1.L2norm.set2_t = beta2.L2norm.set2_t = eta.L2norm.set2_t = rep(0, N)    ### measure parameter estimation accuracy
FPFN.set1_t = FPFN.set2_t = matrix(data=0, nrow = N, ncol = 6)
colnames(FPFN.set1_t) = colnames(FPFN.set2_t) = c("FPbeta1", "FNbeta1", "FPbeta2", "FNbeta2","FPeta","FNeta")


#QQALL's result
ParaEst1_s1 = ParaEst2_s1 = vector("list", N)
mis.error.set1_s1 = mis.error.set2_s1 = rmspe.set1_s1 = rmspe.set2_s1 = rep(0, N)			                 ### measure model error
beta1.L2norm.set1_s1 = beta2.L2norm.set1_s1 = eta.L2norm.set1_s1 = beta1.L2norm.set2_s1 = beta2.L2norm.set2_s1 = eta.L2norm.set2_s1 = rep(0, N)    ### measure parameter estimation accuracy
FPFN.set1_s1 = FPFN.set2_s1 = matrix(data=0, nrow = N, ncol = 6)
colnames(FPFN.set1_s1) = colnames(FPFN.set2_s1) = c("FPbeta1", "FNbeta1", "FPbeta2", "FNbeta2","FPeta","FNeta")



#BTLQQALL's result
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

#cross validation
#N=5

##results of detection step
result=array(0,c(num,5,N))
lab=matrix(0,nrow = N,ncol = num)
#the threshold of detection !!!
M=0.1

Beta1_est=Beta2_est=Eta_est=Beta1_est_t=Beta2_est_t=Eta_est_t=Beta1_est_s=Beta2_est_s=Eta_est_s=Beta1_est_s1=Beta2_est_s1=Eta_est_s1=Beta1_est_sp1=Beta2_est_sp1=Eta_est_sp1=Beta1_est_sp2=Beta2_est_sp2=Eta_est_sp2=matrix(0,nrow=N,ncol=p+1)

##run the replications
for(w in 1:N)
{  
  ###generate the train dataset
  target_z=target_train[(20*w-19):(100*w),1]
  target_y=target_train[(20*w-19):(100*w),2]
  target_x=target_train[(20*w-19):(100*w),-(1:2)]
  ###generate the test dataset
  test_z=target_train[-((20*w-19):(100*w)),1]
  test_y=target_train[-((20*w-19):(100*w)),2]
  test_x=target_train[-((20*w-19):(100*w)),-(1:2)]

  
  ##target data with proposed algorithm
  ParaEst= TranBayes(target_x, target_y,target_z, m, burn, tau1_0 = rep(1,ncol(target_x)+1),tau2_0 =rep(1,ncol(target_x)+1), tau3_0 = rep(1,ncol(target_x)+1),lambda1_0=1, lambda2_0=1, lambda3_0=1)
  
  ##source data and detection
  #w=1
  for (i in 1:num) {
    ##test
    #i=1
    #w=1
    #test
    X_c = X_sou[,,i,1]
    y_c = y_sou[i,,1]
    Z_c = Z_sou[i,,1]
    result[i,,1]=istransferable(ParaEst,target_x,target_y,target_z,X_c,y_c,Z_c,m,burn,M)
    #result[i,1,w] denotes the contrast and result[i,2,w] denotes istransferable or not 
    
    if (result[i,2,1]==1){
      lab[1,i]=i
    }
  }
  
  ###transferable data
  X_s = NULL
  y_s = NULL
  Z_s = NULL
  for (i in lab[1,]){
    if (i>0){
      X_s=rbind(X_s,X_sou[,,i,1])
      y_s=c(y_s,y_sou[i,,1])
      Z_s=c(Z_s,Z_sou[i,,1])
    }
  }
  
  #all source data
  X_alls = NULL
  y_alls = NULL
  Z_alls = NULL
  for (i in 1:num){
    X_alls=rbind(X_alls,X_sou[,,i,1])
    y_alls=c(y_alls,y_sou[i,,1])
    Z_alls=c(Z_alls,Z_sou[i,,1])
  }
  
  X_alls1=rbind(X_alls,target_x)
  y_alls1=c(y_alls,target_y)
  Z_alls1=c(Z_alls,target_z)
  
  
  
  #######################################################
  ############# 1st setting for the tunning parameters
  #######################################################
  
  ###proposed algorithm--BTLQQ!!!
  me1=numeric(4)
  rmse1=numeric(4)
  for (j in (1:4)){
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
      
      ParaEst2[[w]]=TranDebias(target_x, target_y, target_z, m, burn, ParaEst1[[w]], xi1_0 = rep(1,ncol(target_x)+1), xi2_0 =rep(1,ncol(target_x)+1), xi3_0 = rep(1,ncol(target_x)+1), rho1_0=1, rho2_0=1, rho3_0=1)
      ###estimates of final beta/eta
      Beta1_est[w,]= ParaEst1[[w]]$beta1_est+ParaEst2[[w]]$delta1_est
      Beta2_est[w,]= ParaEst1[[w]]$beta2_est+ParaEst2[[w]]$delta2_est
      Eta_est[w,]= ParaEst1[[w]]$eta_est+ParaEst2[[w]]$deltaeta_est
      all_beta1=ParaEst1[[w]]$all_beta1+ParaEst2[[w]]$all_delta1
      all_beta2=ParaEst1[[w]]$all_beta2+ParaEst2[[w]]$all_delta2
      all_eta=ParaEst1[[w]]$all_eta+ParaEst2[[w]]$all_deltaeta
    }
    
    ############# prediction results  
    pred_result1 = vs.predict.gibbs1(Beta1_est[w,],Beta2_est[w,],Eta_est[w,],all_beta1 = all_beta1,all_beta2 = all_beta2,all_eta = all_eta,test_x)
    ### misclassification error rate for qualitative response Z
    me1[j]=mean(abs(pred_result1$predZ -test_z))
    rmse1[j]=sqrt(mean((unlist(pred_result1$predy) - test_y)^2))
  }
  mis.error.set1[w]=mean(me1)
  rmspe.set1[w]=mean(rmse1)
  
  #Plot of the true y of testing data and 95% prediction intervals 
  CI= data.frame(real.y=test_y,lower.y=apply(pred_result1$allpredy[,-(1:1000)],1,quantile,probs=alpha/2),upper.y=apply(pred_result1$allpredy[,-(1:1000)],1,quantile,probs=1-alpha/2))
  plot(seq(1,length(test_y)),CI[,1],type = 'l',col="red",lty=1,ylim = c(-2,6),xlab = "Index")
  lines(seq(1,length(test_y)),CI[,2],lty=1)
  lines(seq(1,length(test_y)),CI[,3],lty=1)
  
  
  ###QQ!!!
  me2=numeric(4)
  rmse2=numeric(4)
  for (j in (1:4)){
    ParaEst= TranBayes(target_x, target_y,target_z, m, burn, tau1_0 = rep(1,ncol(target_x)+1),tau2_0 =rep(1,ncol(target_x)+1), tau3_0 = rep(1,ncol(target_x)+1),lambda1_0=1, lambda2_0=1, lambda3_0=1)
    Beta1_est_t[w,]= ParaEst$beta1_est
    Beta2_est_t[w,]= ParaEst$beta2_est
    Eta_est_t[w,]= ParaEst$eta_est
    
    ############# prediction results  
    pred_result1_t = vs.predict.gibbs(Beta1_est_t[w,],Beta2_est_t[w,],Eta_est_t[w,], test_x)
    me2[j]=mean(abs(pred_result1_t$predZ -test_z))
    rmse2[j]=sqrt(mean((unlist(pred_result1_t$predy) - test_y)^2))
  }
  mis.error.set1_t[w]=mean(me2)
  rmspe.set1_t[w]=mean(rmse2)
  ### misclassification error rate for qualitative response Z
  #mis.error.set1_t[w] = mean(abs(pred_result1_t$predZ - test_z))	
  ### mse for y
  #rmspe.set1_t[w] = sqrt(mean((unlist(pred_result1_t$predy) - test_y)^2))
  
  
  
  
  ###QQALL!!!
  me3=numeric(4)
  rmse3=numeric(4)
  for (j in 1:4){
    ParaEst1_s1[[w]] = TranBayes(X_alls1, y_alls1, Z_alls1, m, burn, 
                                 tau1_0 = rep(1,ncol(X_alls1)+1),
                                 tau2_0 =rep(1,ncol(X_alls1)+1), 
                                 tau3_0 = rep(1,ncol(X_alls1)+1),
                                 lambda1_0=1, lambda2_0=1, lambda3_0=1)
    
    ###estimates of final beta/eta
    Beta1_est_s1[w,]= ParaEst1_s1[[w]]$beta1_est
    Beta2_est_s1[w,]= ParaEst1_s1[[w]]$beta2_est
    Eta_est_s1[w,]= ParaEst1_s1[[w]]$eta_est
    
    ############# prediction results  
    pred_result1_s1 = vs.predict.gibbs(Beta1_est_s1[w,],Beta2_est_s1[w,],Eta_est_s1[w,], test_x)
    me3[j]=mean(abs(pred_result1_s1$predZ -test_z))
    rmse3[j]=sqrt(mean((unlist(pred_result1_s1$predy) - test_y)^2))
  }
  mis.error.set1_s1[w]=mean(me3)
  rmspe.set1_s1[w]=mean(rmse3)
  ### misclassification error rate for qualitative response Z
  #mis.error.set1_s1[w] = mean(abs(pred_result1_s1$predZ - test_z))	
  ### mse for y
  #rmspe.set1_s1[w] = sqrt(mean((unlist(pred_result1_s1$predy) - test_y)^2))  
  
  
  
  ###BTLQQALL!!!
  me4=numeric(4)
  rmse4=numeric(4)
  for (j in 1:4){
    ParaEst1_s[[w]] = TranBayes(X_alls, y_alls, Z_alls, m, burn, 
                                tau1_0 = rep(1,ncol(X_alls)+1),
                                tau2_0 =rep(1,ncol(X_alls)+1), 
                                tau3_0 = rep(1,ncol(X_alls)+1),
                                lambda1_0=1, lambda2_0=1, lambda3_0=1)
    
    ParaEst2_s[[w]]=TranDebias(target_x, target_y,target_z, m, burn, ParaEst1_s[[w]], 
                               xi1_0 = rep(1,ncol(target_x)+1), xi2_0 =rep(1,ncol(target_x)+1), 
                               xi3_0 = rep(1,ncol(target_x)+1), rho1_0=1, rho2_0=1, rho3_0=1)
    ###estimates of final beta/eta
    Beta1_est_s[w,]= ParaEst1_s[[w]]$beta1_est+ParaEst2_s[[w]]$delta1_est
    Beta2_est_s[w,]= ParaEst1_s[[w]]$beta2_est+ParaEst2_s[[w]]$delta2_est
    Eta_est_s[w,]= ParaEst1_s[[w]]$eta_est+ParaEst2_s[[w]]$deltaeta_est
    
    ############# prediction results  
    pred_result1_s = vs.predict.gibbs(Beta1_est_s[w,],Beta2_est_s[w,],Eta_est_s[w,], test_x)
    me4[j]=mean(abs(pred_result1_s$predZ -test_z))
    rmse4[j]=sqrt(mean((unlist(pred_result1_s$predy) - test_y)^2))
  }
  mis.error.set1_s[w]=mean(me4)
  rmspe.set1_s[w]=mean(rmse4)

  
  
  
  
  
  
  
  
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
  ###save the application result as a RData file.
  save.image("application_data.RData")
}








