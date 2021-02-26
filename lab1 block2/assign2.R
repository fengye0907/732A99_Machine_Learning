set.seed(1234567890)
max_it <- 100 # max number of EM iterations
min_change <- 0.1 # min change in log likelihood between two 
                  # consecutive EM iterations
N=1000 # number of training points
D=10 # number of dimensions
x <- matrix(nrow=N, ncol=D) # training data
true_pi <- vector(length = 3) # true mixing coefficients
true_mu <- matrix(nrow=3, ncol=D) # true conditional distributions
true_pi=c(1/3, 1/3, 1/3)
true_mu[1,]=c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
true_mu[2,]=c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
true_mu[3,]=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
plot(true_mu[1,], type="o", col="blue", ylim=c(0,1))
points(true_mu[2,], type="o", col="red")
points(true_mu[3,], type="o", col="green")
# Producing the training data
for(n in 1:N) {
  k <- sample(1:3,1,prob=true_pi)
  for(d in 1:D) {
    x[n,d] <- rbinom(1,1,true_mu[k,d])
  }
}

K=4 # number of guessed components
z <- matrix(nrow=N, ncol=K) # fractional component assignments
pi <- vector(length = K) # mixing coefficients
mu <- matrix(nrow=K, ncol=D) # conditional distributions
llik <- vector(length = max_it) # log likelihood of the EM iterations

# Random initialization of the paramters
pi <- runif(K,0.49,0.51)
pi <- pi / sum(pi)
for(k in 1:K) {
  mu[k,] <- runif(D,0.49,0.51) 
}
pi
mu

################################################################################
for(it in 1:max_it) {
  plot(mu[1,], type="o", col="blue", ylim=c(0,1))
  points(mu[2,], type="o", col="red")
  # points(mu[3,], type="o", col="green")
  # points(mu[4,], type="o", col="black")
  # points(mu[5,], type="o", col="orange")
  # Sys.sleep(0.5)
  
  # E-step: Computation of the fractional component assignments
  mux <- matrix(nrow=N, ncol=K)
  for (n in 1:N) {
    for (k in 1:K) {
      mux[n,k] <- prod(mu[k,]^x[n,],(1-mu[k,])^(1-x[n,]))
    }
  }
  z <- t(pi*t(mux))/rowSums(t(pi*t(mux)))
  #Log likelihood computation.
  # E <- sum(log(rowSums(t(pi*t(mux)))))
  E <- 0
  for (n in 1:N) {
    for (k in 1:K) {
      a <- 0
        for (i in 1:D) {
          a <- a+x[n,i]*log(mu[k,i])+(1-x[n,i])*log(1-mu[k,i])
        }
      a <- log(pi[k])+a
      E <- E+z[n,k]*a
    }
  }
  
  llik[it] <- E
  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
  # flush.console()
  # Stop if the lok likelihood has not changed significantly
  if(it!=1){
    dis <- abs(llik[it]-llik[it-1])
    stopifnot(dis>=min_change)
  }
  #M-step: ML parameter estimation from the data and fractional component 
  # assignments
  pi <- colSums(z)/N
  for (k in 1:K) {
    for (i in 1:D) {
      mu[k,i] <- x[,i]%*%z[,k]/sum(z[,k])
    }
  }
}
pi
mu
plot(llik[1:it], type="o")
