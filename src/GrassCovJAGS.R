
  model{
    # priors
    sigma ~ dunif(0, 1) # dunif(alpha = lower limit, beta = upper limit)
    b0 ~ dnorm(0, (1/2.7)) # dnorm(mu = mean, tau= precision )
    b1 ~ dnorm(0, (1/2.7))
    b2 ~ dnorm(0, (1/2.7))
    b3 ~ dnorm(0, (1/2.7))
    b4 ~ dnorm(0, (1/2.7))
    # likelihood
    for (i in 1:n) {
      # deterministic model
      mu[i] <- ilogit(
         b0
         + b1*x_mtrx[i,2]
         + b2*x_mtrx[i,3]                   
         + b3*x_mtrx[i,4]
         + b4*x_mtrx[i,5]
      )
      # likelihood parameters
      a[i] <- (mu[i]^2 - mu[i]^3 - mu[i]*sigma^2) / sigma^2
      b[i] <- (mu[i] - 2*mu[i]^2 + mu[i]^3 - sigma^2 + mu[i]*sigma^2) / sigma^2
      # likelihood
        # returns density (for continuous) because l.h.s. is data (deterministic b/c defined in data) 
        y[i] ~ dbeta(a[i], b[i]) 
      # posterior predictive distribution of y.new (for model checking)
        # returns random number generator because l.h.s. is not data (i.e. it is unknown/stochastic node)
        y_sim[i]  ~ dbeta(a[i], b[i]) 
    }
    # Derived quantities
    #posterior predictive checks
      mean_y = mean(y)
      sd_y = sd(y)
      mean_y_sim = mean(y_sim)
      sd_y_sim = sd(y_sim)
      p_val_mean = step(mean_y_sim - mean_y)
      p_val_sd = step(sd_y_sim - sd_y)
  }
  
