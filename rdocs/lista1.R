pacman::p_load(LearnBayes,tidyverse)
# 1 ----
## a ----
### p = 0.5 ----
#### Frequentista ----

v <- numeric()
n = 12
p = .5
for (i in 1:1000) {
  x <- rbinom(n, 1, p)
  p_hat <- mean(x)
  se <- sqrt(p_hat * (1 - p_hat) / n) 
  z <- qnorm(0.975) 
  ci <- c(p_hat - z * se, p_hat + z * se)
  v <- c(v, ci[1] <= p & ci[2] >= p)
}
sum(v) / length(v)

# pelo p-valor:
v = numeric()
for (i in 1:1000){
  x = sum(rbinom(n,1,p))
  dbinom(0:n,n,p)
  v = append(v,sum(dbinom(x:n,n,p)))
}
sum((v > .05)/1000)

#### Bayesiano ----

v <- numeric()
beta.par = c(1,1)
for (i in 1:1000){
  s = sum(rbinom(n,1,p))
  beta.post.par <- beta.par + c(s, n-s)
  post.sample <- rbeta(1000, beta.post.par[1], beta.post.par[2])
  ci = quantile(post.sample, c(0.025, 0.975))
  v = append(v,ci[1] <= p & ci[2] >= p)
}
sum(v)/1000

### p = 0.7 ----
#### Frequentista ----

v <- numeric()
n = 12
p = .7
for (i in 1:1000) {
  x <- rbinom(n, 1, p)
  p_hat <- mean(x)
  se <- sqrt(p_hat * (1 - p_hat) / n) 
  z <- qnorm(0.975) 
  ci <- c(p_hat - z * se, p_hat + z * se)
  v <- c(v, ci[1] <= p & ci[2] >= p)
}
sum(v) / length(v)

# pelo p-valor:
v = numeric()
for (i in 1:1000){
  x = sum(rbinom(n,1,p))
  dbinom(0:n,n,p)
  v = append(v,sum(dbinom(x:n,n,p)))
}
sum((v > .05)/1000)

#### Bayesiano ----

v <- numeric()
beta.par = c(1,1)
for (i in 1:1000){
  s = sum(rbinom(n,1,p))
  beta.post.par <- beta.par + c(s, n-s)
  post.sample <- rbeta(1000, beta.post.par[1], beta.post.par[2])
  ci = quantile(post.sample, c(0.025, 0.975))
  v = append(v,ci[1] <= p & ci[2] >= p)
}
sum(v)/1000

### p = 0.9 ----
#### Frequentista ----
v <- numeric()
n = 12
p = .9
for (i in 1:1000) {
  x <- rbinom(n, 1, p)
  p_hat <- mean(x)
  se <- sqrt(p_hat * (1 - p_hat) / n) 
  z <- qnorm(0.975) 
  ci <- c(p_hat - z * se, p_hat + z * se)
  v <- c(v, ci[1] <= p & ci[2] >= p)
}
sum(v) / length(v)

# pelo p-valor:
v = numeric()
for (i in 1:1000){
  x = sum(rbinom(n,1,p))
  dbinom(0:n,n,p)
  v = append(v,sum(dbinom(x:n,n,p)))
}
sum((v > .05)/1000)

#### Bayesiano ----

v <- numeric()
beta.par = c(1,1)
for (i in 1:1000){
  s = sum(rbinom(n,1,p))
  beta.post.par <- beta.par + c(s, n-s)
  post.sample <- rbeta(1000, beta.post.par[1], beta.post.par[2])
  ci = quantile(post.sample, c(0.025, 0.975))
  v = append(v,ci[1] <= p & ci[2] >= p)
}
sum(v)/1000

# ---------------------------------------------------------------------------- #

## b ----
### p = 0.5 ----
#### Frequentista ----

# pelo p-valor:
v = numeric()
for (i in 1:1000){
  s = sum(rbinom(n,1,p))
  x = 1-sum(dnbinom(0:(x-1),n-2,p))
  x = sum(rnbinom(n,1,p))
  v = append(v,sum(dbinom(x:n,n,p)))
}
sum((v > .05)/1000)

#### Bayesiano ----

v <- numeric()
beta.par = c(1,1)
for (i in 1:1000){
  s = sum(rnbinom(n,1,p))
  beta.post.par <- beta.par + c(s, n-s)
  post.sample <- rbeta(1000, beta.post.par[1], beta.post.par[2])
  ci = quantile(post.sample, c(0.025, 0.975))
  v = append(v,ci[1] <= p & ci[2] >= p)
}
sum(v)/1000

