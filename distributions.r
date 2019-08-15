set.seed(1234)
runif(4, min=0, max=1)
x <- runif(10)
hist(x)
x <- runif(1000000)
hist(x)

library(ggplot2)
uniform_pdf <- function(x) {
  ifelse(x>=0 & x<=1, 1L, 0L)
}
ggplot(data.frame(x=c(-1, 2)), aes(x)) +
  stat_function(fun=uniform_pdf) +
  ylab("Uniform PDF") + theme_bw()

ggplot(data.frame(x=c(-4, 4)), aes(x)) +
  stat_function(fun=dnorm) +
  ylab("Normal PDF") + theme_bw()

dataset <- rnorm(1000, 10, 10)
mean(dataset)
hist(dataset)
sd(dataset)


inv_exp_cdf <- function(z) (-log(1-z))
ggplot(data.frame(u=c(0, 0.99)), aes(u)) +
  stat_function(fun=inv_exp_cdf) +
  xlab("u") + ylab("Inverse CDF") + theme_bw()

NSim = 10 ^ 4
U = runif(NSim)
X = -log(U)
Y = rexp(NSim)
par(mfrow=c(1,2))
hist(X, freq = F, main = "Exp from Uniform")
hist(Y, freq = F, main = "Exp From R")

NSim = 1000000
a = 2.7
b = 6.3
M = 2.67
u = runif(NSim, max=M)
y = runif(NSim)
x = y[u<dbeta(y, a, b)]
hist(x)

library(purrr)
library(VGAM)

ggplot(data.frame(x=c(-4, 4)), aes(x)) +
  stat_function(fun=pnorm) +
  xlab("x") + ylab("Normal CDF") +
  annotate("text", x = 2, y = 0.5, label = "mu = 0, sigma = 1") +
  theme_bw()

ggplot(data.frame(x=c(-4, 4)), aes(x)) + stat_function(fun=dlaplace) + xlab("x") + ylab("Laplace PDF") +
annotate("text", x = 0, y = 0.1, label = "a = 0, b = 1") + theme_bw()

bern_pdf <- function(x) {
  p <- 0.25
  (ifelse(x==0, 1-p, ifelse(x == 1, p, 0)))}
ggplot(data.frame(x=c(-1, 2)), aes(x)) +
  stat_function(fun=bern_pdf, geom = "step", n=31) +
  xlab("x") + ylab("Bernoulli PMF") +
  annotate("text", x = 1, y = 0.5, label = "p = 0.25") + theme_bw()

bern_cdf <- function(x) {
  p <- 0.25
  (ifelse(x<0, 0, ifelse(x < 1, 1-p, 1)))}
ggplot(data.frame(x=c(-1, 2)), aes(x)) +
  stat_function(fun=bern_cdf, geom="step") +
  xlab("x") + ylab("Bernoulli CDF") +
  annotate("text", x = 1, y = 0.5, label = "p = 0.25") + theme_bw()

binom_pdf <- partial(dbinom, size=5, prob=0.25)
ggplot(data.frame(x=c(-1, 6)), aes(x)) + stat_function(fun=binom_pdf, geom = "step", n=71) + xlab("x") + 
  ylab("Binomial PMF") + annotate("text", x = 4, y = 0.3, label = "p = 0.25, N = 5") + theme_bw()

ggplot(data.frame(x=c(-1, 6)), aes(x)) + stat_function(fun=partial(pbinom, size=5, prob=0.25),
geom = "step") + xlab("x") + ylab("Binomial CDF") + annotate("text", x = 3, y = 0.5, label = "p = 0.25, N = 5") + theme_bw()

pois_pdf <- partial(dpois,lambda=1)
ggplot(data.frame(x=c(-1, 6)), aes(x)) + stat_function(fun=pois_pdf, geom = "step", n=71) +
xlab("x") + ylab("Poisson PMF") + annotate("text", x = 4, y = 0.3, label = "lambda = 1") + theme_bw()

ggplot(data.frame(x=c(-1, 7)), aes(x)) +
stat_function(fun=partial(ppois, lambda=1), geom = "step") +
xlab("x") + ylab("Poisson CDF") +
annotate("text", x = 4, y = 0.5, label = "lambda = 1") + theme_bw()
