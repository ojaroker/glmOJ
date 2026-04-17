## code to prepare `DATASET` dataset goes here

Greenberg26.dat = read.csv("~/Colgate/Math/MATH454/Homework 3/Greenberg26.csv")
Greenberg26.dat$metro = as.factor(Greenberg26.dat$metro)
Greenberg26.dat$EPAregion = as.factor(Greenberg26.dat$EPAregion)

usethis::use_data(Greenberg26.dat, overwrite = TRUE)

Dahir25.dat <- read.csv("~/Colgate/Math/MATH454/Homework 4/cs_replication_data.csv")
Dahir25.dat$modal_zone <- as.factor(Dahir25.dat$modal_zone)
Dahir25.dat$city       <- as.factor(Dahir25.dat$city)

cols_to_scale <- c("pnhwht", "pnhblk", "mhmval", "entropy",
                   "total_crime_rate", "pop", "hinc", "pvac")
for (col in cols_to_scale) {
  Dahir25.dat[[col]] <- ave(Dahir25.dat[[col]], Dahir25.dat$city,
                             FUN = function(x) as.vector(scale(x)))
}

usethis::use_data(Dahir25.dat, overwrite = TRUE)

# ZITweedie.dat -----------------------------------------------------------
# Count response with excess zeros, for demonstrating zeroinflTweedieGLM.
# Simulated from a compound Poisson-Gamma (Tweedie, p=1.5) with structural
# zeros, then ceiling()-ed to produce non-negative integer counts.
# ceiling() is intentional: this package targets count responses only.
#
# True model:
#   count component : log(mu) = 1.8 + 0.9*x1        (phi=2.0, p=1.5)
#   ZI component    : logit(pi) = 0.5 - 2.5*x2      (x2 is INDEPENDENT of x1)
#
# Design rationale:
#   - High mu (intercept=1.8) means Tweedie natural zeros are rare (~8% at
#     x1=0). The base Tweedie model cannot explain the ~55% observed zeros by
#     adjusting the mean — it has no x2 in the count component to absorb them.
#   - ZI is driven purely by x2, which is orthogonal to the count predictor x1.
#     The base Tweedie fit (y ~ x1 + x2) will pick up a spurious x2 coefficient
#     but still severely under-predict zeros, and DHARMa flags this cleanly.
#   - Note: countGLM() always fits zeroinfl_tweedie alongside tweedie (DHARMa
#     ZI detection is unreliable for Tweedie because glmmTMB can absorb excess
#     zeros by inflating phi). AIC/BIC arbitrates between the two.

set.seed(789)
n <- 400

x1 <- rnorm(n)   # count predictor
x2 <- rnorm(n)   # ZI predictor (independent of x1)

# Tweedie parameters — p well inside (1,2), won't hit boundary
phi <- 2.0
p   <- 1.5
mu  <- exp(1.8 + 0.9 * x1)   # count mean depends only on x1

# Compound Poisson-Gamma parameterisation of Tweedie(mu, phi, p):
#   N_i ~ Poisson(lambda_i),  lambda_i = mu_i^(2-p) / (phi*(2-p))
#   G_j ~ Gamma(alpha, scale=theta_i), alpha=(2-p)/(p-1), theta_i=phi*(p-1)*mu_i^(p-1)
#   Y_i = sum_{j=1}^{N_i} G_j   (0 when N_i = 0)
lambda_cp <- mu^(2 - p) / (phi * (2 - p))
alpha_cp  <- (2 - p) / (p - 1)

y_tw <- vapply(seq_len(n), function(i) {
  N <- rpois(1, lambda_cp[i])
  if (N == 0L) return(0)
  theta_i <- phi * (p - 1) * mu[i]^(p - 1)
  sum(rgamma(N, shape = alpha_cp, scale = theta_i))
}, numeric(1))

# Strong ZI component driven purely by x2:
#   high x2  => low pi_zi => mostly non-zero observations
#   low x2   => high pi_zi => mostly structural zeros
# Average pi_zi over x2~N(0,1) is ~62%, giving ~57% total zeros in the data
# (natural Tweedie zeros already account for ~8% of remaining observations).
pi_zi <- plogis(0.5 - 2.5 * x2)
zi    <- as.logical(rbinom(n, 1L, pi_zi))
y     <- ifelse(zi, 0, y_tw)

y = ceiling(y)
ZITweedie.dat <- data.frame(y = y, x1 = x1, x2 = x2)
rm(n, x1, x2, phi, p, mu, lambda_cp, alpha_cp, y_tw, pi_zi, zi, y)

usethis::use_data(ZITweedie.dat, overwrite = TRUE)
