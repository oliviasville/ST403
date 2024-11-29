df <- with(mtcars, data.frame(
  y = mpg,
  x1 = disp,
  x2 = hp,
  x3 = wt
))

#### 3.2

nll_lm <- function(data, par) {
  beta <- par[1:(ncol(data))]
  sigma <- par[length(par)]
  if (sigma <= 0) return(Inf)

  X <- as.matrix(cbind(1, data[, -1]))

  epsilon <- data$y - X %*% beta

  n <- nrow(data)
  nll <- n * log(sigma) + (1 / (2 * sigma^2)) * sum(epsilon^2)
  return(nll)
}


#### 3.3
init_par <- c(mean(df$y), rep(0, ncol(df) - 1), sd(df$y))

lower_bounds <- c(rep(-Inf, ncol(df)), 1e-6)
upper_bounds <- rep(Inf, ncol(df) + 1)

optim_res <- optim(
  par = init_par,
  fn = nll_lm,
  data = df,
  method = "L-BFGS-B",
  lower = lower_bounds,
  upper = upper_bounds
)

beta_optim <- optim_res$par[1:(ncol(df))]
sigma_optim <- optim_res$par[length(optim_res$par)]

print(beta_optim)
print(sigma_optim)

#### 3.4

##The negative log-likelihood function is needed because optim() minimizes by default. Maximizing the likelihood is equivalent to minimizing the negative log-likelihood, making it suitable for optimization with optim().


#### 3.5 + 3.6
X <- as.matrix(cbind(1, df[, -1]))
y <- df$y

beta_matrix <- solve(t(X) %*% X) %*% t(X) %*% y

residuals <- y - X %*% beta_matrix
sigma_matrix <- sqrt(sum(residuals^2) / nrow(X))


beta_comparison <- data.frame(
  Coefficient = c("Intercept", colnames(df)[-1]),
  Beta_Optim = beta_optim,
  Beta_Matrix = as.vector(beta_matrix),
  Difference = beta_optim - as.vector(beta_matrix)
)

print(beta_comparison)

sigma_comparison <- data.frame(
  Method = c("Optim", "Matrix"),
  Sigma = c(sigma_optim, sigma_matrix),
  Difference = c(sigma_optim - sigma_matrix, NA)
)

print(sigma_comparison)


### exercise 4
lm_model <- lm(y ~ x1 + x2 + x3, data = df)

beta_lm <- coef(lm_model)

sigma_lm <- summary(lm_model)$sigma

print(beta_lm)

print(sigma_lm)

