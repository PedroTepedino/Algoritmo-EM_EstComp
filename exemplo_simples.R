#################
#Dados
#################
n <- 500
X <- rnorm(n)
Y <- 2 + 3 * X + rnorm(n)
freq_na = 0.9
X_na <- sample(1:n, round(freq_na*n))

dados <- tibble(Y, X)
dados$X[X_na] = NA
##################

#Chute Inicial
b0_E = -4
b1_E = -6

tolerance = 0.01

convergiu <- FALSE

n = length(x)
x_E = X
na_obs <- which(is.na(x_E))

while(!convergiu)
{
  coef_lm <- coef(lm(Y~X))

  if (any(is.na(x_E)))
  {
    x_E[na_obs] = (Y[na_obs] - b0_E)/ b1_E
  }

  modelo_E <- lm(Y ~ x_E)
  novos_par <- coef(modelo_E)

  convergiu <- sum(abs(novos_par - c(b0_E, b1_E))) < tolerance

  b0_E <- novos_par[1]
  b1_E <- novos_par[2]
}
b0_E
b1_E

