linreg <- function(X, Y) {
  plot(X, Y)
  n <- length(X)
  sum_XY <- sum(X * Y)
  sum_X <- sum(X)
  sum_Y <- sum(Y)
  sum_X_square <- sum(X^2)

  b <- (n * sum_XY - sum_X * sum_Y) / (n * sum_X_square - sum_X^2)
  a <- (sum_Y - b * sum_X) / n

  curve(a * exp(b * x), col = "blue", add = TRUE)

	lines(X, exp(a*X) * exp(b), type="l" col="red");
}

X <- c(1, 2, 3, 4)
Y <- c(21.6, 163.79, 1210.29, 8942.87)
linreg(X, log(Y))
