V_values = c(3, 5, 7, 9, 11)
U_values = log10(V_values)

linear_regression <- function(X, Y) {
    n <- length(X)
    plot(X, Y)

    sum_Y <- sum(Y)
    sum_X <- sum(X)
    sum_XY <- sum(X * Y)
    sum_X_squared <- sum(X^2)

    sum_a1 <- (sum_XY * n) - (sum_Y * sum_X)
    sum_a2 <- (sum_X_squared * n) - (sum_X * sum_X)
    a <- sum_a1/sum_a2

    sum_b1 <- (sum_X_squared * sum_Y) - (sum_X * sum_XY)
    sum_b2 <- (sum_X_squared * n) - (sum_X * sum_X)
    b <- sum_b1 / sum_b2

    cat("(a) =", a, "\n")
    cat("(b) =", b, "\n")

    e <- 2.71  

    lines(X, a * e^(b * X), type = "l", lty = 1, col = "blue")
}
linear_regression(U_values, V_values)