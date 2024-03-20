X <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
Y <- 5 * X^2 + 3 * X + 7
n <- length(X)

PolynomialReg <- function(X, Y) {
    detA0Upper <- detA0UpperCalculate(X, Y)
    detBottom <- detBottomCalculate(X, Y)

    detA1Upper <- detA1UpperCalculate(X, Y)
    detA2Upper <- detA2UpperCalculate(X, Y)

    A0 <- detA0Upper / detBottom
    A1 <- detA1Upper / detBottom
    A2 <- detA2Upper / detBottom

    print(A0)
    print(A1)

    plot(X, Y)

    lines(X, A2 * X^2 + A1 * X + A0, type = "l", lty = 1, col = "red")
}

detBottomCalculate <- function(X, Y) {
    sumx4 <- Summary(X^4)
    sumx3 <- Summary(X^3)
    sumx2 <- Summary(X^2)
    sumx <- Summary(X)

    detBottom <- sumx4 * CalcDet(sumx2, sumx, sumx, n) -
        sumx3 * CalcDet(sumx3, sumx, sumx2, n) +
        sumx2 * CalcDet(sumx3, sumx2, sumx2, sumx)

    return(detBottom)
}

detA0UpperCalculate <- function(X, Y) {
    sumx4 <- Summary(X^4)
    sumx3 <- Summary(X^3)
    sumx2y <- Summary(X^2, Y)
    sumx2 <- Summary(X^2)
    sumxy <- Summary(X, Y)
    sumx <- Summary(X)
    sumy <- Summary(Y)

    detUpper <- sumx4 * CalcDet(sumx2, sumxy, sumx, sumy) -
        sumx3 * CalcDet(sumx3, sumxy, sumx2, sumy) +
        sumx2y * CalcDet(sumx3, sumx2, sumx2, sumx)

    return(detUpper)
}

detA1UpperCalculate <- function(X, Y) {
	sumx4 <- Summary(X^4)
	sumx2y <- Summary(X^2, Y)
	sumx2 <- Summary(X^2)
	sumx3 <- Summary(X^3)
	sumxy <- Summary(X,Y)
	sumx <- Summary(X)
	sumy <- Summary(Y)
	
	detUpper <- sumx4 * CalcDet(sumxy, sumx, sumy, n) -
		sumx2y * CalcDet(sumx3, sumx, sumx2, n) +
		sumx2 * CalcDet(sumx3, sumxy, sumx2, sumy)

	return(detUpper)

}

detA2UpperCalculate <- function(X, Y) {
	sumx2y <- Summary(X^2, Y)
	sumx3 <- Summary(X^3)
	sumx2 <- Summary(X^2)
	sumxy <- Summary(X, Y)
	sumx2 <- Summary(X^2)
	sumx <- Summary(X)
	sumy <- Summary(Y)
	
	detUpper <- sumx2y * CalcDet(sumx2, sumx, sumx, n) -
		sumx3 * CalcDet(sumxy, sumx, sumy, n) +
		sumx2 * CalcDet(sumxy, sumx2, sumy, sumx)

	return(detUpper)
}

Summary <- function(X, Y) {
    sum <- 0
    if (missing(Y)) {
        for (i in 1:n)
        {
            sum <- sum + X[i]
        }
        return(sum)
    }

    for (i in 1:n)
    {
        sum <- sum + (X[i] * Y[i])
    }
    return(sum)
}

CalcDet <- function(a, b, c, d) {
    total <- (a * d) - (b * c)

    return(total)
}

PolynomialReg(X, Y)