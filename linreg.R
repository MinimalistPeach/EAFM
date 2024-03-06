linreg <- function(X,Y)
{
	plot(X,Y);
	sumA = 0;
	sumB = 0;
	sumC = 0;
	sumASquare = 0;
	detAFirst = 0;
	detASecond = 0;
	a = 0;
	for(i in 1:length(X))
	{
		sumA = sumA + X[i] * Y[i];
		sumB = sumB + X[i];
		sumC = sumC + Y[i];
		sumASquare = sumASquare + X[i]^2;
	}

	detAFirst = sumA * length(X) - sumC * sumB;
	detASecond = sumASquare * length(X) - sumB * sumB;
	a = detAFirst / detASecond;

	detBFirst = sumASquare * sumC - sumB * sumA;
	detBSecond = sumASquare * length(X) - sumB * sumB;
	b = detBFirst / detBSecond;
	
	lines(X, a*X+b, type = "l", lty = 1, col="blue")

	print(a);
	print(b);
}

X = c(1,2,3,4,5);
Y = c(3, 5, 7, 9, 11);
linreg(X,Y);