independency <- function(K){
  rowSum = rowSums(K)
  colSum = colSums(K)
  n = sum(rowSum)
  r = length(rowSum)
  s = length(colSum)
  sumR = 0;
  for (i in 1:r){
    sumS = 0;
    for (j in 1:s){
      o = (K[i,j] - (rowSum[i]*colSum[j])/n)^2
      sumS = sumS + (o/(rowSum[i]*colSum[j]))
    }
    sumR = sumR + sumS;
  }
  result = n*sumR;
  if (result < qchisq(p=.95, df=(r-1)*(s-1) )){
     print('Az erteket elfogadjuk!\n')
  } else {
     print('Az erteket nem fogadjuk el!\n')
  }
}

matrix <- matrix(c(42,28,3,17,89,21), nrow = 3, ncol = 2)
independency (matrix)
