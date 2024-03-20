DiceRoll <- function(N,K,p) {
  sum = 0;

  for(i in 1:length(k) {
    sum += ((K[i] - N*p[i]) ^ 2) / N*p[i];
  }

  print(sum);
}




N <- 600;
K <- c(83, 91, 122, 107, 74, 123);
p <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6);
DiceRoll(N, K, p);