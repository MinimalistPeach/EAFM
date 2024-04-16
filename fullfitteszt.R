rolldice <- function(N,K,p)
{
  sum = 0;

  for(i in 1:length(K)) 
  {
    sum = sum + ((K[i] - N*p[i]) ^ 2) / (N*p[i]);
  }

  if(sum<11.1) 
  {
    print('elfogadjuk')
  } 
  else 
  {
    print('elutasítjuk')
  }
  print(sum)
}

N = 600;
K = c(83, 91, 122, 107, 74, 123);
p = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6);
rolldice(N,K,p);