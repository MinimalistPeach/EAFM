 emp=function(x)
 {
    s=0
    avg=0
    for (i in 1:length(x))
    {
        s=s+x[i]
    }
    avg=s/ length(x)
    return(avg)
 }

teszt=function(x,m,s)
{
    u=(emp(x)-m)/(s/sqrt(length(x)))
    if(abs(u)<1.96)
    {
        return('elfogadjuk')
    }
    else 
    {
        return('elutasítjuk')
    }
}
x=c(1,2,3,4,5)
teszt(x,3,5)