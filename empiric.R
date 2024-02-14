atlag = function(x)
{
	s = 0;
	for(i in (1:length(x)))
	{
		s = s+x[i];
	}
	print(s/length(x));
}

szoras = function(x)
{
	s = 0;
	E=atlag(x);
	for(i in (1:length(x)))
	{
		s = s+(x[i]-E)^2;
		s = sqrt(s);
	}
	print(s/length(x));
}
szoras(x);