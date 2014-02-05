#Problem Set 2
#Jae Hee Jung


# NOTE: This file contains code for benfords() that would fail to pass the unit test by miscalculating the m statistic. This code generates error for both dataset 1 and dataset 2 because the same code chunk in benfords() determines how the m statistic is calculated regardless of when dataset 1 or dataset 2 is put in. That is, if the function calculates the wrong m statistic for dataset 1, it calculates the wrong m statistic for dataset 2, and vice versa. So if we run the unit test on the following function, we will see errors for both dataset 1 and dataset 2. The part of benfords() that creates error is commented by "##".


#1
benfords <- function(votes.tot,statistic){
	#Argument "votes.tot" takes in a vector or matrix of observed vote totals
	#Argument "statistic" specifies the statistic you want to calculate as we will see below
	prop.freq <- numeric(9) #Creates a numeric vector of length nine
	for(i in 1:9){
		prop.freq[i] <- sum((substr(votes.tot,start=1,stop=1)==i)*1)/length(votes.tot)
	} #For loops the vector "prop.freq" to contain proportional frequencies of integers in vote totals
	if(statistic=="both"){ #When you want both m and d statistics
		m <- numeric(9)
		for(i in 1:9){
			m[i] <- abs(prop.freq[i]-log10(1+(1/i)))
		}
		d <- numeric(9)
		for(i in 1:9){
			d[i] <- (prop.freq[i]-log10(1+(1/i)))^2
		}
return(list(digit.distribution=prop.freq,m.statistic=sqrt((length(votes.tot))*max(m)),##The code for m.statistic is wrong, because there is a misplaced pair of brackets, altering the equation for calculating m.
d.statistic=sqrt(length(votes.tot))*sqrt(sum(d)))) #Returns both statistics as list containing the full digit distribution
	}else{
		if(statistic=="Leemis"){ #When you only want m statistic
			m <- numeric(9)
			for(i in 1:9){
			m[i] <- abs(prop.freq[i]-log10(1+(1/i)))
		}
return(list(digit.distribution=prop.freq,m.statistic=sqrt(length(votes.tot))*max(m))) #Returns a list of the m statistic and the full digit distribution
		}
		if(statistic=="Cho-Gains"){ #When you only want d statistic
			d <- numeric(9)
			for(i in 1:9){
			d[i] <- (prop.freq[i]-log10(1+(1/i)))^2
		}
return(list(digit.distribution=prop.freq,d.statistic=sqrt(length(votes.tot))*sqrt(sum(d)))) #Returns a list of the d statistic and the full digit distribution
	}
}
}


#3
unittest.benfords <- function(){
	data1 <- rep(seq(100,900,by=100),round(log10(1+(1/(1:9)))*100))
	#data1 is artifical data that meets Benford's law
	data1.distribution <- numeric(9)
	for(i in 1:9){ 
		data1.distribution[i] <- sum((substr(data1,start=1,stop=1)==i)*1)/length(data1)
	}
	#Calculates distribution of proportional frequencies of integers in data1
	data1.m <- numeric(9)
	for(i in 1:9){
		data1.m[i] <- abs(data1.distribution[i]-log10(1+(1/i)))
	}
	data1.m <- sqrt(length(data1))*max(data1.m)
	#data1.m is the m statistic calculated for data1
	data1.d <- numeric(9)
	for(i in 1:9){
		data1.d[i] <- (data1.distribution[i]-log10(1+(1/i)))^2
	}
	data1.d <- sqrt(length(data1))*sqrt(sum(data1.d))
	#data1.d is the d statistic calculated for data1
	benfords.data1 <- benfords(data1,"both")
	#benfords.data1 is the output of data1 using benfords(), which is the subject of unit testing
	data2 <- rep(seq(100,900,by=100),round(log10(1+(1/(9:1)))*100))
	#data2 is artifical data that doesn't meet Benford's law because it is log10(1+(1/(9:1))), not log10(1+(1/(1:9)))
	data2.distribution <- numeric(9)
	for(i in 1:9){ 
		data2.distribution[i] <- sum((substr(data2,start=1,stop=1)==i)*1)/length(data2)
	}
	#Calculates distribution of proportional frequencies of integers in data2
	data2.m <- numeric(9)
	for(i in 1:9){
		data2.m[i] <- abs(data2.distribution[i]-log10(1+(1/i)))
	}
	data2.m <- sqrt(length(data2))*max(data2.m)
	#data2.m is the m statistic calculated for data2
	data2.d <- numeric(9)
	for(i in 1:9){
		data2.d[i] <- (data2.distribution[i]-log10(1+(1/i)))^2
	}
	data2.d <- sqrt(length(data2))*sqrt(sum(data2.d))
	#data2.d is the d statistic calculated for data2
	benfords.data2 <- benfords(data2,"both")
	#benfords.data2 is the output of data2 using benfords(), which is the subject of unit testing
	matches <- c(benfords.data1[[1]]==data1.distribution, benfords.data1[[2]]==data1.m, benfords.data1[[3]]==data1.d, benfords.data2[[1]]==data2.distribution, benfords.data2[[2]]==data2.m, benfords.data2[[3]]==data2.d)
	#Creates a logical vector that shows whether or not the true calculations for each data are equal to the corresponding values in the outputs from benfords() 
	if(all(matches)==TRUE){
		cat("TRUE")
	}else{
		cat("FALSE\n\n")
		}
	#Returns TRUE if there is perfect match between the true calculations and the outputs of benfords(), and returns FALSE otherwise
	if(any(benfords.data1[[1]]!=data1.distribution)==TRUE){
		cat('Distribution for data1 wrong.\n')
	}
	if(any(benfords.data2[[1]]!=data2.distribution)==TRUE){
		cat('Distribution for data2 wrong.\n')
	}
	if((benfords.data1[[2]]!=data1.m)==TRUE){
		cat('m statistic miscalculated for data1.\n')
	}
	if((benfords.data2[[2]]!=data2.m)==TRUE){
		cat('m statistic miscalculated for data2.\n')
	}
	if((benfords.data1[[3]]!=data1.d)==TRUE){
		cat('d statistic miscalculated for data1.\n')
	}
	if((benfords.data2[[3]]!=data2.d)==TRUE){
		cat('d statistic miscalculated for data2.\n')
	}
	#If there is no perfect match, what went wrong is specified.
}

	
