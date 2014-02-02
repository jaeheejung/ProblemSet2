#Problem Set 2
#Jae Hee Jung


#1
violations <- function(votes.tot,statistic){
	prop.freq <- numeric(9) #Creates a numeric vector of length nine
	for(i in 1:9){ 
		prop.freq[i] <- sum((substr(votes.tot,start=1,stop=1)==i)*1)/9
	} #For loops the vector "prop.freq" to contain proportional frequencies of integers in vote totals
	if(statistic=="both"){ #When you want both m and d statistics
		m <- numeric(9)
		for(i in 1:9){
			m[i] <- prop.freq[i]-log10(1+(1/i))
		}
		d <- numeric(9)
		for(i in 1:9){
			d[i] <- (prop.freq[i]-log10(1+(1/i)))^2
		}
return(list(digit.distribution=prop.freq,m.statistic=max(m),d.statistic=sqrt(sum(d)))) #Returns both statistics as list containing the full digit distribution
	}else{
		if(statistic=="Leemis"){ #When you only want m statistic
			m <- numeric(9)
			for(i in 1:9){
			m[i] <- prop.freq[i]-log10(1+(1/i))
		}
return(list(digit.distribution=prop.freq,m.statistic=max(m))) #Returns a list of the m statistic and the full digit distribution
		}
		if(statistic=="Cho-Gains"){ #When you only want d statistic
			d <- numeric(9)
			for(i in 1:9){
			d[i] <- (prop.freq[i]-log10(1+(1/i)))^2
		}
return(list(digit.distribution=prop.freq,d.statistic=sqrt(sum(d)))) #Returns a list of the d statistic and the full digit distribution
	}
}
}

#2
signif.level.m <- function(mstat){ #Input the value of m statistic from the previous question to get the corresponding significance level
	if(mstat >= 0.851 & mstat < 0.967){
		0.10
	}else{
	if(mstat >= 0.967 & mstat < 1.212){
		0.05
	}else{
	if(mstat >= 1.212){
		0.01
	} else{
		NA
	}
	}
}
}

signif.level.d <- function(dstat){ #Input the value of d statistic from the previous question to get the corresponding significance level
	if(dstat >= 1.212 & dstat < 1.330){
		0.10
	}else{
	if(dstat >= 1.330 & dstat < 1.569){
		0.05
	}else{
	if(dstat >= 1.569){
		0.01
	} else{
		NA
	}
	}
}
}





