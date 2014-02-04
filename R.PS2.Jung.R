#Problem Set 2
#Jae Hee Jung


#1
violations <- function(votes.tot,statistic){
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
return(list(digit.distribution=prop.freq,m.statistic=sqrt(length(votes.tot))*max(m),d.statistic=sqrt(length(votes.tot))*sqrt(sum(d)))) #Returns both statistics as list containing the full digit distribution
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


#2
print.benfords <- function(votes.tot,statistic="both"){
	#By default, this function calculates both statistics
	stat.list <- violations(votes.tot,statistic)[-1]
	#Excludes the first element of the list obtained from violations() because we do not need the digit distribution anymore
	stat.table <- as.matrix(stat.list)
	#Transforms the list to a matrix, which is more like a table
	colnames(stat.table) <- "Value"
	#Creates the column name
	m <- c(stat.table[1,1]<0.851,stat.table[1,1]>=0.851&stat.table[1,1]<0.967,stat.table[1,1]>=0.967&stat.table[1,1]<1.212,stat.table[1,1]>=1.212)
	#This logical vector identifies which critical value, if any, stat.table[1,1], which is the value of the Leemis' m statistic, matches with.
	d <- c(stat.table[2,1]<1.212,stat.table[2,1]>=1.212&stat.table[2,1]<1.330,stat.table[2,1]>=1.330&stat.table[2,1]<1.569,stat.table[2,1]>=1.569)
	#This logical vector identifies which critical value, if any, stat.table[2,1], which is the value of the Cho-Gains' d statistic, matches with.
	stars <- c("","*","**","***")
	#Creates a vector of possible values of stars, which will be used with objects m and d as below
	Signif.level <- c(stars[m==TRUE],stars[d==TRUE])
	#Creates a vector of stars that correspond to the location where object m and d are TRUE; that is, where each statistic corresponds to the critical values
	stat.table <- cbind(stat.table,Signif.level)
	#Adds the new variable "Signif.level" to the existing matrix
	print(stat.table)
	#Prints the resulting matrix
	cat("\n")
	#Provides a line break
	cat("Significance level: 0.10*, 0.05**, 0.01***")
	#A line included to explain what the stars mean
}