#This was one of the earlier files that I made, where I just wrote some code that 
#would generally sort through data

sort_registry <- function(file){
      
      dat <- read.csv(file)
      
      #levels(dat$registry) makes a character vector with
      #all registry names listed out (here 10)
      
      sorted1 <- dat[order(dat$registry),]
      #this sorts it and leaves it in the data registry
      
      sorted <- (split(dat, dat$registry)) 
      #this sorts it all into a large list
      
      sapply(sorted, function(x) dim(x))
      #this will print the dimensions of each element in the list
      unique(dat$...)
      #checks how many different names there are
}

list_outliers <- function(file, test, z = 3){
      
      dat <- read.csv(file)
      
      #lq<-quantile(dat$test, na.rm = TRUE)[2]
      #uq<-quantile(dat$test, na.rm = TRUE)[4]
      #iqr <- IQR(dat$test)
      #this finds the IQR of the data
      #stand <- sd(dat$test, na.rm = TRUE)
      #mean <- mean(dat$test, na.rm = TRUE)
      
      #threshup <- mean + (stand * z)
      #threshlow <- mean - (stand * z)

      #for(i in 1:ncol(dat))
            
      out <- abs((dat[[test]])-mean(dat[[test]], na.rm = TRUE)) >
            z*sd(dat[[test]],na.rm = TRUE)
      #this finds the outliers outside of z standard deviations
      #and puts them into a logical vector
            
      outliers <- data.frame(row = which(out == TRUE),
            value = dat[[test]][which(out == TRUE)])
      #this goes through that logical vector and puts the row number
      #and values of the outliers into a dataframe
      
      print(mean(dat[[test]], na.rm = TRUE))

      print(outliers)
}


#Some basic plotting functions for histrograms and density curves
histo<-function(i){
      
      ts<-dat[[i]]
      hts<-hist(ts,breaks = 10)
      xfit<-seq(min(ts, na.rm = T), max(ts, na.rm = T), length = 40)
      yfit<-dnorm(xfit,mean=mean(ts,na.rm = T),sd=sd(ts,na.rm = T))
      yfit <- yfit*diff(hts$mids[1:2])*length(ts) 
      lines(xfit, yfit, col="blue", lwd=2)
      #code to make histogram with line of normal curve
}

dense<-function(i){
      ts<-dat[[i]]
      plot(d)
      polygon(d, col="red")
      #code to make Kernal density plot data instead of histogram
}

