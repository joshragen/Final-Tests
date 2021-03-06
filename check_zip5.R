#This is the actual fifth and final version of the code, where I have included all important markup,
#and made it more readable overall. This is the one that should be used.

#"file" is .csv data file to go through and clean
#"registry" is the registry to look for the correct results in (default is registry consisting of 
#all zip codes) this should generally not be changed, unless there is some other issue.
#"file_name" is what the .csv file that the data will be printed into is called
#"num_check" is the rows that should checked in 'file' (set to all rows by default)
#"city_check" is for when the city is spelled wrong, it checks for a string of this length among other city names
#with a matching zip code, and then replaces it with the first match. Set to 5 by default as anything lower
#would return too many false results into the city name.

#The confidence level is measure of how different the new data is from the original, by measuring how of the 3 columns
#are different. A score of 3 should indicate that there were no changes, and the data was inputted correctly.
#Changes to correct the zip code from 9-digit codes aren't counted in it.

#Overall, this code takes about 1-2 minutes to run through a spreadsheet with about 20,000 entries
#in it. The part that slowed it down the most was checking for if there were any matches(like !any(dzip[r] == reg_zip)),
#and I wasn't able to identify a way to speed those sections of code up.
#The two slowest sections were 1 and 3, because these did the most checking for matching data.

#source("check_zip_full.R")
#check_postal_codes("CSZ.csv")
#check_postal_codes("CSZ.csv", registry = "postal_codes.csv", file_name = "new_corrected_postal_codes.csv")
check_postal_codes <- function(file, registry = "full_postal_codes.csv", file_name = "corrected_postal_codes.csv", num_check = 1:nrow(dat), city_check = 5){
      
      dat <- read.csv(file, stringsAsFactors = FALSE)[,c("city", "state", "zip")]
      reg <- read.csv(registry, stringsAsFactors = FALSE, colClasses = "character")
      message("Starting... (0 of 5)")
      
      datC<-dat
      datC$state<-toupper(dat$state)
      datC$city<-toupper(dat$city)
      regC<-reg
      regC$city<-toupper(reg$city)
      dzip <- datC$zip
      reg_zip <- reg$zip
      #Pulls the important parts of the data out, and changes them all to be capitalized, correcting
      #for that error (capitalization is turned back before the end)
      
      fix_zip <- function(r){
            if(nchar(dzip[r]) == 10){
                  dzip[r] <- substr(dzip[r], 1,nchar(dzip[r])-5)
            }
            if(!any(dzip[r] == reg_zip)){
                  dzip[r] <- NA
            }
            dzip[r]
      }
      
      dzip_fix<-sapply(num_check, fix_zip, USE.NAMES = FALSE)
      datf<-replace(datC, 3,dzip_fix)
      message("Postal Codes Fixed (1 of 5)")
      #This fixes the postal codes, shortening down 9-digit codes, or removing those not in the database
      
      dcity <- datf$city
      rcity <- regC$city
      
      fix_city <- function(g){
            if(!any(dcity[g]==rcity) & !is.na(dzip_fix[g])){
                  check <- rcity[which(dzip_fix[g] == regC$zip)]
                  
                  for(k in seq_along(check)){
                        checking <- check[k]
                        for(i in 1:(nchar(checking)-city_check+1)){
                              pat<-substr(checking,i,i+city_check-1)
                              test <- grepl(pat, dcity[g])
                              if(test == TRUE){
                                    break
                              }
                        }
                        if(test == TRUE){
                              break
                        }
                  }
                  if(test == FALSE){
                        checking <- dcity[g]
                  }
                  checking
            }else{
                  dcity[g]
            }
      }
      
      dcity_fix <- sapply(num_check, fix_city, USE.NAMES = FALSE)
      datfc<-replace(datf, 1, dcity_fix)
      message("Cities Fixed (2 of 5)")
      #This code looks for close spellings, checking if some number of characters (5 by default)
      #matches up with the database, helping to correct for mild misspellings.
      
      check_zip <- function(n){
            
            reg_row <- reg[which(dzip_fix[n] == reg_zip),]
            reg_rowC <- reg_row
            reg_rowC$city <- toupper(reg_rowC$city)
            reg_rowC$state <- toupper(reg_rowC$state)
            
            if(nrow(reg_row) > 1){
                  if(any(datfc$city[n] == reg_rowC$city)){
                        reg_row <- reg_row[which(datfc$city[n] == reg_rowC$city),]
                  }else{
                        reg_row <- reg_row[1,]
                  }
            }else if(is.na(dzip_fix[n])){
                  reg_row <- matrix(ncol = 3)
            }
            reg_row
      }
      
      cordat_mat<-sapply(num_check, check_zip)
      message("Ordered According to Zip Code (3 of 5)")
      #This put together the corrected city, state and zip codes into a single row, then
      #puts them all together into a data.frame (The next two lines coerce it to the right form)
      cordat_list<-data.frame(t(as.data.frame(cordat_mat)))
      
      cordat<-data.frame(lapply(cordat_list,as.character), stringsAsFactors = FALSE)
      
      conf <- c()
      confidence <- function(p){
            lvl <- length(which(toupper(cordat[p,]) == datf[p,]))
            conf <- c(conf, lvl)
      }
      confidence_level<-sapply(num_check, confidence)
      cordat<-cbind(cordat,confidence_level)
      message("Confidence Level Added (4 of 5)")
      #This adds the confidence level (described at top)
      
      for(l in 1:nrow(dat)){
            if(cordat$confidence_level[l] == 1){
                  if(any((datfc$city[l] == regC$city) & (datfc$state[l] == regC$state))){
                        cor_reg <- reg[which((datfc$city[l] == regC$city) &
                                          (datfc$state[l] == regC$state)),]
                        if(any(cordat$zip[l] != cor_reg$zip)){
                              cordat$confidence_level[l] <- paste(cordat$confidence_level[l], 
                                    "; City and State don't match zip")
                        }
                  }
            }else if(((is.na(datf$zip[l])) & (datfc$city[l] != "") & (datf$state[l] != ""))){
                  if(!any((datfc$city[l] == regC$city) & (datf$state[l] == regC$state))){
                        cordat$confidence_level[l] <- paste(cordat$confidence_level[l], 
                              "; City and State combination not in registry, either in other country or typo present")
                  }
            }else if((datfc$city[l] == "") & (datf$state[l] == "")){
                  cordat[l,] <- c("","","",paste(cordat$confidence_level[l], "; No Data Present"))
            }
            if(cordat$confidence_level[l] == 0){
                  if(any((datfc$city[l] == regC$city) | (datf$state[l] == regC$state)) & (!any(dzip[l] == reg_zip))){
                        cordat$confidence_level[l] <- paste(cordat$confidence_level[l], "; Zip Code Doesn't Match Registry")
                  }
            }
      }
      message("Marked Incorrect Cities and States (5 of 5)")
      #This fixes various miscellaneous issuesthat could be found in the data, mostly when
      #the final corrected data set still doesn't match, and marks them as being incorrect.
      
      final_table <- cbind(Original_Values = "", dat, Corrected_Values ="",cordat)
      
      write.csv(final_table, file_name)
      message(paste("Done: printed data to", file_name))
}