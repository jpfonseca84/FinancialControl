#Function to add manually the expenses
add.manual <-
      function(f.category,
               f.type,
               f.amount,
               f.place = NA,
               f.date = as.character(Sys.Date())){
              
            #Merging original data with input data
            tempframeA <- data
            tempframeB <- cbind(category=f.category, 
                                type=f.type, 
                                amount=f.amount, 
                                date=f.date, 
                                place=f.place)
            data <<- rbind(tempframeA, tempframeB)
            
            #Saving the new complete data set in the file location
            save(data,
                 file = "totalcostsdata.rda")
            
            #Return the new data set inputted
            return(tempframeB)
      }