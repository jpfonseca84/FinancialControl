add.expense <-
      function(f.category,
               f.type,
               f.value,
               f.place = NA,
               f.date = as.character(Sys.Date()),
               f.fileaddress ="C:\\Users\\jpfon\\Google Drive\\Pessoal\\Documentos\\FinancialControl\\totalcostsfile"){

            #Merging original data with input data
            tempframeA <-read.csv(file = f.fileaddress)
            tempframeB <- cbind(category=f.category, 
                                type=f.type, 
                                value=f.value, 
                                date=f.date, 
                                place=f.place)
            tempframe <- rbind(tempframeA, tempframeB)
            
            #Saving the new complete data set in the file location
            write.csv(tempframe, 
                      file = f.fileaddress,
                      row.names = F)
            
            #Return the new data set inputted
            return(tempframeB)
      }