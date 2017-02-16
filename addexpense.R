add.expense <-
      function(category,
               type = NA,
               value,
               place = NA,
               date = as.character(Sys.Date()),
               fileaddress ="C:\\Users\\jpfon\\Google Drive\\Pessoal\\Documentos\\FinancialControl\\totalcostsfile"){

            #Merging original data with inputted data
            tempframeA <-read.csv(file = fileaddress)
            tempframeB <- cbind(category, type, value, date, place)
            tempframe <- rbind(tempframeA, tempframeB)
            
            #Saving the new complete data set in the file location
            write.csv(tempframe, 
                      file = fileaddress,
                      row.names = F)
            
            #Return the new data set inputted
            return(tempframeB)
      }
