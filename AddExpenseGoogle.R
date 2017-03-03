#google file adress
#https://docs.google.com/spreadsheets/d/1dvQb5sfYMFUAOSBAQb8LTUc1bc6EBr9prQhF88pzxEM/pub?gid=0&single=true&output=csv

#pulling info from the google file

#Need to review the wholecode below as it is a copy from the CSV variant
add.expense.G <- function() {
      #read the new and old costs file
      tempdata <- read.csv(file = "NewCostsData", header = T)
      tempdata <- cbind(
            category = tempdata$category,
            type = tempdata$type,
            value = tempdata$value,
            date = tempdata["date"],
            place = tempdata$place
      )
      load("totalcostsdata.rda")
      
      #perform Rbind
      data <- rbind(data, tempdata)
      #save rda file and clean and save new file
      save(data, file = "totalcostsdata.rda")
      tempdata2 <- tempdata[-1:-length(tempdata), ]
      write.csv(tempdata2, file = "NewCostsData",
                row.names = F)
      print("List of info added")
      return(tempdata)
}
