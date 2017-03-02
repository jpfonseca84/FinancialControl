#pulling info from a CSV to a rda

add.expense.csv <- function() {
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
