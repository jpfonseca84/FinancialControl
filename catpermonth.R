#describe inside a category how much was spent per month

catbymonth<-function(f.category,
                     f.date.start=today(),
                     f.date.stop=today(),
                     f.fileaddress = "C:\\Users\\jpfon\\Google Drive\\Pessoal\\Documentos\\FinancialControl\\totalcostsfile"){
      
#define the dates -------------
      #start date
      day(f.date.start) <- 1#Define the initial start date to search
      month(f.date.start)<-month(f.date.start)-12
      
      #stop date
            day(f.date.stop) <- 1
            month(f.date.stop) <-month(f.date.stop) + 1
            day(f.date.stop) <-day(f.date.stop) - 1#point it to monthend
      
      
      ################### ORGANIZE BELOW
      #ORGANIZE THE FILE----------------
      
      #obtain the .csv Totalcostfiles
      tempframe <- read.csv(file = f.fileaddress)
      
      #turn negative values in positive
      tempframe$value <- tempframe$value * -1
      
      #Transform the date column in a date vector
      tempframe$date <- as.Date(tempframe$date)
      
      #create the specific tempframe for the answer
      tempframe <- tempframe[tempframe$date >= f.date.start,]
      
      #adjusting levels of tempframe
      tempframe$category <-
            droplevels.data.frame(tempframe)$category
      
      #define the number of f.numofcat
      if (is.null(f.numofcat)) {
            f.numofcat <- length(levels(tempframe$category))
      }
      
      #The vector of values to be printed
      values.to.print <- sort(tapply(tempframe$value,
                                     tempframe$category,
                                     sum),
                              decreasing = T)[1:f.numofcat]
      
      ################## ORGANIZE ABOVE
      
      #values to plot
      tapply()
}