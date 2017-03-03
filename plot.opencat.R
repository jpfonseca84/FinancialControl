#opens a category to define what was spend inside it

plot.opencat<-function(f.category,
                  f.date.start=Sys.Date(),
                  f.date.stop=Sys.Date()){

      ##Coherce f.dates as a Date class object ----
      f.date.start <- ymd(f.date.start)
      f.date.stop <- ymd(f.date.stop)
      
      ##Input Error -----
      if (f.date.stop < f.date.start) {
            stop("Stop date prior to Start date. Please correct")
      }
      #define the dates -------------
      
      #start date
      day(f.date.start) <- 1#Define the initial start date to search
      
      #stop date
      day(f.date.stop) <- 1
      month(f.date.stop) <-month(f.date.stop) + 1
      day(f.date.stop) <-day(f.date.stop) - 1#point it to monthend
      
      
      #ORGANIZE THE FILE----------------
      
      #obtain the file
      tempframe <- data
      
      #turn negative values in positive
      tempframe$value <- as.numeric(tempframe$value)
      
      #Transform the date column in a date vector
      tempframe$date <- as.Date(tempframe$date)
      
      #create the specific tempframe for the answer
      tempframe <- tempframe[tempframe$date >= f.date.start&
                                   tempframe$date<=f.date.stop&
                                   tempframe$category==f.category,]
      
      
      #Prepare the output values
      values.to.print <- tapply(tempframe$value,
                                ymd(tempframe$date),
                                sum)
      #sort values to print to be from newer to older
      values.to.print<-values.to.print[length(values.to.print):1]
      
      ####Return of error --------------------
      if (nrow(tempframe) == 0) {
            stop("No information in the selected period")
      }
      
      #------------------
      #return values
      #print the plot ------------------
      theplot <- barplot(values.to.print,
                         names.arg = as.yearmon(as.Date(rownames(values.to.print))),
                         col=1:length(values.to.print),
                         ylab="Dollars",
                         xlab="Month",
                         main = paste("Total spent by month in",
                                      as.character(f.category)))
      
      #return(values.to.print)
      return(data.frame(Value=paste("$",values.to.print),
                        row.names = names(values.to.print)))


      
      
      }
            