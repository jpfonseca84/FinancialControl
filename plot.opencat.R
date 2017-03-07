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
      
<<<<<<< HEAD:opencat.R
      #turn negative amounts in positive
      tempframe$amount <- abs(as.numeric(tempframe$amount))
=======
      #turn negative values in positive
      tempframe$value <- as.numeric(tempframe$value)
>>>>>>> refs/remotes/origin/master:plot.opencat.R
      
      #Transform the date column in a date vector
      tempframe$date <- as.Date(tempframe$date)
      
      #create the specific tempframe for the answer
      tempframe <- tempframe[tempframe$date >= f.date.start&
                                   tempframe$date<=f.date.stop&
                                   tempframe$category==f.category,]
      
      
      #Prepare the output amounts
      amounts.to.print <- tapply(tempframe$amount,
                                ymd(tempframe$date),
                                sum)
      #sort values to print to be from newer to older
      values.to.print<-values.to.print[length(values.to.print):1]
      
      ####Return of error --------------------
      if (nrow(tempframe) == 0) {
            stop("No information in the selected period")
      }
      
      #------------------
      #return amounts
      #print the plot ------------------
<<<<<<< HEAD:opencat.R
      theplot <- barplot(amounts.to.print,
                         names.arg = names(amounts.to.print),
                         col=1:length(levels(tempframe$date)),
=======
      theplot <- barplot(values.to.print,
                         names.arg = as.yearmon(as.Date(rownames(values.to.print))),
                         col=1:length(values.to.print),
>>>>>>> refs/remotes/origin/master:plot.opencat.R
                         ylab="Dollars",
                         xlab="Month",
                         main = paste("Total spent by month in",
                                      as.character(f.category)))
      
      #return(amounts.to.print)
      return(data.frame(amount=paste("$",amounts.to.print),
                        row.names = names(amounts.to.print)))


      
      
      }
            