#opens a category to define what was spend inside it

opencat<-function(f.category,
                  f.date.start=Sys.Date(),
                  f.date.stop=Sys.Date(),
                  f.fileaddress = "C:\\Users\\jpfon\\Google Drive\\Pessoal\\Documentos\\FinancialControl\\totalcostsfile") {

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
      
      #obtain the .csv Totalcostfiles
      tempframe <- read.csv(file = f.fileaddress)
      
      #turn negative amounts in positive
      tempframe$amount <- abs(as.numeric(tempframe$amount))
      
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
      
      ####Return of error --------------------
      if (nrow(tempframe) == 0) {
            stop("No information in the selected period")
      }
      
      #------------------
      #return amounts
      #print the plot ------------------
      theplot <- barplot(amounts.to.print,
                         names.arg = names(amounts.to.print),
                         col=1:length(levels(tempframe$date)),
                         ylab="Dollars",
                         xlab="Month",
                         main = "total spent by month")
      
      #return(amounts.to.print)
      return(data.frame(amount=paste("$",amounts.to.print),
                        row.names = names(amounts.to.print)))


      
      
      }
            