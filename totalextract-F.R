#Sum of expenses per category in the period
totalextract <- function(f.date.start = today(), #As "%Y %m %d"
                         f.date.stop = today(),  #As "%Y %m %d"
                         f.numofcat = NULL,
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
      tempframe <- tempframe[tempframe$date >= f.date.start,]
      
      #adjusting levels of tempframe
      tempframe$category <-
            droplevels.data.frame(tempframe)$category
      
      #define the number of f.numofcat
      if (is.null(f.numofcat)) {
            f.numofcat <- length(levels(tempframe$category))
      }

#The vector of amounts to be printed ----
      amounts.to.print <- sort(tapply(tempframe$amount,
                                     tempframe$category,
                                     sum),
                              decreasing = T)[1:f.numofcat]
####Return of error --------------------
      if (nrow(tempframe) == 0) {
            stop("No information in the selected period")
      }
#print the plot ------------------
      theplot <- barplot(amounts.to.print,
                         names.arg = names(amounts.to.print),
                         col=1:length(levels(tempframe$category)),
                         ylab="Dollars",
                         xlab="Category",
                         main = "total spent by category")
      legend("topright",
             legend = c(
                   paste("Start ", f.date.start),
                   paste("Stop ", f.date.stop)))
      
      return(data.frame(total=amounts.to.print))
}
