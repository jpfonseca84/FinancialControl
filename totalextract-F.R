#Sum of expenses per category in the period
totalextract <- function(f.date.start = today(), #As "%Y %m %d"
                         f.date.stop = today(),  #As "%Y %m %d"
                         f.numofcat = NULL,
                         f.fileaddress = "C:\\Users\\jpfon\\Google Drive\\Pessoal\\Documentos\\FinancialControl\\totalcostsfile") {
##Coherce f.dates as a Date class object in case it's a text
      if (is.character(f.date.start)) {
            f.date.start <- ymd(f.date.start)
      }
      if (is.character(f.date.stop)) {
            f.date.stop <- ymd(f.date.stop)
      }
##Input Error
      if (f.date.stop < f.date.start) {
            stop("Stop date prior to Start date. Please correct")
      }
      
#define the dates -------------
      day(f.date.start) <- 1#Define the initial start date to search
      
      if (month(f.date.stop) != 12) {
            #proceeding if not december
            month(f.date.stop) <- month(f.date.stop) + 1
            day(f.date.stop) <- 1 #define it as 1
            day(f.date.stop) <-
                  day(f.date.stop) - 1#point it to monthend
      }else{
            year(f.date.stop) <- year(f.date.stop)+1
            month(f.date.stop) <- 1
            day(f.date.stop) <- 1
            day(f.date.stop) <-day(f.date.stop) - 1#point it to monthend
      }
      
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
####Return of error --------------------
      if (nrow(tempframe) == 0) {
            stop("No information in the selected period")
      }
#Return Answer ------------------
      theplot <- barplot(values.to.print,
                         names.arg = names(values.to.print))
      legend("topright",
             legend = c(
                   paste("Start ", f.date.start),
                   paste("Stop ", f.date.stop)
             ))
      return(invisible(theplot))
}