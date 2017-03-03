#describe inside a category how much was spent per month

catbymonth<-function(f.category,
                     f.date.start=today(),
                     f.date.stop=today())
{
      
#define the dates -------------
      #start date
      f.date.start<-ymd(f.date.start)
      day(f.date.start) <- 1#Define the initial start date to search
      
      #stop date
      f.date.stop<-ymd(f.date.stop)
            day(f.date.stop) <- 1
            month(f.date.stop) <-month(f.date.stop) + 1
            day(f.date.stop) <-day(f.date.stop) - 1#point it to monthend
      
            
      #ORGANIZE THE FILE----------------
      
      #obtain the data file
      load("totalcostsdata.rda")
      tempframe <- data
      
      #turn negative values in positive
      tempframe$value <- abs(as.numeric(as.vector(tempframe$value)))
      
      #create the specific tempframe for the answer
      tempframe <- tempframe[as.vector(tempframe$date) >= f.date.start&as.vector(tempframe$date) <= f.date.stop,]
      
      #adjusting levels of tempframe
      tempframe$category <-droplevels.data.frame(tempframe)$category
      
      
      ###################REVIEW
      #The vector of values to be printed
      values.to.print <- tapply(as.vector(tempframe$value),
                                as.yearmon(sort(tempframe$date)),
                                sum)
      #Organizing columns to be from recent to oldest
      values.to.print<-values.to.print[length(values.to.print):1]
      
      #Ploting the information
      theplot <- barplot(values.to.print,
                         names.arg = names(values.to.print),
                         col=1:length(levels(tempframe$category)),
                         ylab="Dollars",
                         xlab="Category",
                         main = "total spent by category")
      legend("topright",
             legend = c(
                   paste("Start ", f.date.start),
                   paste("Stop ", f.date.stop)))
      
      return(data.frame(total=values.to.print))
      
}