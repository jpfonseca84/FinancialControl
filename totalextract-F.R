#Sum of expenses per category in the period
totalextract <- function(f.date = today(),
                         f.numofcat = NULL,
                         f.fileaddress = "C:\\Users\\jpfon\\Google Drive\\Pessoal\\Documentos\\FinancialControl\\totalcostsfile") {
      
      ##Coherce f.date as a Date class object in case it's a text
      if(is.character(f.date)) {
            f.date <- ymd(f.date)
      }
      
      #Define the oldest date to search
      day(f.date) <- 1
      
      #ORGANIZE THE FILE----------------
      
      #obtain the file Totalcostfiles
      tempframe <- read.csv(file = f.fileaddress)
      
      #turn negative in positive
      tempframe$value <- tempframe$value * -1
      
      #Transform the date column in a date vector
      tempframe$date <- as.Date(tempframe$date)
      
      #create the specific tempframe for the answer
      tempframe <- tempframe[tempframe$date >= f.date, ]
      
      #adjusting levels of tempframe
      tempframe$category <-
            droplevels.data.frame(tempframe)$category
      
      #define the number of f.numofcat
      if (is.null(f.numofcat)) {
            f.numofcat <- length(levels(tempframe$category))
      }
      
      ####Return of error --------------------
      if (nrow(tempframe) == 0) {
            stop("No information in the selected period")
      }
      #Return Answer ------------------
      else{
            return(barplot(sort(
                  tapply(tempframe$value,
                         tempframe$category,
                         sum),
                  decreasing = T)[1:f.numofcat]))
      }
      
}
