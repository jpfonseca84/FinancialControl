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
      month(f.date.start)<-month(f.date.start)-12
      
      #stop date
      day(f.date.stop) <- 1
      month(f.date.stop) <-month(f.date.stop) + 1
      day(f.date.stop) <-day(f.date.stop) - 1#point it to monthend
      
      
      #ORGANIZE THE FILE----------------
      
      #obtain the .csv Totalcostfiles
      tempframe <- read.csv(file = f.fileaddress)
      
      #turn negative values in positive
      tempframe$value <- abs(as.numeric(tempframe$value))
      
      #Transform the date column in a date vector
      tempframe$date <- as.Date(tempframe$date)
      
      #create the specific tempframe for the answer
      tempframe <- tempframe[tempframe$date >= f.date.start&
                                   tempframe$category==f.category,]
      }
            