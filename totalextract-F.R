#Sum of expenses per month and year
totalextract <-
      function(f.month = NULL,
               f.year = NULL,
               f.category = NULL,
               f.numofcat=NULL,
               f.opencat=F,
               f.day=1,
               f.fileaddress= "C:\\Users\\jpfon\\Google Drive\\Pessoal\\Documentos\\FinancialControl\\totalcostsfile"){
            
            #obtain the file Totalcostfiles
            tempframe<-read.csv(file=f.fileaddress)
            #turn negative in positive
            tempframe$value<-tempframe$value*-1
            
            #Transform the columns date to a Month and Year to 
            #work with previous dataset
            date.vector<-as.vector(read.csv(file=f.fileaddress)[,4])
            month.vector<-as.vector(as.numeric(substr(date.vector,6,7)))
            year.vector<-as.vector(as.numeric(substr(date.vector,1,4)))
            
            tempframe<-tempframe[,-4]
            tempframe<-data.frame(cbind(tempframe,
                                        month=month.vector,
                                        year=year.vector))
            
            ##Define the month and year -----------
            
            #---------------------------------
            #turn negative in positive
            #InputErrors
            if(is.null(category)&f.opencat==T){
                  stop("To open a category, please provide it in 
                       \"category\"")
            }
            
            #Provided MONTH AND YEAR -------------------------------------
      if (!is.null(month) & !is.null(f.year)) {
                  if (is.null(category)) {
                        #NO f.category
                        tempframe <-
                              tempframe[tempframe$month == f.month &
                                               tempframe$year == f.year, ]
                  }
                  else{
                        #WITH f.category
                        tempframe <-
                              tempframe[tempframe$month == f.month &
                                               tempframe$year == f.year &
                                               tempframe$category == f.category,]
                  }
            }
            
            #ONLY f.year is provided
            if (is.null(f.month) & !is.null(f.year)) {
                  if (is.null(f.category)) {
                        #NO f.category
                        tempframe <-
                              tempframe[tempframe$year == f.year, ]
                  } else{
                        #WITH f.category
                        tempframe <-
                              tempframe[tempframe$year == f.year &
                                               tempframe$category == f.category, ]
                  }
            }
            
            
            #only f.month provided
            if (is.null(f.year) & !is.null(f.month)) {
                  if (is.null(f.category)) {
                        #NO f.category
                        tempframe <-
                              tempframe[tempframe$month == f.month, ]
                  } else{
                        #WITH f.category
                        tempframe <-
                              tempframe[tempframe$month == f.month &
                                               tempframe$category == f.category, ]
                  }
            }
            ####Return of error ####
            if (nrow(tempframe) == 0) {
                  stop("No information in the selected period")
            }
            
            #adjusting levels of tempframe
            tempframe$category<-droplevels.data.frame(tempframe)$category
            #defining number of categories for f.numofcat NULL
            if(is.null(f.numofcat)){
                  f.numofcat<-length(levels(tempframe$category))
            }
            
            ####RETURN OF THE PLOT BASED IN f.opencat####
                  if (f.opencat==F) {
                  return(barplot(sort(
                        tapply(tempframe$value,
                               tempframe$category, sum),
                  decreasing=T)[1:f.numofcat]))
            } else{
                  return(barplot(sort(
                        tapply(tempframe$value[tempframe$category==f.category],
                               tempframe$type[tempframe$category==f.category],
                               sum),
                        decreasing=T)))
            }
      }
