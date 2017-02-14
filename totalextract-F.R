#Sum of expenses per month and year
totalextract <-
      function(monthname = NULL,
               year = NULL,
               category = NULL,
               numofcat=NULL,
               opencat=F){
            #InputErrors
            if(is.null(category)&opencat==T){
                  stop("To open a category, please provide it in \"category\"")
            }
            
            #obtain the file Totalcostfiles
            tempframe<-read.csv(file="C:\\Users\\jpfon\\Google Drive\\Pessoal\\Documentos\\Documentos JP & Erika\\Gastos Financeiros\\totalcostsfile")
            #turn negative in positive
            tempframe$value<-tempframe$value*-1
             
            #Provided MONTH AND YEAR -------------------------------------
            if (!is.null(monthname) &
                !is.null(year)) {
                  if (is.null(category)) {
                        #NO CATEGORY
                        tempframe <-
                              tempframe[tempframe$month == monthname &
                                               tempframe$year == year, ]
                  }
                  else{
                        #WITH CATEGORY
                        tempframe <-
                              tempframe[tempframe$month == monthname &
                                               tempframe$year == year &
                                               tempframe$category == category,]
                  }
            }
            
            #ONLY year is provided
            if (is.null(monthname) & !is.null(year)) {
                  if (is.null(category)) {
                        #NO CATEGORY
                        tempframe <-
                              tempframe[tempframe$year == year, ]
                  } else{
                        #WITH CATEGORY
                        tempframe <-
                              tempframe[tempframe$year == year &
                                               tempframe$category == category, ]
                  }
            }
            
            
            #only MONTH provided
            if (is.null(year) & !is.null(monthname)) {
                  if (is.null(category)) {
                        #NO CATEGORY
                        tempframe <-
                              tempframe[tempframe$month == monthname, ]
                  } else{
                        #WITH CATEGORY
                        tempframe <-
                              tempframe[tempframe$month == monthname &
                                               tempframe$category == category, ]
                  }
            }
            ####Return of error ####
            if (nrow(tempframe) == 0) {
                  stop("No information in the selected period")
            }
            
            #adjusting levels of tempframe
            tempframe$category<-droplevels.data.frame(tempframe)$category
            #defining number of categories for numofcat NULL
            if(is.null(numofcat)){
                  numofcat<-length(levels(tempframe$category))
            }
            
            ####RETURN OF THE PLOT BASED IN opencat####
                  if (opencat==F) {
                  return(barplot(sort(
                        tapply(tempframe$value,
                               tempframe$category, sum),
                  decreasing=T)[1:numofcat]))
            } else{
                  return(barplot(sort(
                        tapply(tempframe$value[tempframe$category==category],
                               tempframe$type[tempframe$category==category],
                               sum),
                        decreasing=T)))
            }
      }
