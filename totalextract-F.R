#Sum of expenses per month and year
totalextract <-
      function(f.date = NULL,
               f.numofcat=NULL,
               f.fileaddress= "C:\\Users\\jpfon\\Google Drive\\Pessoal\\Documentos\\FinancialControl\\totalcostsfile"){
            
#Define the oldest date to search
           
            #If f.date is blank
            if(is.null(f.date)){
            f.date<-today()      
            day(f.date)<-1
            }
            #if f.date is not blank
            ##Coherce f.date as a Date class Object
            if(!is.null(f.date)){
                  f.date<-as.Date(f.date,'%Y %m %d')
                  day(f.date)<-1
            }
            
#ORGANIZE THE FILE---------------- 
            #obtain the file Totalcostfiles
            tempframe<-read.csv(file=f.fileaddress)
            #turn negative in positive
            tempframe$value<-tempframe$value*-1
            #Transform the date column in a date vector
            tempframe$date<-as.Date(tempframe$date)
            
            #create the specific tempframe for the answer
            tempframe<-tempframe[tempframe$date>=as.Date(f.date),]
            #adjusting levels of tempframe
            tempframe$category<-droplevels.data.frame(tempframe)$category
            
            
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
                        decreasing=T))[1:numofcat])
           }
            
      }
