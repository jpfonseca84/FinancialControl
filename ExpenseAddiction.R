add.expense <- function(category, type, value, month, year) {
      #Merging original data with inputted data
      tempframeA <-read.csv(file = "C:\\Users\\jpfon\\Google Drive\\Pessoal\\Documentos\\Documentos JP & Erika\\Gastos Financeiros\\totalcostsfile")
      tempframeB <- cbind(category, type, value, month, year)
      tempframe<-rbind(tempframeA,tempframeB)
      
      #Saving the new complete data set in the file location
      write.csv(tempframe,file="C:\\Users\\jpfon\\Google Drive\\Pessoal\\Documentos\\Documentos JP & Erika\\Gastos Financeiros\\totalcostsfile",row.names = F)
      
      #Return the new data set inputted
      return(tempframeB)
}

      