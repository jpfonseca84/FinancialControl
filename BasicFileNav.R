#Basic costfile navigation
fileaddress<-function(fileaddress ="C:\\Users\\jpfon\\Google Drive\\Pessoal\\Documentos\\FinancialControl\\totalcostsfile"){
tempcostfile<<-read.csv(fileaddress)
            }


#Function to read the types of a particular category
typesin<-function(f.category){
      return(levels(droplevels(tempcostfile$type[tempcostfile$category==f.category])))
}

