#-------------------Basic costfile navigation

#define the path to the file in the system
fileaddress<-function(fileaddress ="C:\\Users\\jpfon\\Google Drive\\Pessoal\\Documentos\\FinancialControl\\totalcostsfile"){
tempcostaddress<<-as.character(fileaddress)
            }


#Function to read the types of a particular category
typesin<-function(f.category){
      tempcostfile<-read.csv(file=tempcostaddress)
      return(levels(droplevels(tempcostfile$type[tempcostfile$category==f.category])))
}

placesin<-function(f.category){
      tempcostfile<-read.csv(file=tempcostaddress)
      return(levels(droplevels(tempcostfile$place[tempcostfile$category==f.category])))
}

catin<-function(){ #to see all categories avaiable to be used
      tempcostfile<-read.csv(file=tempcostaddress)
      return(levels(tempcostfile$category))
}