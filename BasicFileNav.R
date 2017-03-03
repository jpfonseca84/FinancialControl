#-------------------Basic costfile navigation

#Function to read the types of a particular category
typesincat<-function(f.category){
      return(levels(droplevels(data$type[data$category==f.category])))
}

placesincat<-function(f.category){
      return(levels(droplevels(data$place[data$category==f.category])))
}

all.cats<-function(){ #to see all categories avaiable to be used
      return(levels(data$category))
}

