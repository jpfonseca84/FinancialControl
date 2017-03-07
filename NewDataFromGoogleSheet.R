

#grant access to the spreadsheet with the personal access
cloudupdate <- function() {
      gs_copy(gs_gap(), to = "Gapminder")
      
      #Register the spreadsheet by link
      g.data <-
            gs_url("https://docs.google.com/spreadsheets/d/1Wp-v9MltlcEpbmVxydKgiRGcYw8xHPZPkYsqI4oL_Rs/edit?usp=sharing")
      newdata <- gs_read(g.data)
      
      #clear the data -------------------
      newdata <-as.data.frame(newdata[, -(ncol(newdata))]) #remove extra columns
      newdata$date <- ymd(newdata$date)#convert the date to propper format
      newdata <- newdata[, c(1, 2, 12, 13, 14, 3:11)]
      #define the number of columns with "type" in name
      typecols <- sum(substr(names(newdata),
                             nchar(names(newdata)) - 4,
                             nchar(names(newdata))) == "types")
      
      #prepare a vector to contain all the types
      type <- NULL
      
      i <- ncol(newdata) - typecols + 1
      #compile types information in a single column
      while (i <= ncol(newdata)) {
            for (j in 1:nrow(newdata)) {
                  if (!is.na(newdata[j, i])) {
                        type[j] <- newdata[j, i]
                  }
                  j <- j + 1
            }
            i <- i + 1
      }
      
      #remove the specific type column to leave only the single one
      newdata <- newdata[, -which(endsWith(names(newdata), "types"))]
      newdata <- cbind(newdata, type)
      
      #-------------------Compile the new info in the base Data file
      data[] <- lapply(data, as.character)
      data <- rbind(data, newdata)
      save(data, file = "")
      
      #--------Clear the information in the google spreadsheet
      #define the number of columns that need to cleared
      
      return("information updated to the local database")
}
#---------OTHER FUNCTION---------------------------
#function to make a row blank

clear_g_row <-
      function(t.anchor = stop('please provide the anchor')) {
            g.data <-
                  gs_url("https://docs.google.com/spreadsheets/d/1Wp-v9MltlcEpbmVxydKgiRGcYw8xHPZPkYsqI4oL_Rs/edit?usp=sharing")
            gs_edit_cells(
                  ss = g.data,
                  anchor = t.anchor,
                  input = rep("", ncol(gs_read(g.data))),
                  byrow = T
            )
      }
#---------OTHER FUNCTION----------------------------
#function to clear the info in the google spreadsheet
clear_g_ws <- function() {
      g.data <-
            gs_url("https://docs.google.com/spreadsheets/d/1Wp-v9MltlcEpbmVxydKgiRGcYw8xHPZPkYsqI4oL_Rs/edit?usp=sharing")
      #define the number of rows that need to be cleared
      t.row <- nrow(gs_read(g.data))
      #define the number of columns in the file
      t.col <- ncol(gs_read(g.data))
      #stop function if t.row is = 0
      if (t.row == 0) {
            stop("spreadsheet already blank")
      }
      #list the rows that need to be deleted starting at A2
      rowtodelete<-paste("A",seq(2,t.row+1),sep = "")
      #bring clear_g_row function for each of rowtodelete
      clear_g_row(rowtodelete)
      return("completed the removal")
}