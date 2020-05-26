# TURN THIS INTO A SCRIPT JUST ON GETTING DATA INTO R AND FORMATTING IT CORRECTLY

#Extended download function from velociraptr

Taxa <- " " #write the taxon name you want to search
StartInterval <- " " #write the start time interval
StopInterval <- " " #write the end time interval

StartInterval <- gsub(" ", "%20", StartInterval)
StopInterval <- gsub(" ", "%20", StopInterval)
Taxa <- paste(Taxa, collapse = ",")
URL <- paste0("https://paleobiodb.org/data1.2/occs/list.csv?base_name=", 
              Taxa, "&interval=", StartInterval, ",", StopInterval, 
              "&show=coords,paleoloc,class&limit=all")
File <- utils::read.csv(URL, header = TRUE)