################################################################################
#                                                                              #
#                SCRIPT I - SETUP AND GETTING PBDB DATA INTO R                 #
#                                                                              #
################################################################################

# SCRIPT AIMS:
#
# 1. Install and load the pcakages required for the workshop.
# 2. Introduce the PBDB and its' API.
# 3. Get some example data into R for use in the later scripts.

# First up we need to install the packages we will want to use today:
PackageBundle <- c("devtools", "earth", "nlme", "paleoTS", "plotrix",
"praise", "velociraptr")
install.packages(PackageBundle, dependencies = TRUE)
devtools::install_github("graemetlloyd/metatree")

# And load these into memory:
for(pkg in c(PackageBundle, "metatree")) try(library(pkg,
character.only = TRUE), silent = TRUE)



# If you are familiar with R then you will know the hardest part in using a
# package or script is to get your data into R in the format required by the
# functions you want to use.




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












# Glytpolepis the conifer:
browseURL("https://paleobiodb.org/classic/basicTaxonInfo?taxon_no=291933")

# Glyptolepis - the lobe-finned fish:
browseURL("https://paleobiodb.org/classic/basicTaxonInfo?taxon_no=34920")

# So if we ask the API for occurrences of Glyptolepis what will we get?:
utils::read.csv("https://paleobiodb.org/data1.2/occs/list.csv?base_name=Glyptolepis&interval=Cambrian,Cretaceous&show=coords,paleoloc,class&limit=all", header = T, na.strings ="")

# Oh dear...

utils::read.csv("https://paleobiodb.org/data1.2/occs/list.csv?taxon_id=34920&interval=Cambrian,Cretaceous&show=coords,paleoloc,class&limit=all", header = T, na.strings ="")

# Oh dear oh dear...


