################################################################################
#                                                                              #
#                SCRIPT I - SETUP AND GETTING PBDB DATA INTO R                 #
#                                                                              #
################################################################################

# SCRIPT AIMS:
#
# 1. Install and load the packages required for the workshop.
# 2. Introduce the PBDB and its' API.
# 3. Get some example data into R for use in the later scripts.

# First up we need to install the packages we will want to use (note this may
# take a while and you might be prompted to install additional packages):
PackageBundle <- c("devtools", "earth", "iNEXT", "nlme", "paleoTS", "plotrix",
  "praise", "tidyverse", "velociraptr")
install.packages(PackageBundle, dependencies = TRUE)
devtools::install_github("graemetlloyd/metatree")

# And load these into memory:
for(pkg in c(PackageBundle, "metatree")) try(library(pkg,
  character.only = TRUE), silent = TRUE)

# If you are familiar with R then you will know the hardest part in using a
# package or script is to get your data into R in the format required by the
# functions you want to use. Here we are going to take advantage of the
# Paleobiology Database's "API" (short for Application Programming Interface),
# which lets us "download" data directly into R. We are also going to use as
# an example data set Permo-Triassic brachiopods.
#
# We can begin by setting up some variables:
Taxa <- "Brachiopoda" # Set "Taxa" as the taxonomic group of interest
StartInterval <- "Capitanian" # Set start interval for sampling window
StopInterval <- "Anisian" # Set stop interval for sampling window

# In case you want to alter these to your own purposes then you should also
# run the following lines which will ensure things get fromatted properly
# for use with the API:
Taxa <- paste(Taxa, collapse = ",")
StartInterval <- gsub(" ", "%20", StartInterval)
StopInterval <- gsub(" ", "%20", StopInterval)

# We are now ready to use the API, but to do we have to produce a formatted
# URL (Uniform Resource Locator; i.e., a web address).
#
# These will always begin with:
"https://paleobiodb.org/data1.2"

# This is simply the top-level of the database with data1.2 indicating we
# are using version 1.2 (the latest version) of the API. Next we want the
# type of query, here we want some fossil occurrences (which is what most
# queries are going to be). Here we are going to ask for them as a CSV
# (comma-separated values):
"https://paleobiodb.org/data1.2/occs/list.csv"

# It is important to note that this means R will assume any comma it finds
# in the output represents a division between columns of data. This means
# if any of the data fields we want output contain a comma things are going
# to break and hence why other formats (e.g., JSON) are also available).
# Here we should be fine though.
#
# Next we need to tell the database what taxon we actually want data for, so
# we can use our Taxa variable from above with:
paste0("https://paleobiodb.org/data1.2/occs/list.csv?base_name=", Taxa)

# It is worth pointing out that, again, that things can go wrong this way if
# names are duplicated in the database. An example is the genus "Glyptolepis"
# which is both a plant (type of conifer)...:
browseURL("https://paleobiodb.org/classic/basicTaxonInfo?taxon_no=291933")

# ...and a fish (lobe-fin):
browseURL("https://paleobiodb.org/classic/basicTaxonInfo?taxon_no=34920")

# Thus if we ask the database for Glyptolepis the response from the database
# might not be what you expect. Let's skip ahead and ask for this data to
# see what happens:
utils::read.csv("https://paleobiodb.org/data1.2/occs/list.csv?base_name=Glyptolepis&show=coords,paleoloc,class&limit=all", header = T, na.strings ="")

# Instead of an error message (there are two Glyptotlepises!) the API just
# goes with one of them (the plant). Note this is not happening because
# there are no occurrences of the fish in the database. We can check that
# this is true by asking for the "right" Glyptolepis (at least if you are a
# fish person) with:
utils::read.csv("https://paleobiodb.org/data1.2/occs/list.csv?taxon_id=34920&show=coords,paleoloc,class&limit=all", header = T, na.strings ="")

# Thus if you *really* want to be sure you are getting the data you want you
# should use taxon_id= ad the taxon number, and not base_name= and the taxon
# name. Again, with nrachiopods we are OK, but remember that the ICZN and
# ICBN are separate entitie so there is nothing to stop someone naming a group
# of plants Brachiopoda!
#
# Now we have stated what taxon we want the next thing to do is add any
# additional options we want to add to our query. The obvious one here is the
# sampling window. We can do this with the interval= option and as this is an
# addition to the query we proceed it with an ampersand (&):
paste0("https://paleobiodb.org/data1.2/occs/list.csv?base_name=", Taxa,
  "&interval=", StartInterval, ",", StopInterval)

# Note that the start and end of the interval have to be separated by a comma.
#
# We can now add some additional options for what we want the output to include
# with show=. If you want multiple things, agaian, these must be seoarated by
# commas. Here we will ask for coordinate data (coords), palaeo-locality data
# (paleoloc) and taxonomic hierarchy data (class):
paste0("https://paleobiodb.org/data1.2/occs/list.csv?base_name=", Taxa,
  "&interval=", StartInterval, ",", StopInterval,
  "&show=coords,paleoloc,class")

# Now we have a complete URL we can store it in a variable...:
URL <- paste0("https://paleobiodb.org/data1.2/occs/list.csv?base_name=",
  Taxa, "&interval=", StartInterval, ",", StopInterval,
  "&show=coords,paleoloc,class")

# ...and then use the read.csv function to read the data into R:
RawData <- utils::read.csv(URL, header = TRUE)

# This is a lot of data!:
nrow(RawData)

# So best to just look at part of it:
head(RawData)






#  Use the PBDB API to download a dataset of tetrapods from the Wuchiapingian (early Late Permian) to
#  Carnian (early Late Triassic)
tetrapods <- utils::read.csv("https://paleobiodb.org/data1.2/occs/list.csv?base_name=Tetrapoda&interval=Wuchiapingian,Carnian&show=coords,paleoloc,class&limit=all", header = T, na.strings ="")
#  Trim to the columns with information we want
tetrapods <- tetrapods[, c("occurrence_no", "collection_no", "phylum", "class", "order",
"family", "genus", "accepted_name", "early_interval", "late_interval", "max_ma",
"min_ma", "lng", "lat", "paleolng", "paleolat")]

#  Create a vector giving the chronological order of stages
stages <- c("Wuchiapingian", "Changhsingian", "Induan", "Olenekian", "Anisian", "Ladinian",
"Carnian")

#  Create a vector of stage midpoints
midpoints <- c(257, 253.2, 251.7, 249.2, 244.6, 239.5, 232)

#  This code is set up to give you generic richness, but can be modified fairly easily to give other
#  taxonomic levels of diversity.

#  When taxa are made synonymous in the PBDB, they are retained as separate entries with the same
#  name. If you want to know diversity, this is an issue, as it articifially inflates your estimate.
#  We can stop this from happening by stripping out combinations of the same collection no. AND
#  accepted name.
tetrapods <- distinct(tetrapods, accepted_name, collection_no, .keep_all = T)

#  Retain only occurrences which are identified to genus or species level
tetrapods <- filter(tetrapods, !is.na(genus))

#  Retain occurences which are dated to a single stage
#  [Note: doing this in a simple, automated fashion loses lots of occurrences unnecessarily, such as
#  those dated to a single substage or regional biozone, but is done here for ease]
tetrapods <- filter(tetrapods, is.na(late_interval)) %>% filter(early_interval %in% stages)






# Some package version of the query:

VRData <- velociraptr::downloadPBDB(Taxa = Taxa, StartInterval = StartInterval, StopInterval = StopInterval)

MTData <- metatree::PaleobiologyDBOccurrenceQuerier(unlist(lapply(apply(metatree::PaleobiologyDBChildFinder("1", Taxa, interval = c(StartInterval, StopInterval), returnrank = "3"), 1, list), function(x) {x <- unlist(x)[c("OriginalTaxonNo", "ResolvedTaxonNo")]; gsub("txn:|var:", "", unname(x[!is.na(x)][1]))})))




