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
  "praise", "tidyverse", "velociraptr", "viridis")
utils::install.packages(PackageBundle, dependencies = TRUE)
devtools::install_github("graemetlloyd/metatree")

# And load these into memory:
for(pkg in c(PackageBundle, "metatree")) try(library(pkg,
  character.only = TRUE), silent = TRUE)

# If you are familiar with R then you will know the hardest part in using a
# package or script is to get your data into R in the format required by the
# functions you want to use. Here we are going to take advantage of the
# Paleobiology Database's "API" (short for Application Programming Interface),
# which lets us "download" data directly into R. We are also going to use as
# an example data set Permo-Triassic bivalves.
#
# We can begin by setting up some variables:
Taxa <- "Bivalvia" # Set "Taxa" as the taxonomic group of interest
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

# It is worth pointing out that, again, things can go wrong this way if
# names are duplicated in the database. An example is the genus "Glyptolepis"
# which is both a plant (type of conifer)...:
utils::browseURL("https://paleobiodb.org/classic/basicTaxonInfo?taxon_no=291933")

# ...and a fish (lobe-fin):
utils::browseURL("https://paleobiodb.org/classic/basicTaxonInfo?taxon_no=34920")

# Thus if we ask the database for Glyptolepis the response from the database
# might not be what you expect. Let's skip ahead and ask for this data to
# see what happens:
utils::read.csv("https://paleobiodb.org/data1.2/occs/list.csv?base_name=Glyptolepis&show=coords,paleoloc,class&limit=all", header = T, na.strings = "")

# Instead of an error message (there are two Glyptotlepises!) the API just
# goes with one of them (the plant). Note this is not happening because
# there are no occurrences of the fish in the database. We can check that
# this is true by asking for the "right" Glyptolepis (at least if you are a
# fish person) with:
utils::read.csv("https://paleobiodb.org/data1.2/occs/list.csv?taxon_id=34920&show=coords,paleoloc,class&limit=all",
  header = T, na.strings = "")

# Thus if you *really* want to be sure you are getting the data you want you
# should use taxon_id= and the taxon number, and not base_name= and the taxon
# name. Again, with nrachiopods we are OK, but remember that the ICZN and
# ICBN are separate entities so there is nothing to stop someone naming a
# group of plants Bivalvia!
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

# One final tip is to make sure you only get "regular" taxa and not something
# from a parataxonomy (like egg or footprint "species") with the pres= option
# and the value "regular":
paste0("https://paleobiodb.org/data1.2/occs/list.csv?base_name=", Taxa,
  "&interval=", StartInterval, ",", StopInterval,
  "&show=coords,paleoloc,class&pres=regular")

# Now we have a complete URL we can store it in a variable...:
URL <- paste0("https://paleobiodb.org/data1.2/occs/list.csv?base_name=",
  Taxa, "&interval=", StartInterval, ",", StopInterval,
  "&show=coords,paleoloc,class&pres=regular")

# ...and then use the read.csv function to read the data into R:
RawData <- utils::read.csv(URL, header = TRUE, stringsAsFactors = FALSE)

# This is a lot of data!:
nrow(RawData)

# So best to just look at part of it to begin with:
head(RawData)

# Note that you can do similar queries in packages such as velociraptr or
# metatree, e.g.:
VRData <- velociraptr::downloadPBDB(Taxa = Taxa, StartInterval = StartInterval, StopInterval = StopInterval)
MTData <- metatree::PaleobiologyDBOccurrenceQuerier(unlist(lapply(apply(
  metatree::PaleobiologyDBChildFinder("1", Taxa, interval = c(StartInterval,
  StopInterval), returnrank = "3"), 1, list), function(x) {x <-
  unlist(x)[c("OriginalTaxonNo", "ResolvedTaxonNo")]; gsub("txn:|var:",
  "", unname(x[!is.na(x)][1]))})))

# But here we will stick with manual use of the API as it gives us more
# options/control of the output.
#
# Importantly, you should never take a raw data query like these and use it
# without some kind of scrutiny. But first we will simply trim away some of
# the database fields (columns) we don't really need:
RawData <- RawData[, c("occurrence_no", "collection_no", "phylum", "class",
  "order", "family", "genus", "accepted_name", "early_interval",
  "late_interval", "max_ma", "min_ma", "lng", "lat", "paleolng",
  "paleolat", "identified_rank")]

# We have already excluded egg and trace fossils ("form taxa" in the language
# of the PBDB), but we also have a bunch of fossils that are only assignable
# to some higher-level group:
unique(RawData[, "identified_rank"])

# There are clever ways some of these can be used to set some minimum level
# of species diversity where no lower-level taxa are known, but for now we
# will simply remove anything above genus-level (the level of analysis we
# will use here):
RawData <- dplyr::filter(RawData, nchar(genus) > 0)

# We can see this has shrunk the data, but not by much:
nrow(RawData)

# Anotehr important issue to consider is that synonymisation of taxa in the
# PBDB can lead to separate entries with the same name (as junior synonyms are
# replaced with their senior counterparts. If you want to know about richness
# this is an issue, as it artificially inflates your estimate.
#
#  We can stop this from happening by stripping out combinations of the same
# collection no. AND accepted name.
RawData <- dplyr::distinct(RawData, accepted_name, collection_no,
  .keep_all = TRUE)

# This has shrunk our data a little more:
nrow(RawData)

# Next we will consider occurrences not assigned to the stage(s) we want to
# use as out time bins, but first we need to explicitly state what these are:
StageNames <- c("Capitanian", "Wuchiapingian", "Changhsingian", "Induan",
  "Olenekian", "Anisian")

# Whilst we are at it we will also create a vector of stage midpoints we can
# use later for (e.g.) plots:
StageMidpoints <- c(263.1, 257, 253.2, 251.7, 249.2, 244.6)

# Now we can use this information to only retain occurrences assigned to
# *single* one of our named stages with:
RawData <- dplyr::filter(RawData, nchar(late_interval) == 0) %>%
  dplyr::filter(early_interval %in% StageNames)

# Note that we are doing this in a simple, automated fashion, which loses
# lots of occurrences unnecessarily, such as those dated to a single substage
# or regional biozone and only do so here for ease.

# Higher taxon loop
# Create an empty dataset with PBDB column names to collect unique occurrences in
unique_by_stage <- RawData[FALSE,]

#Loop through each collection
# For that collection, retain species occurrences, then retain unique taxa at gradually
# higher taxonomic levels which are not already represented in that collection
# i.e. if there is an indeterminate dicynodont but no occurrences in that collection which
# are dicynodonts but more specifically identified, the occurrence is retained
for (i in 1:(length(StageNames))) {
  print(i)
  one_stage <- RawData %>% filter(early_interval == StageNames[i])
  for (j in 1:(nrow(one_stage))) {
    if (one_stage$identified_rank[j] == "species") unique_by_stage <- rbind(unique_by_stage, one_stage[j,]) else
    if (!is.na(one_stage$genus[j]))
      (if (one_stage$genus[j] %in% one_stage$genus[-j] == F) unique_by_stage <- rbind(unique_by_stage, one_stage[j,])) else
        if (!is.na(one_stage$family[j]))
          (if (one_stage$family[j] %in% one_stage$family[-j] == F) unique_by_stage <- rbind(unique_by_stage, one_stage[j,])) else
            if (!is.na(one_stage$order[j]))
              (if (one_stage$order[j] %in% one_stage$order[-j] == F) unique_by_stage <- rbind(unique_by_stage, one_stage[j,]))
  }
}

#
# We can now extract clean data (the genus names for each fossil occurrence)
# as vectors for each stage and store them as a list:
CleanData <- lapply(as.list(StageNames), function(x)
  {unlist(lapply(strsplit(unique_by_stage[unique_by_stage[, "early_interval"] == x,
  "genus"], split = " "), function(y) y[1]))})

# And add the stage names for each one:
names(CleanData) <- StageNames

# We cna then access the names given to each occurrence in a stage with (for
# the Induan):
CleanData[["Induan"]]

# Note that this is a small list as these are the occurrences from the first
# stage after the Permo-Traissic extinction. Things are not looking good for
# our bivalves.
#
# We can do a simple (face value) diversity curve with:
plot(x = StageMidpoints, y = unlist(lapply(CleanData, function(x)
  length(unique(x)))), xlab = "Time (Ma)", ylab = "Richness (N genera)",
  type = "l", xlim = c(max(StageMidpoints), min(StageMidpoints)))

# The Permo-Triassic extinction appears pretty evident, as are the early
# stages of recovery. Next we can look at how much of this pattern may be
# down to sampling bias (see other scripts).


