################################################################################
#                                                                              #
#                SCRIPT I - SETUP AND GETTING PBDB DATA INTO R                 #
#                                                                              #
#                     Graeme T. Lloyd and Bethany J. Allen                     #
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

# If you are familiar with R then you will know the hardest part of using a
# package or script is to get your data into R in the format required by the
# functions you want to use. Here we are going to take advantage of the
# Paleobiology Database's "API" (short for Application Programming Interface),
# which lets us "download" data directly into R. We are going to use Permo-
# Triassic bivalves as an example data set.
#
#
# We can begin by setting up some variables:
Taxa <- "Bivalvia" # Set "Taxa" as the taxonomic group of interest
StartInterval <- "Capitanian" # Set start interval for sampling window
StopInterval <- "Anisian" # Set stop interval for sampling window

# In case you want to alter these for your own purposes then you should also
# run the following lines which will ensure things get formatted properly
# for use with the API:
Taxa <- paste(Taxa, collapse = ",")
StartInterval <- gsub(" ", "%20", StartInterval)
StopInterval <- gsub(" ", "%20", StopInterval)

# We are now ready to use the API, but to do that we have to produce a
# formatted URL (Uniform Resource Locator; i.e., a web address).
#
# These will always begin with:
"https://paleobiodb.org/data1.2"

# This is simply the top-level of the database with 'data1.2' indicating we
# are using version 1.2 (the latest version) of the API. Next we want the
# type of query, here we want some fossil occurrences (which is what most
# queries are going to be). Here we are going to ask for them as a CSV
# (comma-separated values):
"https://paleobiodb.org/data1.2/occs/list.csv"

# It is important to note that this means R will assume any comma it finds
# in the output represents a division between columns of data. This means
# if any of the data fields we want to output contain a comma things are going
# to break and hence why other formats (e.g., JSON) are also available.
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
# might not be what we expect. Let's skip ahead and ask for this data to
# see what happens:
utils::read.csv("https://paleobiodb.org/data1.2/occs/list.csv?base_name=Glyptolepis&show=coords,paleoloc,class&limit=all", header = T, na.strings = "")

# Instead of an error message (there are two Glyptolepises!) the API just
# goes with one of them (the plant). Note this is not happening because
# there are no occurrences of the fish in the database. We can check that
# this is true by asking for the "right" Glyptolepis (at least if you are a
# fish person) with:
utils::read.csv("https://paleobiodb.org/data1.2/occs/list.csv?taxon_id=34920&show=coords,paleoloc,class&limit=all",
  header = T, na.strings = "")

# Thus if you *really* want to be sure you are getting the data you want you
# should use taxon_id= and the taxon number, and not base_name= and the taxon
# name. Again, with bivalves we are OK, but remember that the ICZN and
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
# with show=. If you want multiple things, again, these must be separated by
# commas. Here we will ask for coordinate data (coords), palaeo-locality data
# (paleoloc), taxonomic hierarchy data (class) and stratigraphic information
# (strat):
paste0("https://paleobiodb.org/data1.2/occs/list.csv?base_name=", Taxa,
  "&interval=", StartInterval, ",", StopInterval,
  "&show=coords,paleoloc,class,strat")

# (Note: for a full list of all the options you should consult the API
# documentation at: https://paleobiodb.org/data1.2/.)
#
# One final tip is to make sure you only get "regular" taxa and not something
# from a parataxonomy (like egg or footprint "species") with the pres= option
# and the value "regular":
paste0("https://paleobiodb.org/data1.2/occs/list.csv?base_name=", Taxa,
  "&interval=", StartInterval, ",", StopInterval,
  "&show=coords,paleoloc,class&pres=regular")

# Now we have a complete URL we can store it in a variable...:
URL <- paste0("https://paleobiodb.org/data1.2/occs/list.csv?base_name=",
  Taxa, "&interval=", StartInterval, ",", StopInterval,
  "&show=coords,paleoloc,class,strat&pres=regular")

# ...and then use the read.csv function to read the data into R:
RawData <- utils::read.csv(URL, header = TRUE, stringsAsFactors = FALSE)

# This is a lot of data!:
nrow(RawData)

# So best to just look at part of it to begin with:
head(RawData)

# Note that you can do similar queries in packages such as velociraptr or
# metatree, e.g.:
VRData <- velociraptr::downloadPBDB(Taxa = Taxa,
  StartInterval = StartInterval, StopInterval = StopInterval)
MTData <- metatree::PaleobiologyDBOccurrenceQuerier(unlist(lapply(apply(
  metatree::PaleobiologyDBChildFinder("1", Taxa, interval = c(StartInterval,
  StopInterval), returnrank = "3"), 1, list), function(x) {x <-
  unlist(x)[c("OriginalTaxonNo", "ResolvedTaxonNo")]; gsub("txn:|var:",
  "", unname(x[!is.na(x)][1]))})))

# But here we will stick with manual use of the API as it gives us more
# options/control over the output.
#
# Importantly, you should never take a raw data query like these and use it
# without some kind of scrutiny. There could be all sorts of mistakes or
# things you don't intend in the dataset.
#
# First we will simply trim the database fields (columns) to the ones we 
# really want:
RawData <- RawData[, c("occurrence_no", "collection_no", "phylum", "class",
  "order", "family", "genus", "accepted_name", "early_interval",
  "late_interval", "max_ma", "min_ma", "lng", "lat", "paleolng",
  "paleolat", "identified_rank", "formation")]

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

# Another important issue to consider is that synonymisation of taxa in the
# PBDB can lead to separate entries with the same name (as junior synonyms are
# replaced with their senior counterparts). If you want to know about richness
# this is an issue, as it artificially inflates your estimate.
#
# We can stop this from happening by stripping out combinations of the same
# accepted name and collection number (a collection is somewhat analogous to a
# fossil locality; we will revisit this in greater detail in Script II).
RawData <- dplyr::distinct(RawData, accepted_name, collection_no,
  .keep_all = TRUE)

# This has shrunk our data a little more:
nrow(RawData)

# Next we will consider occurrences not assigned to the stage(s) we want to
# use as our time bins, but first we need to explicitly state what these are:
StageNames <- c("Capitanian", "Wuchiapingian", "Changhsingian", "Induan",
  "Olenekian", "Anisian")

# Whilst we are at it we will also create a vector of stage midpoints (in Mya,
# millions of years ago) we can use later for (e.g.) plots:
StageMidpoints <- c(263.1, 257, 253.2, 251.7, 249.2, 244.6)

# Now we can use this information to only retain occurrences assigned to a
# *single* one of our named stages with:
RawData <- dplyr::filter(RawData, nchar(late_interval) == 0) %>%
  dplyr::filter(early_interval %in% StageNames)

# Note that we are doing this in a simple, automated fashion, which loses
# lots of occurrences unnecessarily, such as those dated to a single substage
# or regional biozone, and only do so here for speed/ease.
#
# We can now extract clean data, in the form of the genus names for each fossil
# occurrence for each stage, as vectors and store them as a list:
CleanData <- lapply(as.list(StageNames), function(x)
  {unlist(lapply(strsplit(RawData[RawData[, "early_interval"] == x,
  "genus"], split = " "), function(y) y[1]))})

# And add the stage names for each one:
names(CleanData) <- StageNames

# We can then access the names given to each occurrence in a stage with (for
# the Induan):
CleanData[["Induan"]]

# Note that this is a small list as these are the occurrences from the first
# stage after the Permo-Traissic extinction. Things are not looking good for
# our bivalves.

# This type of data is known as sampled-in-bin, as it only reflects the fossils
# found in each stage, as opposed to ranged-through data, which fills in ghost
# ranges (gaps in a taxon's stratigraphic range between its first and last
# known occurrences).
#
# We can do a simple (face value) diversity curve with:
plot(x = StageMidpoints, y = unlist(lapply(CleanData, function(x)
  length(unique(x)))), xlab = "Time (Ma)", ylab = "Genus richness",
  type = "l", xlim = c(max(StageMidpoints), min(StageMidpoints)))

# The Permo-Triassic extinction appears pretty evident, as are the early
# stages of recovery. But is this just sampling bias? What if we also look
# at the total numbers of occurrences or formations alongside genus richness?
#
# We can do that with:
par(mfrow = c(3, 1))
plot(x = StageMidpoints, y = unlist(lapply(CleanData, function(x)
  length(unique(x)))), xlab = "Time (Ma)", ylab = "Genus richness",
  type = "l", xlim = c(max(StageMidpoints), min(StageMidpoints)),
  main = "Genus richness")
plot(x = StageMidpoints, y = unlist(lapply(as.list(StageNames),
  function(x) sum(RawData[, "early_interval"] == x))), xlab = "Time (Ma)",
  ylab = "N occurrences", type = "l", xlim = c(max(StageMidpoints),
  min(StageMidpoints)), main = "Number of occurrences")
plot(x = StageMidpoints, y = unlist(lapply(as.list(StageNames),
  function(x) {UniqueFormations <-
  unique(RawData[RawData[, "early_interval"] == x, "formation"]);
  length(UniqueFormations[nchar(UniqueFormations) > 0])})),
  xlab = "Time (Ma)", ylab = "N formations", type = "l",
  xlim = c(max(StageMidpoints), min(StageMidpoints)),
  main = "Number of formations")

# There seems to be enough co-variance in these values to give us some concern
# about sampling's role in explaining our data. In the next two scripts we will
# explore ways to attempt to remove sampling from the equation to see if the
# extinction and recovery patterns holds up.
