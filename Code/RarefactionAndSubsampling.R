################################################################################
#                                                                              #
#                   SCRIPT II - RAREFACTION AND BOOTSTRAPPING                  #
#                                                                              #
################################################################################

# SCRIPT AIMS:
#
# 1. Introduce basics of resampling.
# 2. Learn hot to rarefy and bootstrap data in R.
# 3. Generate rarefied and bootstrapped richness estimates.

# Make sure the packages are loaded into memory...:
PackageBundle <- c("devtools", "earth", "iNEXT", "metatree", "nlme", "paleoTS",
  "plotrix", "praise", "tidyverse", "velociraptr", "viridis")
for(pkg in c(PackageBundle, "metatree")) try(library(pkg,
  character.only = TRUE), silent = TRUE)

# ...alongside the data the data into R:
RawData <- utils::read.csv("https://paleobiodb.org/data1.2/occs/list.csv?base_name=Brachiopoda&interval=Capitanian,Anisian&show=coords,paleoloc,class",
  header = TRUE, stringsAsFactors = FALSE)
RawData <- RawData[, c("occurrence_no", "collection_no", "phylum", "class",
  "order", "family", "genus", "accepted_name", "early_interval",
  "late_interval", "max_ma", "min_ma", "lng", "lat", "paleolng",
  "paleolat", "identified_rank")]
RawData <- dplyr::filter(RawData, nchar(genus) > 0)
RawData <- dplyr::distinct(RawData, accepted_name, collection_no,
  .keep_all = TRUE)
StageNames <- c("Capitanian", "Wuchiapingian", "Changhsingian", "Induan",
  "Olenekian", "Anisian")
StageMidpoints <- c(263.1, 257, 253.2, 251.7, 249.2, 244.6)
RawData <- dplyr::filter(RawData, nchar(late_interval) == 0) %>%
  dplyr::filter(early_interval %in% StageNames)
CleanData <- lapply(as.list(StageNames), function(x)
  {unlist(lapply(strsplit(RawData[RawData[, "early_interval"] == x,
  "genus"], split = " "), function(y) y[1]))})
names(CleanData) <- StageNames

# One way to deal with sampling bias is to artifically resample the data. This
# allows us to, for example, quantify and plot the relationship between sampling
# and the thing we are measuring (here genus richness) as well as set an equal
# sampling level to use across a range of samples (here six geologic stages
# spanning the Permian-Traissic extinction). A great many packages include
# functions to perform the two resamling methods we will use here (rarefaction
# and bootstrapping), but we will just write our own code as the process is
# simple enough and it should aid understanding and avoid "black boxes".
#
# Both resampling processes can be coded in R using a simple base R function
# called "sample". We can see what this does if just give it the numbers 1:10:
base::sample(1:10)

# You shoudl see it basically "shuffles" them. try running it a few times to
# confirm that the answer will vary:
base::sample(1:10)
base::sample(1:10)
base::sample(1:10)
base::sample(1:10)
base::sample(1:10)

# We can imagine each number represents a fossil and we are artifically
# creating an order in which they could have been discovered. However, here
# we want to know not the number of the fossil but the taxon to which it is
# assigned. Let's use our real data (brachiopods from the Induan stage)
# instead of numbers:
sample(CleanData[["Induan"]])

# Again, if we try it a few times we should see that it varies:
sample(CleanData[["Induan"]])
sample(CleanData[["Induan"]])
sample(CleanData[["Induan"]])
sample(CleanData[["Induan"]])
sample(CleanData[["Induan"]])

# Note that it might *appear* to change quite often as some names are
# duplicated, but remember that "underneath" it we could be sampling fossil 1
# of genus A followed by fossil two of genus A one time and fossil 2 followed
# by fossil 1 another time and we would just see genus A twice in a row both
# ways.
#
# Now, to really mimic sampling verus richness what we want to record across
# our resample (from left to right) are two things: 1) the sample size and 2)
# the number of unique genera sampled. Thus for the first value this is going
# to be one (fossil/occurrence) and one (genus):
ReSample <- sample(CleanData[["Induan"]])
ReSample[1]

# Whereas for the next value it will be two (fossils/occurrences), but
# possibly either one (if our genus from before is sampled again) or two if
# we now sample a different genus:
ReSample[1:2]

# It would be tedious to continue in this way manually, so let's write a
# little function to do this job for us (including counting the unique
# names in each size sample)...:
ReSampler <- function(NamesVector) {ShuffledSample <- sample(NamesVector);
  matrix(c(1:length(ShuffledSample),
  unlist(lapply(as.list(1:length(ShuffledSample)),
  function(x) length(unique(ShuffledSample[1:x]))))), nrow = 2, byrow = TRUE,
  dimnames = list(c("SampleSize", "NUnique"), c()))}

# ...and apply it to the Induan brachiopods:
ReSampler(CleanData[["Induan"]])

# Note that sample size will always increment up by one on the top row, but the
# bottom row can vary each time:
ReSampler(CleanData[["Induan"]])
ReSampler(CleanData[["Induan"]])
ReSampler(CleanData[["Induan"]])
ReSampler(CleanData[["Induan"]])
ReSampler(CleanData[["Induan"]])

# Thus we really want to repeat this process multiple times so we can get (for
# example) a mean expectation for number of genera based on a given number of
# fossil occurrences. Again, manually doing this would be tedious, so lets's
# write some code to do it...:
MultiReSampler <- function(NamesVector, NReplicates) apply(do.call(rbind,
  lapply(as.list(1:NReplicates),
  function(x) ReSampler(NamesVector)["NUnique", ])), 2, mean)

# ...and test it out 100 times:
MultiReSamples <- MultiReSampler(NamesVector = CleanData[["Induan"]],
  NReplicates = 100)
MultiReSamples

# These results are best understood graphically:
plot(x = 1:length(MultiReSamples), y = MultiReSamples, xlab = "N Occurrences",
  ylab = "Genus richness (N unique genera)", type = "l")

# What we have here is a rarefaction curve. It clearly shows that as we add
# samples (go from left to right) we also add divesrity (genus richness).
# Let's try this again, but with a different stage, the Anisian:
MultiReSamples <- MultiReSampler(NamesVector = CleanData[["Anisian"]],
  NReplicates = 100)
plot(x = 1:length(MultiReSamples), y = MultiReSamples, xlab = "N Occurrences",
  ylab = "Genus richness (N unique genera)", type = "l")

# You should se this is a much larger sample (x-axis extends to much higher
# values). But also that the shape of the curve is now much clearer and appears
# to be asymptoting (i.e. it gets less steep fom left to right). Now let's
# produce a plot where we use all our stages so we can see the various
# rarefaction curves simultaneously (this might take a while to run):
MultiReSamples <- lapply(as.list(StageNames), function(x)
  MultiReSampler(NamesVector = CleanData[[x]], NReplicates = 100))
names(MultiReSamples) <- StageNames
plot(x = 1:max(unlist(lapply(MultiReSamples, length))), y = seq(from = 1,
  to = max(unlist(lapply(MultiReSamples, max))),
  length.out = max(unlist(lapply(MultiReSamples, length)))), log = "xy",
  type = "n", xlab = "N Occurrences", ylab = "Genus richness")
for(i in StageNames) points(x = 1:length(MultiReSamples[[i]]),
  y = MultiReSamples[[i]], type = "l",
  col = viridis::viridis(length(StageNames))[StageNames == i], lwd = 2)
legend("topleft", legend = StageNames, col = viridis::viridis(length(StageNames)),
  lwd = 2, bty = "n", cex = 1)






