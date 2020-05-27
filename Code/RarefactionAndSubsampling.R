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

# ...and we have gotten the data into R:
RawData <- utils::read.csv("https://paleobiodb.org/data1.2/occs/list.csv?base_name=Bivalvia&interval=Capitanian,Anisian&show=coords,paleoloc,class",
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
# assigned. Let's use our real data (bivalves from the Induan stage)
# instead of numbers:
base::sample(CleanData[["Induan"]])

# Again, if we try it a few times we should see that it varies:
base::sample(CleanData[["Induan"]])
base::sample(CleanData[["Induan"]])
base::sample(CleanData[["Induan"]])
base::sample(CleanData[["Induan"]])
base::sample(CleanData[["Induan"]])

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
ReSample <- base::sample(CleanData[["Induan"]])
ReSample[1]

# Whereas for the next value it will be two (fossils/occurrences), but
# possibly either one (if our genus from before is sampled again) or two if
# we now sample a different genus:
ReSample[1:2]

# It would be tedious to continue in this way manually, so let's write a
# little function to do this job for us (including counting the unique
# names in each size sample)...:
ReSampler <- function(NamesVector) {ShuffledSample <-
  base::sample(NamesVector); matrix(c(1:length(ShuffledSample),
  unlist(lapply(as.list(1:length(ShuffledSample)),
  function(x) length(unique(ShuffledSample[1:x]))))), nrow = 2, byrow = TRUE,
  dimnames = list(c("SampleSize", "NUnique"), c()))}

# ...and apply it to the Induan bivalves:
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

# You should be able to see that the curves for each stage are at least
# slightly different, with the Induan and Olenekian falling below the other
# stages (indicating they are genuinely less genus rich). We can the use this
# data to pick an "equal" sampling level (N occurrences) across which diversity
# can be compared. Let's try 100 occurrences and plot our diversity curve next
# to our rarefaction curves to aid understanding of how things are working:
SamplingLevel <- 100
par(mfrow = c(1, 2))
plot(x = 1:max(unlist(lapply(MultiReSamples, length))), y = seq(from = 1,
  to = max(unlist(lapply(MultiReSamples, max))),
  length.out = max(unlist(lapply(MultiReSamples, length)))), log = "xy",
  type = "n", xlab = "N Occurrences", ylab = "Genus richness")
for(i in StageNames) points(x = 1:length(MultiReSamples[[i]]),
  y = MultiReSamples[[i]], type = "l",
  col = viridis::viridis(length(StageNames))[StageNames == i], lwd = 2)
legend("topleft", legend = StageNames, col = viridis::viridis(length(StageNames)),
  lwd = 2, bty = "n", cex = 0.7)
lines(x = c(SamplingLevel, SamplingLevel), y = c(1, 10000), lty = 2, lwd = 1)
plot(x = StageMidpoints, y = unlist(lapply(MultiReSamples,
  function(x) x[[SamplingLevel]])), xlab = "Time (Ma)",
  ylab = "Genus richness", type = "l", xlim = c(max(StageMidpoints),
  min(StageMidpoints)), ylim = c(1, max(unlist(MultiReSamples))), log = "y")
points(x = StageMidpoints, y = unlist(lapply(MultiReSamples,
  function(x) x[[SamplingLevel]])), pch = 20, cex = 2,
  col = viridis::viridis(length(StageNames)))

# Hopefully you should still see the big dip after the Permo-Triassic (note the
# log scale on the y-axis which might make this seem less severe than it really
# is).
#
# This gives us an idea of how rarefaction works and how it can be used to
# (notionally) set a level sampling playing field by only considering the
# number of genera expected from a specific number of fossil occurrences.
#
# Bootstrapping works in a very similar way to rarefaction and we will consider
# this next by first revisiting our sample function:
base::sample(1:10)

# There are options for this function that we have not explored, specifically
# the "replace" option. By default we have this set to FALSE:
base::sample(1:10, replace = FALSE)

# What happens if instead we set this to true:
base::sample(1:10, replace = TRUE)

# We can again run this a few times to see waht is happening:
base::sample(1:10, replace = TRUE)
base::sample(1:10, replace = TRUE)
base::sample(1:10, replace = TRUE)
base::sample(1:10, replace = TRUE)
base::sample(1:10, replace = TRUE)

# You should now see that when we sample the numbers 1 to 10 we sometimes see
# some numbers multiple times and other numbers not at all. This is the
# essential difference between rarefaction and bootstrapping. I.e., when we
# resample from our "pool" of fossils under rarefaction we "keep" each fossil
# we pick out of the pool, but with bootstrapping we "return" the fossil
# meaning it can be sampled again.
#
# Typically rarefaction and boostrapping are used for very different things,
# but ultimately they both represent ways to resample a data set and hence
# bootstrapping can be used here in exactly the same way as we have used
# rarefaction above, except by "flipping" this replace switch to TRUE. To see
# what, if any, effect this has let's rewrite our functions from before to
# allow this option (Type = either "rarefaction" or "bootstrap")...:
ReSampler <- function(NamesVector, Type) {
  if(Type == "rarefaction") Replace <- FALSE
  if(Type == "bootstrap") Replace <- TRUE
  ShuffledSample <- base::sample(NamesVector, replace = Replace)
  matrix(c(1:length(ShuffledSample), unlist(lapply(as.list(1:length(ShuffledSample)),
  function(x) length(unique(ShuffledSample[1:x]))))), nrow = 2, byrow = TRUE,
  dimnames = list(c("SampleSize", "NUnique"), c()))}
MultiReSampler <- function(NamesVector, NReplicates, Type)
  apply(do.call(rbind, lapply(as.list(1:NReplicates),
  function(x) ReSampler(NamesVector, Type = Type)["NUnique", ])), 2, mean)

# ...try bootstrapping our samples instead...:
MultiReSamples <- lapply(as.list(StageNames), function(x)
  MultiReSampler(NamesVector = CleanData[[x]], NReplicates = 100, Type = "bootstrap"))
names(MultiReSamples) <- StageNames

# ...and plotting the output as before:
SamplingLevel <- 100
par(mfrow = c(1, 2))
plot(x = 1:max(unlist(lapply(MultiReSamples, length))), y = seq(from = 1,
  to = max(unlist(lapply(MultiReSamples, max))),
  length.out = max(unlist(lapply(MultiReSamples, length)))), log = "xy",
  type = "n", xlab = "N Occurrences", ylab = "Genus richness")
for(i in StageNames) points(x = 1:length(MultiReSamples[[i]]),
  y = MultiReSamples[[i]], type = "l",
  col = viridis::viridis(length(StageNames))[StageNames == i], lwd = 2)
legend("topleft", legend = StageNames, col = viridis::viridis(length(StageNames)),
  lwd = 2, bty = "n", cex = 0.7)
lines(x = c(SamplingLevel, SamplingLevel), y = c(1, 10000), lty = 2, lwd = 1)
plot(x = StageMidpoints, y = unlist(lapply(MultiReSamples,
  function(x) x[[SamplingLevel]])), xlab = "Time (Ma)",
  ylab = "Genus richness", type = "l", xlim = c(max(StageMidpoints),
  min(StageMidpoints)), ylim = c(1, max(unlist(MultiReSamples))), log = "y")
points(x = StageMidpoints, y = unlist(lapply(MultiReSamples,
  function(x) x[[SamplingLevel]])), pch = 20, cex = 2,
  col = viridis::viridis(length(StageNames)))

# You should see that mostly it looks pretty similar, which shouldn't surprise
# us too much. The main difference with bootstrapping is it will generate some
# uncertainty around the final value for a given sample. In other words, if in
# our starting sample we had 100 fossil occurrences and 25 sampled genera then
# resampling with rarefaction after 100 fossils are sampled we will always end
# up with 25 genera. However, with bootstrapping, because some genera may not
# be part of the resample, after 100 fossils are sampled we could have less
# than 25 genera. Generally, speaking this could be seen as a more desirable
# property as it better reflects the types of uncertanity inherent to fossil
# data. However, in terms of subsampling richness data we are better off using
# the more sophisticated coverage metrics such as SQS and squares.
#
# We can, however, use bootstrapping in a more sophsticated way by resampling
# not indiviudal fossil occurrences but individual fossil collections, and
# then counting the genera in the collections you have sampled.
#
# A "collection" in the Paleobiology Database is not well defined, but in
# theory represents a set of fossils collected from a single temporospatial
# locality. (In practice the way these are entered can vary *a lot*!) Here is
# an example from within our data set:
utils::browseURL("https://paleobiodb.org/classic/displayCollResults?a=basicCollectionSearch&collection_no=197722")

# We can see just how many collections are in our data set with:
unique(RawData[, "collection_no"])

# And format them into a list, recording both the stage they are assigned
# to and the unique genera present:
CollectionsList <- lapply(as.list(unique(RawData[, "collection_no"])),
  function(x) {CollectionRows <- which(RawData[, "collection_no"] == x);
  list(CollectionStage = RawData[CollectionRows[1], "early_interval"],
  CollectionGenera = unique(unlist(lapply(strsplit(RawData[CollectionRows,
  "genus"], split = " "), function(y) y[1]))))})

# Each value in the list thus looks something like this:
CollectionsList[[1]]

# So what we want to do now is (for each geologic stage) randomly sample
# from the available collections of that age and record the number of
# unique genera we find.
#
# We can find all the Induan collections with...:
which(unlist(lapply(CollectionsList, function(x)
  x$CollectionStage == "Induan")))

# ...and hence we can bootstrap this list with...:
base::sample(which(unlist(lapply(CollectionsList, function(x)
  x$CollectionStage == "Induan"))), replace = TRUE)

# ..and grab just the genus names with...:
unlist(lapply(CollectionsList[base::sample(which(unlist(lapply(CollectionsList,
  function(x) x$CollectionStage == "Induan"))), replace = TRUE)], function(y)
  y$CollectionGenera))

# ..and finally the unique names with:
unique(unlist(lapply(CollectionsList[base::sample(which(unlist(lapply(
  CollectionsList, function(x) x$CollectionStage == "Induan"))),
  replace = TRUE)], function(y) y$CollectionGenera)))

# However, if we are going to repat this for all stages we also need to set a
# sampling level (number of collections). One of the useful things about
# bootstrapping is that we are not constrained by the size of the sample, if
# we are happy to sample collections multiple times then we can sample more
# than the total size of the sample. E.g., with our simple numerical example
# we can sample the numbers 1 to 10 20 times with the size= option:
base::sample(1:10, replace = TRUE, size = 20)

# However, we don't want to oversample as this can create bias in our results.
# As our Induan stage is the least well sampled we can use the number of
# collections here as a guide to what is appropriate:
length(which(unlist(lapply(CollectionsList, function(x)
  x$CollectionStage == "Induan"))))

# Fifty thus seems reasonable:
SamplingLevel <- 50

# To set this for our Induan stage we can add the size= option with:
unique(unlist(lapply(CollectionsList[base::sample(which(unlist(lapply(
  CollectionsList, function(x) x$CollectionStage == "Induan"))),
  replace = TRUE, size = SamplingLevel)], function(y) y$CollectionGenera)))

# Now we just need some code to repeat this for each stage...:
lapply(as.list(StageNames), function(z) unique(unlist(lapply(CollectionsList[
  base::sample(which(unlist(lapply(CollectionsList, function(x)
  x$CollectionStage == z))), replace = TRUE, size = SamplingLevel)],
  function(y) y$CollectionGenera))))

# ...and again for multiple replicates to get a mean:
NReplicates <- 100
BootstrappedCollections <- unlist(lapply(as.list(StageNames), function(z)
  mean(unlist(lapply(as.list(1:NReplicates), function(q)
  length(unique(unlist(lapply(CollectionsList[base::sample(
  which(unlist(lapply(CollectionsList, function(x) x$CollectionStage == z))),
  replace = TRUE, size = SamplingLevel)], function(y)
  y$CollectionGenera)))))))))

# We can plot this as a time series (as we have before) with:
plot(x = StageMidpoints, y = BootstrappedCollections, xlab = "Time (Ma)",
  ylab = "Genus richness", type = "l", xlim = c(max(StageMidpoints),
  min(StageMidpoints)), ylim = c(1, max(unlist(MultiReSamples))), log = "y")
points(x = StageMidpoints, y = BootstrappedCollections, pch = 20, cex = 2,
  col = viridis::viridis(length(StageNames)))

# To finish up we can compare some of these results side-by-side:
par(mfrow = c(3, 1))
plot(x = StageMidpoints, y = unlist(lapply(CleanData, function(x)
  length(unique(x)))), xlab = "Time (Ma)", ylab = "Richness (N genera)",
  type = "l", xlim = c(max(StageMidpoints), min(StageMidpoints)),
  ylim = c(1, max(unlist(lapply(CleanData,
  function(x) length(unique(x)))))), log = "y", main = "Raw sampled-in-bin")
points(x = StageMidpoints, y = unlist(lapply(CleanData, function(x)
  length(unique(x)))), pch = 20, cex = 2,
  col = viridis::viridis(length(StageNames)))
plot(x = StageMidpoints, y = unlist(lapply(MultiReSamples,
  function(x) x[[SamplingLevel]])), xlab = "Time (Ma)",
  ylab = "Genus richness", type = "l", xlim = c(max(StageMidpoints),
  min(StageMidpoints)), ylim = c(1, max(unlist(MultiReSamples))), log = "y",
  main = "Rarefied sampled-in-bin")
points(x = StageMidpoints, y = unlist(lapply(MultiReSamples,
  function(x) x[[SamplingLevel]])), pch = 20, cex = 2,
  col = viridis::viridis(length(StageNames)))
plot(x = StageMidpoints, y = BootstrappedCollections, xlab = "Time (Ma)",
  ylab = "Genus richness", type = "l", xlim = c(max(StageMidpoints),
  min(StageMidpoints)), ylim = c(1, max(unlist(MultiReSamples))), log = "y",
  main = "Collection bootstrapped sampled-in-bin")
points(x = StageMidpoints, y = BootstrappedCollections, pch = 20, cex = 2,
  col = viridis::viridis(length(StageNames)))





# ADD BELOW TO THE SETUP SCRIPT:

"https://paleobiodb.org/data1.2/"





