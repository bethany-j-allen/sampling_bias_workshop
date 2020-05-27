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
RawData <- utils::read.csv("https://paleobiodb.org/data1.2/occs/list.csv?base_name=Bivalvia&interval=Capitanian,Norian&show=coords,paleoloc,class",
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




collections <- unique(tetrapods_trim$collection_no)

#Create frames to store sampled collection numbers and the matching curve for each iteration
sample_list <- data.frame()
gradients <- data.frame()

#Indicate number of bootstraps and reps required -> 250 for terrestrial, 30 for marine, 100 reps of each
bootstraps <- 30
reps <- 100

for (x in 1:reps){
  
  print(x)
  
  #Create subsampled bootstrap of 100 occurrences (with replacement)
  colls_to_sample <- base::sample(collections, bootstraps, replace = T)
  subset <- tetrapods_trim[FALSE,]
  for (k in 1:bootstraps){
    one_coll <- filter(tetrapods_trim, collection_no == colls_to_sample[k])
    subset <- rbind(subset, one_coll)
  }
  
  #Filter unique taxa
  #Create an empty dataset with PBDB column names to collect unique occurrences in
  unique_by_bin <- tetrapods_trim[FALSE,]
  
  #Loop through each collection
  #For that collection, retain species occurrences, then retain unique taxa at gradually
  #   higher taxonomic levels which are not already represented in that collection
  #   i.e. if there is an indeterminate dicynodont but no occurrences in that collection which
  #   are dicynodonts but more specifically identified, the occurrence is retained
  for (i in 1:(length(labels))) {
    one_bin <- subset %>% filter(paleolat_code == labels[i])
    for (j in 1:(nrow(one_bin))) {
      if (nrow(one_bin) != 0)
        if (one_bin$accepted_rank[j] == "species") unique_by_bin <- rbind(unique_by_bin, one_bin[j,]) else
          if (!is.na(one_bin$genus[j]))
            (if (one_bin$genus[j] %in% one_bin$genus[-j] == F) unique_by_bin <- rbind(unique_by_bin, one_bin[j,])) else
              if (!is.na(one_bin$family[j]))
                (if (one_bin$family[j] %in% one_bin$family[-j] == F) unique_by_bin <- rbind(unique_by_bin, one_bin[j,])) else
                  if (!is.na(one_bin$order[j]))
                    (if (one_bin$order[j] %in% one_bin$order[-j] == F) unique_by_bin <- rbind(unique_by_bin, one_bin[j,]))
    }
  }
  
  #Remove repeats of species names, to get one occurrence per unique species per substage
  unique_by_bin <- distinct(unique_by_bin, accepted_name, paleolat_code, .keep_all = T)
  
  #Generate counts
  raw_curve <- count(unique_by_bin, paleolat_code)
  
  #Fill in gaps
  for(m in 1:length(labels)) {
    if((labels[m] %in% raw_curve$paleolat_code) == FALSE) raw_curve <- rbind(raw_curve, c(labels[m], 0))
  }
  
  #Reorder for ease of checking
  raw_curve <- arrange(raw_curve, desc(paleolat_code))
  raw_curve$paleolat_code <- as.numeric(as.character(raw_curve$paleolat_code))
  
  #Add collection sample and curve for each iteration to a list
  sample_list <- rbind(sample_list, colls_to_sample)
  gradients <- rbind(gradients, raw_curve$n)
}

names(sample_list) <- 1:bootstraps
names(gradients) <- sort(labels, decreasing = T) #reorder labels to match order in output

write.csv(sample_list, file = "data/Bootstrap sample list.csv") #write csv of sampled collections
write.csv(gradients, file = "data/Bootstrap gradients.csv") #write csv of gradients produced for each bootstrap

sample_list <- read_csv("data/Bootstrap samples LP terr.csv")
gradients <- read_csv("data/Bootstrap gradients LP terr.csv")
gradients <- gradients[,-1]

#Generate summary statistics
means <- colMeans(gradients)
stand_devs <- apply(gradients, 2, sd)
error <- qnorm(0.95)*(stand_devs/sqrt(reps))
lower <- means - error
upper <- means + error
#Create data frame containing summary statistics ready for plotting
to_plot <- cbind(labels = sort(labels, decreasing = T), means, lower, upper)
to_plot <- data.frame(to_plot)

#Designate colour, limegreen or blue
colour <- "limegreen"

#Plot mean raw richness across bootstraps, with 95% confidence intervals
ggplot(to_plot, aes(x = labels, y = means, ymin = lower, ymax = upper)) +
  geom_line(size = 1, col = colour) + geom_point(size = 2, col = colour) + geom_linerange(col = colour) +
  labs(x = "Palaeolatitude", y = "Raw species richness count") + coord_flip() +
  scale_x_continuous(limits = c(-90, 90)) + geom_vline(aes(xintercept = 0), colour = "black") +
  scale_colour_manual(values = "green") +
  theme_classic()

