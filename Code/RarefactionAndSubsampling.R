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
  "plotrix", "praise", "tidyverse", "velociraptr")
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






# Get Jurassic and Cretaceous dinosaur occurrences from the PBDB:
JurassicDinosaurs <- metatree::PaleobiologyDBOccurrenceQuerier(unname(unlist(lapply(apply(metatree::PaleobiologyDBChildFinder(taxon_names = "Dinosauria", interval = c("Jurassic", "Jurassic"), returnrank = "3")[, 1:2], 1, list), function(x) {x <- unlist(x); gsub("txn:|var:", "", x[!is.na(x)][1])}))))
CretaceousDinosaurs <- metatree::PaleobiologyDBOccurrenceQuerier(unname(unlist(lapply(apply(metatree::PaleobiologyDBChildFinder(taxon_names = "Dinosauria", interval = c("Cretaceous", "Cretaceous"), returnrank = "3")[, 1:2], 1, list), function(x) {x <- unlist(x); gsub("txn:|var:", "", x[!is.na(x)][1])}))))

# Remove n. gen. and n. sp. parts so names are correct:
JurassicDinosaurs[, "IdentifiedName"] <- gsub("n. gen. |n. sp. ", "", JurassicDinosaurs[, "IdentifiedName"])
CretaceousDinosaurs[, "IdentifiedName"] <- gsub("n. gen. |n. sp. ", "", CretaceousDinosaurs[, "IdentifiedName"])

NReps <- 100

ResampledJurassicDinosaurs <- do.call(rbind, lapply(as.list(1:NReps), function(x) {x <- sample(JurassicDinosaurs[, "IdentifiedName"]); unlist(lapply(as.list(1:length(x)), function(y) {length(unique(x[1:y]))}))}))
ResampledCretaceousDinosaurs <- do.call(rbind, lapply(as.list(1:NReps), function(x) {x <- sample(CretaceousDinosaurs[, "IdentifiedName"]); unlist(lapply(as.list(1:length(x)), function(y) {length(unique(x[1:y]))}))}))

plot(x = 1:nrow(CretaceousDinosaurs), y = apply(ResampledCretaceousDinosaurs, 2, mean), type = "l", col = "red", xlab = "N occurrences", ylab = "Sampled richness")
points(x = 1:nrow(JurassicDinosaurs), y = apply(ResampledJurassicDinosaurs, 2, mean), type = "l", col = "blue")
legend(list(x = nrow(CretaceousDinosaurs), y = 200), legend = c("Cretaceous", "Jurassic"), col = c("red", "blue"), lty = 1, lwd = 2, merge = TRUE, xjust = 1)







