################################################################################
#                                                                              #
#                   SCRIPT II - RAREFACTION AND BOOTSTRAPPING                  #
#                                                                              #
################################################################################

# SCRIPT AIMS:
#
# 1. Install and load the packages required for the workshop.
# 2. Introduce the PBDB and its' API.
# 3. Get some example data into R for use in the later scripts.

# Load packages:
PackageBundle <- c("devtools", "earth", "iNEXT", "metatree", "nlme", "paleoTS",
  "plotrix", "praise", "tidyverse", "velociraptr")
for(pkg in c(PackageBundle, "metatree")) try(library(pkg,
  character.only = TRUE), silent = TRUE)

# Read the data into R:
RawData <- utils::read.csv("https://paleobiodb.org/data1.2/occs/list.csv?base_name=Brachiopoda&interval=Capitanian,Anisian&show=coords,paleoloc,class", header = TRUE, stringsAsFactors = FALSE)



unique(RawData[unique(RawData[, "accepted_rank"]) == "species", "accepted_name"])





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







