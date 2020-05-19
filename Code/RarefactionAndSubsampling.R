# HEADER!!!

# Grab the Benson dinosaur body mass data from GitHub:
DinoBodyMasses <- read.table("https://raw.githubusercontent.com/bethany-j-allen/sampling_bias_workshop/master/Data/DinoBodyMasses.txt?token=ABZPFHSQJZOGHAKWFKTP73K6ZUEPA", header = TRUE)

# Shuffle the order to randomly resample the data:
x <- sample(DinoBodyMasses[, "Mass_kg"])

set.seed(9)
Samples <- do.call(rbind, lapply(as.list(1:length(x)), function(y) {z <- x[1:y]; c(min(z), mean(z), max(z))}))
colnames(Samples) <- c("MinMass", "MeanMass", "MaxMass")

plot(x = 1:length(x), y = Samples[, "MeanMass"], log = "y", xlab = "N species sampled", ylab = "Mass (kg)", type = "l", ylim = c(0.001, 100000), lwd = 2)
points(x = 1:length(x), y = Samples[, "MinMass"], type = "l", col = "red", lwd = 2)
points(x = 1:length(x), y = Samples[, "MaxMass"], type = "l", col = "blue", lwd = 2)
legend(list(x = length(x), y = 10), legend = c("Max", "Mean", "Min"), col = c("blue", "black", "red"), lty = 1, lwd = 2, merge = TRUE, xjust = 1)







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


for(i in 1:1108) {}

x <- readLines(paste("http://fossilworks.org/bridge.pl?a=displayInterval&interval_no=", i, sep = ""))
x <- paste(x[grep("emltime1", x)], collapse = " ")
x <- strsplit(x, split = "\"")[[1]][grep("emltime1", strsplit(x, split = "\"")[[1]], invert = TRUE)]
x <- x[grep("[:a-z:]", x)]

