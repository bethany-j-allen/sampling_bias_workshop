########################################################
# Quality of the fossil record
# AMD, modified by BJA
# 23rd October 2019
# Phanerzoic record of bivalves
########################################################

# set working directory
setwd("#####")

# download datasets - we are looking at bivalve diversity through the Phanerozoic
#  (Ordovician - Pliocene)

# download diversity by time bin
bin_diversity <- utils::read.csv("https://paleobiodb.org/data1.2/occs/diversity.csv?base_name=Bivalvia&count=genera&interval=Ordovician,Pliocene", header = T)

# This type of download gives you abundance and richness for each stage in your sampled time interval
# Take a look at the data using
head(bin_diversity)

# You can also look at summary statistics using
summary(bin_diversity)

# Let's add columns with the stage durations and midpoints to help us plot the data later
bin_diversity$duration <- bin_diversity$max_ma - bin_diversity$min_ma
bin_diversity$midpoint <- bin_diversity$min_ma + (bin_diversity$duration/2)

# download genus ranges
genus_ranges <- utils::read.csv("https://paleobiodb.org/data1.2/occs/taxa.csv?base_name=Bivalvia&rank=genus&interval=Ordovician,Pliocene&show=app", header = T)

# This type of download gives you a list of genera, with their first and last appearances
# Take a look at the data using
head(genus_ranges)

# You can see summary statistics for this too, using
summary(genus_ranges)

# Save these files if you want to - it's good practice if you want to use PBDB data for research to
#  keep a copy of the original downloads
write.csv(bin_diversity, file = "Bivalves_through_time.csv")
write.csv(genus_ranges, file = "Bivalve_genus_ranges.csv")

# You can read these in at a later date using
bin_diversity <- read.csv("Bivalves_through_time.csv", header = T)
genus_ranges <- read.csv("Bivalve_genus_ranges.csv", header = T)

####################################
# Now that we have prepared the data, we can produce exploratory graphs to see what the fossil record
#  of bivalves in the PBDB looks like

## (1) Occurrences through time
# This is simply the number of bivalve occurrences per stage

plot(bin_diversity$midpoint, bin_diversity$n_occs,
		xlim=c(500,0), xlab="Millions of years", ylab="No. of occurrences",
			type="o", lty=1, lwd=2, pch=1, col=1)

## (2) Generic diversity through time
# This is the number of bivalve genera observed per stage
# This is a type of 'sampled-in-bin' metric - there is no correction for ghost ranges

plot(bin_diversity$midpoint, bin_diversity$sampled_in_bin,
     xlim=c(500,0), xlab="Millions of years", ylab="No. of genera",
     type="o", lty=1, lwd=2, pch=1, col=1)


points(palaeo$age, palaeo$ranged, 
		type="o", lty=1, lwd=2, pch=2, col=2) 		
legend("topleft", c("sampled bivalve richness", "ranged-through bivalve richness"),
		lty=1, lwd=2, pch=1:2, col=1:2)

## (x) Generic evenness
# This is the number of bivalve occurrences per genus
# A huge range, with some genera very common, and others very rare

hist(genus_ranges$n_occs, breaks = 20, xlab = "No. of occurrences", ylab = "No. of genera")



####################################
## (4) plotting sampling proxies

# plot collections and richness through time on a log scale

pdf("log richness and collections.pdf", height=5, width=7)
	plot(palaeo$age, log(palaeo$collections),
		xlim=c(500,0), ylim=c(0,max(log(palaeo$collections))), xlab="Millions of years", ylab="log frequency",
			type="o", lty=1, lwd=2, pch=1, col=1)
	points(palaeo$age, log(palaeo$richness), 
		type="o", lty=1, lwd=2, pch=2, col=2) 	
	legend("bottomright", c("log bivalve collections", "log bivalve richness"),
		lty=1, lwd=2, pch=1:2, col=1:2)
dev.off()

####################################
## (5) correlating variables

# xy plot to visualise data

plot(palaeo$collections, palaeo$richness)


# check normality
par(mfrow=c(1,2))
	hist(palaeo$collections)
	hist(palaeo$richness)
par(mfrow=c(1,1))

# try logging
par(mfrow=c(1,2))
	hist(log(palaeo$collections))
	hist(log(palaeo$richness))
par(mfrow=c(1,1))

# no - use a nonparametric spearman test

# perform appropriate tests
cor.test(palaeo$collections, palaeo$richness, method="spearman")



