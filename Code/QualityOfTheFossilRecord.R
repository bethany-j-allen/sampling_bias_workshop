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
## (1) plotting data

# plotting bivalve richness and ranged diversity through time
plot(palaeo$age, palaeo$richness,
		xlim=c(500,0), ylim=c(0,510), xlab="Millions of years", ylab="generic richness",
			type="o", lty=1, lwd=2, pch=1, col=1)
points(palaeo$age, palaeo$ranged, 
		type="o", lty=1, lwd=2, pch=2, col=2) 		
legend("topleft", c("sampled bivalve richness", "ranged-through bivalve richness"),
		lty=1, lwd=2, pch=1:2, col=1:2)

####################################
## (2) Calculating in R

# writing a function for SCM

SCM = function(a,b){
	SCM1 = 100*(a/(a+(b-a)))
	return(SCM1)
}

calculating SCM for bivalve data
SCMbiv<-SCM(palaeo$richness, palaeo$ranged)
# check results
SCMbiv

# plotting SCM and analysing gaps

# binding age and SCM together as a data set

SCMtime<-cbind(palaeo$age, SCMbiv)

# plotting

# format plot as before and export as a pdf

pdf("bivalve SCM.pdf", height=5, width=7)
	plot(SCMtime[,1], SCMbiv,
		xlim=c(500,0), ylim=c(0,100), xlab="Millions of years", ylab="SCM",
			type="o", lty=1, lwd=2, pch=1, col=1)
	legend("bottomright", c("bivalve SCM"),
		lty=1, lwd=2, pch=1, col=1)
dev.off()


####################################
## (3) plotting subsampled data

# plot and export subsampled data plot
pdf("subsampled bivalve richness.pdf", height=5, width=7)
	plot(palaeo$age, palaeo$subsampled,
		xlim=c(500,0), xlab="Millions of years", ylab="subsampled richness",
			type="o", lty=1, lwd=2, pch=1)
	legend("topleft", "bivalve richness at n=250 subsamples", lty=1, lwd=2, pch=1)
dev.off()

# comparing subsampled with raw richness
pdf("subsampled and raw bivalve richness.pdf", height=5, width=7)
	plot(palaeo$age, palaeo$subsampled,
		xlim=c(500,0), ylim=c(0,510), xlab="Millions of years", ylab="subsampled richness", 
			type="o", lty=1, lwd=2, pch=1, col=1)
	points(palaeo$age, palaeo$richness, 
		type="o", lty=1, lwd=2, pch=2, col=2)
	legend("topleft", c("bivalve richness at n=250 subsamples", "raw bivalve richness"),
		lty=1, lwd=2, pch=1:2, col=1:2)
dev.off()

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

####################################
## residual modelling
# using collections as proxy and richness as diversity

# sourcing code from the web

source("http://www.graemetlloyd.com/pubdata/functions_2.r") 

# installing and loading packages

install.packages("earth", dependencies=TRUE)
install.packages("nlme", dependencies=TRUE)
install.packages("paleoTS", dependencies=TRUE)
install.packages("plotrix", dependencies=TRUE)
install.packages("praise", dependencies=TRUE)

library("earth")
library("nlme")
library("paleoTS")
library("plotrix")
library("praise")

# renaming variable to fit the model parameters

time<-palaeo$age
div<-palaeo$richness
proxy<-palaeo$collections

# run the model code

results<-rockmodel.predictCI(proxy,div) 

# plot modelled diversity

pdf("raw and residual bivalve diversity.pdf", height=12, width=12)
par(mfrow=c(2,1))
plot(palaeo$age, palaeo$richness,
		xlim=c(500,0), ylim=c(0,510), xlab="Millions of years", ylab="generic richness",
			type="o", lty=1, lwd=2, pch=1, col=1)
	points(palaeo$age, palaeo$ranged, 
		type="o", lty=1, lwd=2, pch=2, col=2) 		
	legend("topleft", c("sampled bivalve richness", "ranged-through bivalve richness"),
		lty=1, lwd=2, pch=1:2, col=1:2)
plot(time,div-results$predicted,type="l",xlim=c(max(time),min(time)),
        xlab="Time (Ma)",ylab="Model Detrended Taxonomic Richness")
    polygon(x=c(time,rev(time)),y=c(div-results$predicted,
        rep(0,length(time))),col="grey",border=0)
    points(time,div-results$predicted,type="l")
    lines(time,rep(0,length(time)))
    lines(time,results$seupperCI-results$predicted,lty=2)
    lines(time,results$selowerCI-results$predicted,lty=2)
    lines(time,results$sdupperCI-results$predicted,lty=4)
    lines(time,results$sdlowerCI-results$predicted,lty=4)
dev.off()

