################################################################################
#                                                                              #
#                        SCRIPT III - SQS AND SQUARES                          #
#                                                                              #
#                               Bethany J. Allen                               #
#                                                                              #
################################################################################

# SCRIPT AIMS:
#
# 1. Learn about the basics of coverage-based methods for estimating diversity.
# 2. Implement Shareholder Quorum Subsampling (SQS) and the Squares
#    extrapolator in R.

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

# In Script II we were introduced to rarefaction and bootstrapping, which are
# fairly simplistic ways of estimating diversity in deep time. Now we introduce
# coverage-based methods, which use strings of rank-order abundance to try and
# estimate sampling completeness, and this informs the degree of extrapolation
# or subsampling carried out for each sample.

# Here we will use the same dataset as before - bivalves from the Permian and
# Triassic - to implement Shareholder Quorum Subsampling (Alroy, 2010) using
# iNEXT (Hsieh et al. 2016), and the Squares extrapolator (Alroy, 2018).

####################################
## (1) Shareholder Quorum Subsampling (SQS)
# SQS (Alroy, 2010) uses rank-order abundance to estimate diversity through
# subsampling at different degrees of sampling completeness, or 'quorum
# levels', and estimates diversity using a metric called Good's u. Here we will
# use iNEXT, which estimates diversity using Hill numbers via both subsampling
# using the equations of Chao and Jost (2012). This subsampling method has been
# shown to be analogous to SQS (Close et al. 2018). iNEXT also implements
# extrapolation, using the Chao1 estimator (Hsieh et al. 2016).

# First we generate a table of occurrences by stage:
totals <- dplyr::count(RawData, early_interval)

# Then we reorder the substages in the table chronologically:
totals <- totals[base::match(StageNames, totals$early_interval),]

# This will be used later to evaluate whether our diversity estimates are
# overextrapolated. Hsieh et al. says that an estimate of more than 2-3 times
# your sample size should be considered unreliable.

# iNEXT requires input data in a very specific format. Each 'bin' that you want
# to sample needs a string of the abundance of each taxon, with the first number
# in the string being the total number of occurrences. So, if you had a sample
# containing 1 Aenigmasaurus, 2 Dicynodon, 5 Lystrosaurus, 1 Moschorhinus, and
# 1 Prolacerta, the string would look like this:
#
# Karoo Sample   10 5 2 1 1 1
#
# Given the nature of our input data, we need to carefully consider what our
# 'abundance' actually is. Locality-based abundance data is inconsistently applied
# in the PBDB, so for most analyses, the abundance of a taxon is actually a count
# of the number of collections (~ number of localities) in which it is present,
# rather than a strict count of the number of individuals.
#
# We will now take our PBDB occurrence data and convert it into rank abundance
# strings, one for each stage.
#
# First, we need to make an empty list to store the strings in:
stage_freq <- list()

# Then we loop through each stage:
for (i in 1:length(StageNames)) {
  
  # We filter the dataset to the desired stage, make the rank abundance string,
  # and add the string total at the start, before converting it into a vector:
  f_list <- RawData %>% filter(early_interval == StageNames[i]) %>%
    count(., genus) %>% arrange(desc(n)) %>% add_row(n = sum(.$n), .before = 1) %>%
    select(n)
  f_list <- unlist(f_list, use.names = FALSE)
  
  # If there are less than 3 taxa in the stage, we make the string NA (as SQS
  # won't work on so small a sample)...:
  if(f_list[1] < 3) f_list <- NA
  
  # ...and then we add the string to the list:
  stage_freq[[i]] <- f_list
  
}

# Once each stage has a rank abundance string, we rename the strings with their
# stages:
names(stage_freq) <- StageNames

# You can now take a look at your set of rank abundance strings:
glimpse(stage_freq)

# Or quickly plot just the Induan with:
barplot(stage_freq[["Induan"]], log = "y", col = "grey", border = 0, space = 0,
  xlab = "Rank order abundance", ylab = "Abundance")

# If any of the stages contained less than 3 taxa, they will now be listed as
# NA. These need to be removed before we run SQS:
stage_freq <- stage_freq[!is.na(stage_freq)]

# Now we start running SQS [this next bit of the code is based on Dunne et al.
# 2018].
#
# First we need to set our quorum levels, or the levels of sampling completeness
# you want to produce. Here we will use 0.4 to 0.7, a fairly standard range in
# the literature:
quorum_levels <- round(seq(from = 0.4, to = 0.7, by = 0.1), 1)

# Then we make an empty list to store the outputs in:
estD_list <- list()

# We loop through each desired quorum level:
for(i in 1:length(quorum_levels)) {
  
  # We use the estimateD function in iNEXT for our desired quorum level...:
  estD <- estimateD(stage_freq, datatype = "incidence_freq", base = "coverage",
    level = quorum_levels[i])
    
  # ...then filter the iNEXT output to the diversity estimates:
  estD <- filter(estD, order == 0)
  
  # We then label the estimates with the quorum level, measured sample size
  # (which we produced at the start of the script) and stage midpoint age:
  estD$quorum_level <- quorum_levels[i]
  estD$reference_t <- totals$n
  estD$midpoints <- StageMidpoints[match(names(stage_freq), StageNames)]
  
  # Finally the output is added to our list:
  estD_list[[i]] <- estD
  
}

# iNEXT may give you a warning at this point about overextrapolation beyond
# the measured sample size. We have labelled the estimates with their sample
# sizes to check this, and will do so shortly.
#
# Next we bind the individual dataframes into one:
estD_list <- bind_rows(estD_list)

# Then remove values where the estimated diversity is more than three times
# the sample size (see earlier):
estD_list[which(estD_list$t >= 3 * estD_list$reference_t), c("qD", "qD.LCL",
  "qD.UCL")] <- rep(NA, 3)

# Now we can look at our estimates:
View(estD_list)

# In order to plot the data, we need to make quorum level a 'factor':
estD_list$quorum_level <- as.factor(estD_list$quorum_level)

# And finally we can plot our diversity estimates. The error bars are 95%
# confidence intervals. This plot includes lines to indicate the start and end
# of our stages, as well as a red-dashed line for the Permian-Triassic boundary:
ggplot(estD_list, aes(x = midpoints, y = qD, ymin = qD.LCL, ymax = qD.UCL, group = quorum_level, colour = quorum_level)) +
  geom_line(size = 1) + geom_point() + geom_linerange() +
  scale_colour_manual(values = c("red", "orange", "darkgreen", "blue")) +
  scale_x_reverse(limits = c(266, 240)) + labs(x = "Ma", y = "SQS genus diversity", colour = "Quorum level") +
  geom_vline(aes(xintercept = 265.1), colour = "grey") + #Start of Capitanian
  geom_vline(aes(xintercept = 259.8), colour = "grey") + #Wuchiapingian
  geom_vline(aes(xintercept = 254.14), colour = "grey") + #Changhsingian
  geom_vline(aes(xintercept = 252.17), colour = "grey") + #Induan
  geom_vline(aes(xintercept = 252.17), linetype = "longdash", colour = "red") + #PT
  geom_vline(aes(xintercept = 251.2), colour = "grey") + #Olenekian
  geom_vline(aes(xintercept = 247.2), colour = "grey") + #Anisian
  geom_vline(aes(xintercept = 242), colour = "grey") + #End of Anisian
  theme_classic()


####################################
## (2) Squares
# Squares is an extrapolator developed by Alroy (2018). It is also a
# coverage-based approach which performs well when the rank abundance
# distributions of samples are particularly strongly skewed (i.e. many
# rare taxa, as is often the case with fossil data) (Alroy 2018, 2020).
# We use the equation as stated by Alroy (2018) to generate our diversity
# estimates.
#
# Squares also uses rank abundance strings of each taxon, but this time, we
# don't need the string sum at the front. So now, if you had a sample containing
# 1 Aenigmasaurus, 2 Dicynodon, 5 Lystrosaurus, 1 Moschorhinus, and 1 Prolacerta,
# the string would look like this:
#
# Karoo Sample   5 2 1 1 1
#
# As before, abundance here is the number of PBDB collections the taxon is
# present in.
#
# So again, we start by making an empty list to store the strings in:
stage_freq2 <- list()

# We then loop through each stage:
for (i in 1:length(StageNames)) {
  
  # We filter the dataset to that stage and make a rank abundance string:
  f_list2 <- RawData %>% filter(early_interval == StageNames[i]) %>% count(., genus) %>%
    arrange(desc(n)) %>% select(n)
    
  # We convert it to a vector for ease of use...:
  f_list2 <- unlist(f_list2, use.names = F)
  
  # ...and add the vector to the list:
  stage_freq2[[i]] <- f_list2
  
}

# We then rename the strings with their stages:
names(stage_freq2) <- StageNames

# You can see that the strings should be identical to the ones we made last
# time, just without the sum at the start:
glimpse(stage_freq2)

# Now we come to using the strings to estimate diversity with the squares method.
# Alroy (2018) states the equation for extrapolating with squares, and here we
# have the equation hard-coded so you can see what is happening.
#
# We start by, again, making an empty vector to store the squares estimates in:
squares_list <- vector("numeric", length = 0)

# Then we loop through each stage:
for(i in 1:length(stage_freq2)) {
  
  # This line filters the rank-abundance string of the stage of interest:
  count_list <- stage_freq2[[i]]
  
    # Then we find total number of genera in the string (by finding the length
    # of the list):
    genus_count <- length(count_list)
    
    # We find the number of singletons (the number of genera in the string with
    # an abundance of 1):
    sing_count <- sum(count_list == 1)
    
    # We find the number of individuals (the sum of the string):
    ind_count <- sum(count_list)
    
    # And finally we dind the sum of the string squared (this is where the name
    # comes from):
    sum_nsq <- sum(count_list^2)
    
    # Now we have our input parameters, we put them into our squares equation:
    squares <- genus_count + (((sing_count^2)*sum_nsq)/((ind_count^2) - (sing_count*genus_count)))
    
    # Because the equation is a fraction, you can get an estimate of infinity
    # if all taxa are singletons. As a safeguard against that, if the estimate
    # comes out as infinite, we replace it with the observed genus count:
    if(squares == Inf){squares <- genus_count}
    
  # Finally we add the squares estimate to the end of the vector:
  squares_list <- append(squares_list, squares)
  
}

# And that's it! We now have our list of diversity estimates per stage.
#
# We will label the squares estimates with their stage midpoints...:
to_plot <- data.frame(squares_list, StageMidpoints)

# ...and finally plot them. As before, the grey lines to indicate the start
# and end of our stages, with red-dashed line for the Permian-Triassic
# boundary:
ggplot(to_plot, aes(x = StageMidpoints, y = squares_list)) +
  geom_line(size = 1) + geom_point() +
  scale_x_reverse(limits = c(266, 240)) + labs(x = "Ma", y = "Squares genus diversity") +
  geom_vline(aes(xintercept = 265.1), colour = "grey") + #Start of Capitanian
  geom_vline(aes(xintercept = 259.8), colour = "grey") + #Wuchiapingian
  geom_vline(aes(xintercept = 254.14), colour = "grey") + #Changhsingian
  geom_vline(aes(xintercept = 252.17), colour = "grey") + #Induan
  geom_vline(aes(xintercept = 252.17), linetype = "longdash", colour = "red") + #PT
  geom_vline(aes(xintercept = 251.2), colour = "grey") + #Olenekian
  geom_vline(aes(xintercept = 247.2), colour = "grey") + #Anisian
  geom_vline(aes(xintercept = 242), colour = "grey") + #End of Anisian
  theme_classic()

#
# There are a multitude of other methods available for estimating evolutionary
# parameters whilst taking sampling into account. If you want to know more, we
# have provided a 'Further Reading' list which is a good place to start. For
# specific questions, you can ask us at the workshop drop-in, use the '#workshop'
# channel on Discord or email any of the workshop authors.
