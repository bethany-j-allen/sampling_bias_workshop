################################################################################
#                                                                              #
#                   SCRIPT III - SQS AND SQUARES                               #
#                                                                              #
################################################################################

# SCRIPT AIMS:
#
# 1. Learn how to use Shareholder Quorum Subsampling in R.
# 2. Learn how to use the Squares extrapolator in R.

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

####################################
## (1) SQS

#  Generate a table of sample sizes by stage - this will be used later to evaluate whether our
#  diversity estimates are overextrapolated (Hsieh et al. 2016, the original iNEXT paper, says that
#  an estimate of more than 2-3x your sample size should be considered unreliable)
totals <- count(RawData, early_interval)
#  Order the substages in the table chronologically
totals <- totals[match(StageNames, totals$early_interval),]

#  iNEXT requires input data in a very specific format. Each 'bin' that you want to sample needs a
#  string of the relative abundances of each taxon, with the first number in the string being the total
#  number of occurrences. So, if you had a sample containing 1 Aenigmasaurus, 2 Dicynodon,
#  5 Lystrosaurus, 1 Moschorhinus, and 1 Prolacerta, the string would look like this:
# Karoo Sample   10 5 2 1 1 1
#  The nature of data in the PBDB means that, particularly with vertebrate data, the 'abundance' of a
#  taxon is usually a count of the number of collections (~ number of localities) in which it is
#  present, rather than a strict count of the number of individuals.

#  The next bit of code takes occurrences in the format of a standard PBDB download and generates
#  the necessary abundance strings, with each geologic stage treated as a separate bin.

#  Make an empty list to store the strings in
stage_freq <- list()

#  Loop through each stage
for (i in 1:length(StageNames)) {
  #  Filter the dataset to the desired stage, make a count string, and start with the string total
  f_list <- RawData %>% filter(early_interval == StageNames[i]) %>%
    count(., genus) %>% arrange(desc(n)) %>% add_row(n = sum(.$n), .before = 1) %>%
    select(n)
  f_list <- unlist(f_list, use.names = F)
  #  If there are less than 3 taxa in the bin, make the string NA
  if(f_list[1] < 3){f_list <- NA}
  #  Add the string to the list
  stage_freq[[i]] <- f_list
}
#  Rename the strings with their stages
names(stage_freq) <- StageNames
#  Look at the strings to check they are sensible
glimpse(stage_freq)

#  Filter out the NA strings (bins with less than 3 taxa) if necessary
stage_freq <- stage_freq[!is.na(stage_freq)]

#  [This next bit of the code is based on Dunne et al. 2018 (https://doi.org/10.1098/rspb.2017.2730)]

#  Set your quorum levels - this is the proportion of taxa you want to sample up to, in the paper they
#  used 0.4 - 0.7, which is a fairly standard range
quorum_levels <- round(seq(from = 0.4, to = 0.7, by = 0.1), 1)

#  Estimate your diversity levels using estimateD in iNEXT
#  Make an empty list to store the outputs in
estD_list <- list()

#  Loop through each desired quorum level
for(i in 1:length(quorum_levels)) {
  estD <- estimateD(stage_freq, datatype = "incidence_freq", base = "coverage", level = quorum_levels[i])
  #  Filter the iNEXT output to the species diversity estimates
  estD <- filter(estD, order == 0)
  #  Label with the relevant quorum level, sample size and stage midpoint age
  estD$quorum_level <- quorum_levels[i]
  estD$reference_t <- totals$n
  estD$midpoints <- StageMidpoints[match(names(stage_freq), StageNames)]
  #  Add the output to the list
  estD_list[[i]] <- estD
}

#  [You might get an error at this point warning you about overextrapolation - we'll fix that shortly]

#  Bind the individual dataframes into one
estD_list <- bind_rows(estD_list)

#  Remove values where estimated diversity is more than three times the sample size (see earlier)
estD_list[which(estD_list$t >= 3 * estD_list$reference_t), c("qD", "qD.LCL", "qD.UCL")] <- rep(NA, 3)

#  Take a look at the list to check it looks sensible
View(estD_list)

#  Make quorum level a factor (this helps with plotting)
estD_list$quorum_level <- as.factor(estD_list$quorum_level)

#  Plot your diversity estimates - the error bars are 95% confidence intervals
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

#  Squares also uses strings of the relative abundances of each taxon, but without the string sum
#  at the front. So this time, if you had a sample containing 1 Aenigmasaurus, 2 Dicynodon,
#  5 Lystrosaurus, 1 Moschorhinus, and 1 Prolacerta, the string would look like this:
# Karoo Sample   5 2 1 1 1
#  The nature of data in the PBDB means that, particularly with vertebrate data, the 'abundance' of a
#  taxon is usually a count of the number of collections (~ number of localities) in which it is
#  present, rather than a strict count of the number of individuals.

#  The next bit of code takes occurrences in the format of a standard PBDB download and generates
#  the necessary abundance strings, with each geologic stage treated as a separate bin.

#  Make an empty list to store the strings in
stage_freq2 <- list()

#  Loop through each stage
for (i in 1:length(StageNames)) {
  #  Filter the dataset to the desired stage and make a count string
  f_list2 <- RawData %>% filter(early_interval == StageNames[i]) %>% count(., genus) %>%
    arrange(desc(n)) %>% select(n)
  f_list2 <- unlist(f_list2, use.names = F)
  #  Add the string to the list
  stage_freq2[[i]] <- f_list2
}

#  Rename the strings with their stages
names(stage_freq2) <- StageNames
#  Look at the strings to check they are sensible
glimpse(stage_freq2)

#  Estimate diversity using squares method, based on the equation by Alroy (2018, see Further reading)

#  Make an empty vector to store the squares estimates in
squares_list <- vector("numeric", length = 0)

#  Loop through each stage
for(i in 1:length(stage_freq2)) {
  count_list <- stage_freq2[[i]]
    # Find number of genera
    genus_count <- length(count_list)
    #  Find number of singletons
    sing_count <- sum(count_list == 1)
    #  Find number of individuals (sum of string)
    ind_count <- sum(count_list)
    #  Find the sum of the string squared (hence the name)
    sum_nsq <- sum(count_list^2)
    #  The equation
    squares <- genus_count + (((sing_count^2)*sum_nsq)/((ind_count^2) - (sing_count*genus_count)))
    #  Because the equation is a fraction, you can get an estimate of infinity if all taxa are
    #  singletons - this is a failsafe against that
    if(squares == Inf){squares <- length(count_list)}
  #  Add the squares estimate to the end of the vector
  squares_list <- append(squares_list, squares)
}

#  Label the squares estimates with the stage midpoints ready for plotting
to_plot <- data.frame(squares_list, StageMidpoints)

#  Plot your diversity estimates
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
