########################################################
# Bethany' code
########################################################

#Load packages
library(tidyverse)
library(iNEXT)

tetrapods <- read_csv("data/tetrapods.csv")

#Create a vector giving the chronological order of substages
substages <- c("Wuchiapingian", "Changhsingian", "Griesbachian", "Smithian", "Spathian",
               "Aegean-Bithynian", "Pelsonian-Illyrian", "Fassanian", "Longobardian", "Julian",
               "Tuvalian")

#Create a vector of substage midpoints
midpoints <- c(256.6, 253, 251.7, 250.2, 248.2, 245.9, 243.3, 240.8, 238.3, 234.5, 229.5)

####################################
## (1) SQS

#  When taxa are made synonymous in the PBDB, they are retained as separate entries with the same
#  name. If you want to know diversity, this is an issue, as it articifially inflates your estimate.
#  We can stop this from happening by stripping out combinations of the same collection no. AND
#  accepted name.
tetrapods <- distinct(tetrapods, accepted_name, collection_no, .keep_all = T)

#  Remove occurrences which aren't dated to substage level
tetrapods <- tetrapods %>% filter(!is.na(substage_assignment))

#  Generate a table of sample sizes by substage - this will be used later to evaluate whether our
#  diversity estimates are overextrapolated (Hsieh et al. 2016, the original iNEXT paper, says that
#  an estimate of more than 2-3x your sample size should be considered unreliable)
totals <- count(tetrapods, substage_assignment)
#  Order the substages in the table chronologically
totals <- totals[match(substages, totals$substage_assignment),]

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
for (i in 1:length(substages)) {
  #  Filter the dataset to the desired stage, make a count string, and start with the string total
  f_list <- totals %>% filter(substage_assignment == substages[i]) %>%
    count(., accepted_name) %>% arrange(desc(n)) %>% add_row(n = sum(.$n), .before = 1) %>%
    select(n)
  f_list <- unlist(f_list, use.names = F)
  #  If there are less than 3 taxa in the bin, make the string NA
  if(f_list[1] < 3){f_list <- NA}
  #  Add the string to the list
  stage_freq[[i]] <- f_list
}
#  Rename the strings with their stages
names(stage_freq) <- substages
#  Look at the strings to check they are sensible
glimpse(stage_freq)

#  Filter out the NA strings (stages with less than 3 taxat)
stage_freq <- stage_freq[!is.na(stage_freq)]

#  [This next bit of the code is based on Dunne et al. 2018 (https://doi.org/10.1098/rspb.2017.2730)]

#  Set your quorum levels - this is the proportion of taxa you want to sample up to, in the paper they
#  used 0.4 - 0.7, which is a fairly standard range
quorum_levels <- round(seq(from = 0.4, to = 0.7, by = 0.1), 1)

#  Estimate your diversity levels using estimateD in iNEXT
#  Make an empty list to store the outputs in
t.estD_list <- list()

#  Loop through each desired quorum level
for(i in 1:length(quorum_levels)) {
  t.estD <- estimateD(stage_freq, datatype = "incidence_freq", base = "coverage", level = quorum_levels[i])
  #  Filter the iNEXT output to the species diversity estimates
  t.estD <- filter(t.estD, order == 0)
  #  Label with the relevant quorum level, sample size and stage midpoint age
  t.estD$quorum_level <- quorum_levels[i]
  t.estD$reference_t <- terres_totals$n
  t.estD$midpoints <- midpoints[match(names(terres_substage_freq), substages)]
  #  Add the output to the list
  t.estD_list[[i]] <- t.estD
}

#  Bind the individual dataframes into one
t.estD_list <- bind_rows(t.estD_list)

#  Remove values where estimated diversity is more than three times the sample size (see earlier)
t.estD_list[which(t.estD_list$t >= 3 * t.estD_list$reference_t), c("qD", "qD.LCL", "qD.UCL")] <- rep(NA, 3)

#  Take a look at the list to check it looks sensible
View(t.estD_list)

#  Make quorum level a factor (this helps with plotting)
t.estD_list$quorum_level <- as.factor(t.estD_list$quorum_level)

#  Plot your diversity estimates - the error bars are 95% confidence intervals
ggplot(t.estD_list, aes(x = midpoints, y = qD, ymin = qD.LCL, ymax = qD.UCL, group = quorum_level, colour = quorum_level)) +
  geom_line(size = 1) + geom_point() + geom_linerange() +
  scale_colour_manual(values = c("chartreuse1", "chartreuse2", "chartreuse3", "chartreuse4")) +
  scale_x_reverse(limits = c(260, 227)) + labs(x = "Ma", y = "Interpolated diversity", colour = "Quorum level") +
  theme_classic()
