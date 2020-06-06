# Sampling Bias in the Fossil Record
A workshop developed for Progressive Palaeontology 2020.

# Authors
Bethany Allen, Alex Dunhill and Graeme Lloyd

Palaeo@Leeds, School of Earth and Environment, University of Leeds, UK

# Getting started
This workshop is comprised of videos and practicals in R, a statistical programming language. In order to run the practicals, you will need to download and install R onto your computer. You may also want to install RStudio, a wrapper for R which organises your inputs and outputs into easy-to-see panels.

Download R by choosing your local CRAN mirror here: https://cran.r-project.org/mirrors.html

Download R Studio here: https://rstudio.com/products/rstudio/download/#download

We will only cover enough basic R here to allow new users to complete the practicals. If you want to learn more about R, we recommend the textbook "Getting Started with R: An Introduction for Biologists" by Andrew Beckerman, Dylan Childs and Owen Petchley.

To access the workshop files, click the green "Clone or download" button at the top right of this page, and select "Download ZIP". Once downloaded, open the folder by right-clicking and selecting "Extract all", saving the files somewhere you can access them.

If you have your own GitHub account, you may want to keep your own copy of this repository by 'forking' it, again via the relevant button at the top right of this page.

# Accessing the videos
There are two video lectures associated with the workshop. We advise that you watch the videos first, as these will introduce you to the key concepts.

The first is an introduction to the sources of sampling bias in the fossil record, given by Dr. Alex Dunhill, and can be found here: https://www.youtube.com/watch?v=smfm6mR-obE (duration: 45 mins)

The second is a discussion of what sampling bias means for palaeontologists who want to conduct quantitative analysis of the fossil record, given by Dr. Graeme Lloyd, and can be found here: https://www.youtube.com/watch?v=oKK4dERKqFU (duration: 55 mins)

# Running the practicals
To run the practicals, start R (or RStudio, if you have installed it). Using 'Open' at the top left of either program, open the script you want to run; they are numbered in their recommended order.

All of our scripts have been written to download data directly from the Paleobiology Database, so you shouldn't need to worry too much about where you've saved your files. However, if you do wish to use saved data, you will need to tell R where to find the files by setting the working directory. This is briefly covered in Script 1.

Start running the script line-by-line in the console. A # indicates a comment - that's for you to read, not the computer. We have tried to be extensive in our explanations of what is happening in the code to help you understand the processes.

We will be holding a live Q&A session as part of the conference to answer any questions - this will be at 2pm BST on Friday 12th June. Delegates will have a Zoom link to the session emailed to them.

# Further reading
Raup 1972 (www.jstor.org/stable/1734207): One of the most famous papers in macroevolution, this is the first attempt at producing a diversity curve for the whole Phanerozoic. This paper is where a lot of core ideas relating to models and subsampling were laid out.

Nichols & Pollock 1983 (https://doi.org/10.1017/S0094837300007533) and Liow & Nichols 2010 (https://doi.org/10.1017/S1089332600001820): The papers which first outlined how the mechanisms behind capture-mark-recapture (CMR) methods, commonly applied in extant ecology, can be used to develop statistical tools to understand sampling intensity in the fossil record.

Colwell & Coddington 1994 (https://doi.org/10.1098/rstb.1994.0091): A review paper discussing the theory of collector curves and how they relate to extrapolation metrics for estimating diversity. Written from an ecology perspective, but also holds for analysis of the fossil record.

Peters 2005 (https://doi.org/10.1073/pnas.0502616102): This paper outlines the 'common cause' hypothesis, discussing how environmental drivers can influence both biodiversity and sampling biases to shape our perception of diversity in deep time.

Smith 2007 (https://doi.org/10.1144/0016/76492006-184): A review paper considering sampling bias in the marine fossil record and the relative success of various 'correction' methods.

Alroy 2010 (https://doi.org/10.1111/j.1475-4983.2010.01011.x): The original paper on shareholder quorum subsampling (SQS). Not the most accessible paper, but discusses a lot of important sources of bias in large-scale datasets.

Benton et al. 2011 (https://doi.org/10.1144/SP358.6): A review paper on sampling bias in the vertebrate fossil record, with particular consideration of sampling proxies (metrics relating to the scale of a particular facet of sampling bias).

Benson & Upchurch 2013 (https://doi.org/10.1130/G33543.1): One of the first papers to illustrate the importance of understanding, and differentiating between, spatial and temporal aspects of sampling bias.

Silvestro et al. 2014 (https://doi.org/10.1111/2041-210X.12263) and Silvestro et al. 2019 (https://doi.org/10.1017/pab.2019.23): Papers outlining the program PyRate, which estimates speciation, extinction, and preservation rates from fossil occurrence data using a Bayesian framework.

Hsieh et al. 2016 (https://doi.org/10.1111/2041-210X.12613): The paper outlining iNEXT, one of the R packages we use here. The paper discusses the mechanisms used by the package, the assumptions made about the data put into it, and gives examples of how the package produces subsampled and extrapolated estimates of diversity.

Starrfelt & Liow 2016 (https://doi.org/10.1098/rstb.2015.0219): The first paper to estimate diversity from the fossil record using TRiPS (True Richness estimated using a Poisson Sampling model).

Close et al. 2017 (https://doi.org/10.1038/ncomms15381), Reddin et al. 2018 (https://doi.org/10.1111/geb.12732), Antell et al. 2020 (https://doi.org/10.1016/j.cub.2019.10.065) and Close et al. 2020 (https://doi.org/10.1126/science.aay8309): Examples of papers developing methods to produce diversity estimates through time while controlling for spatial sampling bias.

Alroy 2018 (https://doi.org/10.1111/ele.13152): The paper which outlines the squares extrapolator we implement. It's based on the ideas outlined in Colwell and Coddington (1994; see above).

Close et al. 2018 (https://doi.org/10.1111/2041-210X.12987) and Alroy 2020 (https://doi.org/10.1017/pab.2019.40): Important, if a bit technical, papers which compare the performance of different methods to produce diversity estimates from the fossil record.

Dunne et al. 2018 (https://doi.org/10.1098/rspb.2017.2730): A nice example of applying SQS to examine diversity through time, specifically to understand the impact of the Carboniferous Rainforest Collapse on tetrapods.

Dunhill et al. 2018 (https://doi.org/10.1111/pala.12331): A simulation-based study showing that geological sampling proxies cannot be used to try and 'correct' for sampling bias when estimating diversity.

Stadler et al. 2018 (https://doi.org/10.1016/j.jtbi.2018.03.005), Hopkins et al. 2018 (https://doi.org/10.1017/pab.2018.27) and Warnock et al. 2020 (https://doi.org/10.1017/pab.2020.12): Papers considering the role of sampling on our understanding of phylogenies and evolutionary processes, introducing and advocating for use of the fossilised birth-death model.

Kocsis et al. 2019 (https://doi.org/10.1111/2041-210X.13161): The paper outlining divDyn, an R package containing a comprehensive suite of functions for analysing data in the Paleobiology Database.

Allen et al. 2020 (in press; https://doi.org/10.1098/rspb.2020.1125): An example of applying and comparing SQS and squares, across both spatial and temporal bins, specifically to look at the impact of the end-Permian mass extinction on the latitudinal diversity gradient of tetrapods.
