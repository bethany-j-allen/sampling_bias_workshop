# Sampling Bias workshop
A workshop on sampling bias for the Progressive Palaeontology 2020 conference.

# Authors
Bethany Allen, Alex Dunhill and Graeme Lloyd

# Resources
Link to the Google Doc
https://docs.google.com/document/d/1qcvRaOIkBRDdG3dvco6amFj6tyhLrb1_8u7CPC7pZJM/edit?ts=5e9d71cd

Link to Alex's video
https://drive.google.com/file/d/1VfbfV1gsiKUwOQUrPGr3iGas1O89QvwC/view

Link to Graeme's slides
https://docs.google.com/presentation/d/1gBbQTGRhejkAmWhyMDbV42IkM5-6k9YpYvUPJXUKaPE/edit#slide=id.p

# Getting started
This workshop is comprised of videos, annotated slides and practicals in R, a statistical programming language. In order to run the practicals, you will need to download and install R onto your computer. You may also want to install R Studio, a wrapper for R which organises your inputs and outputs into easy-to-see panels.

Download R by choosing your local CRAN mirror here: https://cran.r-project.org/mirrors.html

Download R Studio here: https://rstudio.com/products/rstudio/download/#download

To access the workshop files, click the green "Clone or download" button at the top right of this page, and select "Download ZIP". Once downloaded, open the folder by right-clicking and selecting "Extract all", saving the files somewhere you can access them - you will have to tell R where the files are when you start the practicals.

If you have your own GitHub account, you may want to keep your own copy of this repository by 'forking' it, again via the relevant button at the top right of this page.

# Further reading
Raup 1972 (www.jstor.org/stable/1734207): One of the most famous papers in macroevolution, this is the first attempt at producing a diversity curve for the whole Phanerozoic. This paper is where a lot of core ideas relating to models and subsampling were laid out.

Nichols & Pollock 1983 (https://doi.org/10.1017/S0094837300007533) and Liow & Nichols 2010 (https://doi.org/10.1017/S1089332600001820): The papers which first outlined how the mechanisms behind capture-mark-recapture (CMR) methods, commonly applied in extant ecology, can be used to develop statistical tools to understand sampling intensity in the fossil record.

Colwell & Coddington 1994 (https://doi.org/10.1098/rstb.1994.0091): A review paper discussing the theory of collector curves and how they relate to extrapolation metrics for estimating diversity. Written from an ecology perspective, but also holds for analysis of the fossil record.

Peters 2005 (https://doi.org/10.1073/pnas.0502616102): This paper outlines the 'common cause' hypothesis, discussing how environmental drivers can influence both biodiversity and sampling biases to shape our perception of diversity in deep time.

Smith 2007 (https://doi.org/10.1144/0016/76492006-184): A review paper considering sampling bias in the marine fossil record and the relative success of various 'correction' methods.

Alroy 2010 (https://doi.org/10.1111/j.1475-4983.2010.01011.x): The original paper on shareholder quorum subsampling (SQS). Not the most accessible paper, but discusses a lot of important sources of bias in large-scale datasets.

Benton et al. 2011 (https://doi.org/10.1144/SP358.6): A review paper on sampling bias in the vertebrate fossil record, with particular consideration of sampling proxies (metrics relating to the scale of a particular facet of sampling bias).

Benson & Upchurch 2013 (https://doi.org/10.1130/G33543.1): One of the first papers to illustrate the importance of understanding, and differentiating between, spatial and temporal aspects of sampling bias.

Hsieh et al. 2016 (https://doi.org/10.1111/2041-210X.12613): The paper outlining iNEXT, one of the R packages we use here. The paper discusses the mechanisms used by the package, the assumptions made about the data put into it, and gives examples of how the package produces subsampled and extrapolated estimates of diversity.

Starrfelt & Liow 2016 (https://doi.org/10.1098/rstb.2015.0219): The first paper to estimate diversity from the fossil record using TRiPS (True Richness estimated using a Poisson Sampling model).

Close et al. 2017 (https://doi.org/10.1038/ncomms15381), Antell et al. 2020 (https://doi.org/10.1016/j.cub.2019.10.065) and Close et al. 2020 (https://doi.org/10.1126/science.aay8309): Papers developing methods to produce diversity estimates through time while controlling for spatial sampling bias.

Alroy 2018 (https://doi.org/10.1111/ele.13152): The paper which outlines the squares extrapolator we implement. It's based on the ideas outlined in Colwell and Coddington (1994; see above).

Close et al. 2018 (https://doi.org/10.1111/2041-210X.12987) and Alroy 2020 (https://doi.org/10.1017/pab.2019.40): Important, if a bit technical, papers which compare the performance of different methods to produce diversity estimates from the fossil record.

Dunne et al. 2018 (https://doi.org/10.1098/rspb.2017.2730): A nice example of applying SQS to examine diversity through time, specifically to understand the impact of the Carboniferous Rainforest Collapse on tetrapods.

Dunhill et al. 2018 (https://doi.org/10.1111/pala.12331): A simulation-based study showing that geological sampling proxies cannot be used to try and 'correct' for sampling bias when estimating diversity.

Kocsis et al. 2019 (https://doi.org/10.1111/2041-210X.13161): The paper outlining divDyn, an R package containing a comprehensive suite of functions for analysing data in the Paleobiology Database.
