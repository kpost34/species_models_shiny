#Created by Keith Post on 2/10/23
#Code for objects used in Species Model App for Rarefaction mini-app

#### Datasets=======================================================================================
### Create vectors of choices
datasets_rf<-c("50-1 ha plots of tree species on Barro Colorado Is"="BCI",
               "20 sites of cover class values of Dutch dune meadow vegetation" ="dune",
               "70 soil cores of oribatid mite counts"="mite",
               "18 islands of land bird species counts in Sipoo Arch., Fl"="sipoo")

### Create vectors of choices
datasets2_rf<-c("50-1 ha plots of tree species on Barro Colorado Is"="BCI",
               "20 sites of cover class values of Dutch dune meadow vegetation" ="dune",
               "70 soil cores of oribatid mite counts"="mite")


### Load datasets
rf_dfs_names<-c("BCI","dune","mite","sipoo")
data(list=rf_dfs_names)


#### Choices========================================================================================
### Species-accumulation curve options
sac_rf<-c(
  "Use site order in dataset" = "collector",
  "Random order of sites" = "random",
  "Expected species richness (by site)" = "exact",
  "Expected species richness (by ind)" = "rarefaction"
)


##### tabPanel titles===============================================================================
out_tab_titles_rf<-c("Species Accumulation","Rarefaction")



#### User Guide=====================================================================================
### Background 
## SACs
bp_sac_rf<-"This Shiny mini-app demonstrates two critical ideas in community ecology: species-
accumulation curves (SACs) and rarefaction. In SACs or collector's curves, species richness is 
plotted against either sites or individuals sampled from a given area to visualize this relationship.
This information can be used to extrapolate the numbers of species in a given area (i.e., estimated 
species richness) using various estimators."

## Rarefaction
bp_rare_rf<-"Rarefaction focuses on interpolation. Specifically it's a tool used to compare species 
richness among sites with different numbers of individuals because richness is often not linearly 
related to sample size or site numbers."


#### Instructions
## SACs
instruct_sac_rf<-"Select a dataset from four options. Use the slider to set the number of sites
(e.g., plots, cores) sampled from a given area. Choose the SAC method: 1) in
numerical order of sites; 2) in random order of sites; 3) using expected (mean) richness by sites;
and 4) by individuals. Note that the SAC appears once a dataset is selected and dynamically changes
if the 1) dataset, 2) slider, or 3) SAC method is changed. Individual data point values can be
displayed by hovering over each point. Finally, to see the expected species richness using
two different methods (i.e., Chao and ACE), check that box."

## Rarefaction
instruct_rare_rf<-"Choose one of three datasets to display rarefaction curves. Once completed,
rarefaction curves appear on the plot to the right. Select the number of sites (curves) using the
first slider. Note that despite datasets having upwards of 70 sites, only 2-6 can be selected. Next,
choose the number of subsamples (individuals) using the second slider. This value is limited to the 
site with the fewest individuals. As this slider is moved, a corresponding vertical line is shifted 
along the x-axis of the plot and a rarefaction table displaying the intersection points is displayed 
below the plot. Like the SACs, the rarefaction plot is interactive by hovering the pointer over the
curves."









