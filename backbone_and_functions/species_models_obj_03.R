#Created by Keith Post on 2/10/23
#Code for objects used in Species Model App for Rarefaction mini-app

#### Datasets=======================================================================================
### Create vector of choices
datasets_rf<-c("50-1 ha plots of tree species on Barro Colorado Is"="BCI",
               "20 sites of cover class values of Dutch dune meadow vegetation" ="dune",
               "70 soil cores of oribatid mite counts"="mite",
               "18 islands of land bird species counts in Sipoo Arch., Fl"="sipoo")

### Load datasets
rf_dfs_names<-c("BCI","dune","mite","sipoo")
data(list=rf_dfs_names)




