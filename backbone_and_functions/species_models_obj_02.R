#Created by Keith Post on 1/22/23
#Code for objects used in Species Model App for Species-Area Curves mini-app

#### Datasets=======================================================================================
### Create vector of choices
datasets_sa<-c("Invertebrate species on Greek Is"="aegean",
               "Plant species on Greek Is"="aegean2",
               "Plant species on Galapagos Is"="galap",
               "Plant species on islands in Kapingamarangi"="niering")

### Load datasets
sars_dfs_names<-c("aegean","aegean2","galap","niering")
data(list=sars_dfs_names)



##### tabPanel titles===============================================================================
out_tab_titles_sa<-c("Drawing Models","Model Fitting")


