#Created by Keith Post on 1/22/23
#Code for objects used in Species Model App for Species-Area Relationships mini-app

#### Datasets=======================================================================================
### Create vector of choices
datasets_sa<-c("Invertebrate species on Greek Island"="aegean",
               "Plant species on Greek Island"="aegean2",
               "Plant species on Galapagos Islands"="galap",
               "Plant species on islands in Kapingamarangi"="niering")

### Load datasets
sars_dfs_names<-c("aegean","aegean2","galap","niering")
data(list=sars_dfs_names)



#### tabPanel titles===============================================================================
out_tab_titles_sa<-c("Drawing Models","Model Fitting")


### User Guide=====================================================================================
### Background 
bp_sa<-"This Shiny mini-app illustrates species-area relationships (SARs), which models the relationship
between species richness and sampling area. Historically, SARs have been modeled using two approaches: 
a power function where S = cA^z or semilog model where S = log(c) + zlog(A). In these equations, S =
species richness, A = area sampled, z = a constant that typically ranges from 0.10 to 0.35, and c = a 
constant which represents the number of species when A = 1 and depends on the organism type. Note 
that the power function is often expressed in log space, changing the form to log(S) = log(c) + zlog(A)."


#### Instructions
## Drawing Models
instruct_dm_sa<-"By default, all plots are displayed but feel free de-select 1-2. Choose the range of
sampling area by selecting the min and max x, which is the power of area expressed in 10^x. Then
choose c and z using the second and third sliders. The plot(s) will adjust based on these three
inputs. Reset the values by clicking on 'Reset sliders' button."

# Model Fitting
instruct_mf_sa<-"Select one of four datasets. This generates plots of the power function on 
log-log scale, the semi-log model, and a summary table below comparing model fits (e.g., p, R^2,
RSE). The plots are interactive in that you can hover over points, models, and CI bands."

