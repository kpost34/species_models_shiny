#Created by Keith Post on 2/5/23
#Backbone code for Species Models App: Rarefaction

pacman::p_load(here,tidyverse,vegan,plotly)

#### Barro Colorado Island (BCI) data: 50-1 ha plots (in a total area of 50 ha) of 225 neotropical 
  #tree species with counts of inds greater than 10 cm DBH
data(BCI)


### Species-accumulation curve (plots 1-50)
## Hard coded
# Generate cumulative ns
BCI %>%
  rowSums() %>%
  cumsum() %>%
  as_tibble() -> n

# Find when new species are added then convert to cumulative species & bind with cumulative inds and sites
BCI %>%
  mutate(across(everything(),cumsum),
         across(everything(),~ifelse(.x>0,1,0)),
         across(everything(),~ifelse(row_number(.x)==1 & .x==1|.x==1 & lag(.x,1)==0,
                                     "Yes",
                                     .x))) %>%
  rowwise() %>%
  mutate(S=sum(c_across()=="Yes")) %>%
  ungroup() %>%
  select(S) %>%
  mutate(S=cumsum(S)) %>%
  
  bind_cols(samp=1:50,n) %>%
  select(samp,n="value",S) -> bci_spac_sitensDF
        

## vegan functions
# Sites-S
#in order of collection
specaccum(BCI,method="collector") %>%
    .[3:4] %>%
  bind_rows() -> bci_spac_sites_orderDF

bci_spac_sites_orderDF %>%
  ggplot(aes(sites,richness)) +
  geom_point()

#subsample
bci<-BCI[sample(10),]

specaccum(bci,method="collector") %>%
    .[3:4] %>%
  bind_rows() -> subbci_spac_sites_orderDF

subbci_spac_sites_orderDF %>%
  ggplot(aes(sites,richness)) +
  geom_point()



#random order
specaccum(BCI,method="random",permutations=100) %>%
    .[3:4] %>%
  bind_rows() -> bci_spac_sites_randDF

bci_spac_sites_randDF %>%
  ggplot(aes(sites,richness)) +
  geom_point()

#expected value
specaccum(BCI,method="exact") %>%
    .[3:4] %>%
  bind_rows() -> bci_spac_sites_expDF

bci_spac_sites_expDF %>%
  ggplot(aes(sites,richness)) +
  geom_point()


# N-S
specaccum(BCI,method="rarefaction") %>%
  .[c(7,4)] %>%
  bind_rows() -> bci_spac_nsDF


bci_spac_nsDF %>%
  ggplot(aes(individuals,richness)) +
  geom_point()



### Estimate total species richness
## Sample data
#randomly choose 10 rows & all cols
bci_samp<-BCI[sample(1:50,10),]


## Total inds for each species (excluding 0s)
bci_spp_count<-colSums(bci_samp) %>% 
  .[.>0]
bci_total_spp<-length(bci_spp_count)

BCI_samp_count<-colSums(BCI) %>%
  .[.>0]

estimateR(bci_spp_count)


specaccum(bci_samp,method="random")


### Rarefaction 
## Rarefy to mininum number of individuals from a site
raremax<-min(rowSums(BCI))
BCI_rareDF<-rrarefy(BCI,raremax)
rarefy(BCI,raremax)

rarefy(BCI[1:10,],20)

## Generate curve
rarecurve(BCI)

#use subset of community
bci<-BCI[sample(10),]

# By individuals
out<-rarecurve(bci,label=FALSE)
names(out)<-as.character(1:nrow(bci))

map2_df(out,names(out),.f=function(x,y){
  x %>%
    as_tibble() %>%
    mutate(individuals=row_number(),
           site=y) %>%
    rename(species="value") %>%
    relocate(site,individuals,species)
}) -> protox


protox %>%
  ggplot(aes(x=individuals,y=species,color=site)) +
  geom_line() +
  theme_bw() +
  theme(legend.position="none") -> p

p %>%
  ggplotly()


# By samples
out2<-rarecurve(BCI,step=10)




map2_df(out,names(out),.f=function(x,y){
  mydf <- as.data.frame(x)
  colnames(mydf) <- "value"
  mydf$species <- y
  mydf$subsample <- attr(x, "Subsample")
  mydf
}) -> protox

rownames(protox)<-NULL


xy <- do.call(rbind, protox)
rownames(xy) <- NULL  # pretty

ggplot(xy, aes(x = subsample, y = value, color = species)) +
  theme_bw() +
  scale_color_discrete(guide = "none") +  # turn legend on or off
  geom_line()

ggplotly(
  ggplot(xy, aes(x = subsample, y = value, color = species)) +
    theme_bw() +
    theme(legend.position = "none") +  # ggplotly doesn't respect scales?
    geom_line()
)




#### Plans==========================================================================================
# 1) choose data set (optional...or maybe just given one with brief description)
# 2) slider with the subsample size
# 3) choose spec accumulation type (collector, random, exact)
# 4) predict total richness & choose method
# 5) #4 would be added to plot
# 6) separate tab -- rarefaction
# 7) subsample data to start (e.g., BCI study would start at < 10 sites instead of 50)
# 8) visual with individuals and species
# 9) slider for sample size for rarefaction
# 10) new set of graphs -> grouped bar charts??
# 11) rarecurve too
























