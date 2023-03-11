#Created by Keith Post on 1/4/23
#Code for objects used in Species Models App for Island Biogeography mini-app



#### Scenario Options===============================================================================
### Choices for first radio button
#choose between a scenario or custom settings
scenarios1_ib<-c("scenario (e.g., large vs small islands)"="scenario",
                 "custom specifications" = "custom")


### Choices for second radio button
#if scenario selected, then which scenario (which aligns with tabset panel)
scenarios2_ib<-c("large vs small islands"="lvs",
                "near vs distant islands"="nvd",
                "large, near vs small, distant islands"="lnvsd",
                "large, distant vs small, near islands"="ldvsn")



#### Output Tabset Object===========================================================================
out_tabs_ib<-tabsetPanel(id="out_tabset_ib",type="hidden",
              #app opens with blank panel
              tabPanel("tab_blank_ib"),
              #if scenario selected, only schematic of scenarios 1-4 outputted here
              tabPanel(scenarios1_ib[1],
                plotOutput("plot_sc1_4_schematic_ib"),
              ),
              #custom model yields plots from custom inputs
              tabPanel(scenarios1_ib[2],
                plotOutput("plot_cust_schematic_ib",
                           width="85%",
                           height="350px"),
                br(),
                plotlyOutput("plotly_cust_rate_ib",
                             width="85%",
                             height="450px"),
                plotlyOutput("plotly_cust_sppt_ib",
                             width="85%",
                             height="450px"),
              )
            )


#### Text Sidebar Obj===============================================================================
#need to add text later to describe scenarios
sc1_text_ib<-"In this scenario, the islands are the same distance from the mainland but differ in
size. If all other factors are held constant, the colonization rate function is the same for both
islands. However, the extinction rate function has a steeper, positive slope for the small island. 
Thus, the small island has a smaller equilibrium species richness than the larger island."


sc2_text_ib<-"In this scenario, the islands are the same size but differ in their distance from
the mainland. If all other factors are held constant, the extinction rate function is the same
for both islands. However, the slope of the colonization rate function of the near island has a 
greater magnitude (more negative) than the far island. Thus, the near island has a larger equilibrium 
species richness than the distant island."


sc3_text_ib<-"In this scenario, one island is large and close to the mainland, while the other
island is small and far from the mainland. If all other factors are held constant, the
extinction rate function of the large, near island would have a smaller slope, and the slope of
its colonization rate function would have a greater magnitude (more negative) than the small, 
distant island. In other words, the large, near island would have less extinction and greater 
colonization than the small, distant island, which yields a larger equilibrium species richness."


sc4_text_ib<-"In this scenario, one island is large and far from the mainland, while the other
island is small and close to the mainland. If all other factors are held constant, the slope of
the colonization rate of the large, distant island has a smaller magnitude (less negative) than
the small, near island because of its distance from the mainland, but the slope of the extinction 
rate function is smaller because of the island's greater size. These conditions indicate less
colonization but greater extinction than the small, near island. Thus, in this scenario, the 
magnitude of the differences in island size and distance from mainland will determine which 
island has greater equilibrium species richness. Using the numbers in this particular example, the
large, distance island has more species at equilibrium."



#### Rate DF Obj====================================================================================
#build each scenario rateDF separately to make switch() in reactive({}) easier to read

### Sc1
sc1_rateDF_ib <- bind_rows(
  build_rate_static_df(island="large island",d=1000,a=5000,rate="Extinction"),
  build_rate_static_df(island="small island",d=1000,a=1000,rate="Extinction"),
  #note: area does not affect colonization rate
  build_rate_static_df(island="both islands",d=1000,a=1000,rate="Colonization")
  ) 

### Sc2
sc2_rateDF_ib <- bind_rows(
  build_rate_static_df(island="near island",d=1000,a=1000,rate="Colonization"),
  build_rate_static_df(island="distant island",d=5000,a=1000,rate="Colonization"),
  #note: distance does not affect extinction rate
  build_rate_static_df(island="both islands",d=1000,a=1000,rate="Extinction")
)

### Sc3
sc3_rateDF_ib <- bind_rows(
  build_rate_static_df(island="large, near island",d=1000,a=5000,rate="Both"),
  build_rate_static_df(island="small, distant island",d=5000,a=1000,rate="Both")
)

### Sc4
sc4_rateDF_ib <- bind_rows(
  build_rate_static_df(island="large, distant island",d=5000,a=5000,rate="Both"),
  build_rate_static_df(island="small, near island",d=1000,a=1000,rate="Both")
)



#### Equilibrium Rate DF Obj========================================================================
#build each scenario rate_eqDF separately to make switch() in reactive({}) easier to read

### Sc1
sc1_rate_eqDF_ib<-bind_rows(
  build_eq_df(island="large island",a=5000,d=1000),
  build_eq_df(island="small island",a=1000,d=1000)
)

### Sc2
sc2_rate_eqDF_ib<-bind_rows(
  build_eq_df(island="near island",a=1000,d=1000),
  build_eq_df(island="distant island",a=1000,d=5000)
)

### Sc3
sc3_rate_eqDF_ib<-bind_rows(
  build_eq_df(island="large, near island",a=5000,d=1000),
  build_eq_df(island="small, distant island",a=1000,d=5000)
)


### Sc4
sc4_rate_eqDF_ib<-bind_rows(
  build_eq_df(island="large, distant island",a=5000,d=5000),
  build_eq_df(island="small, near island",a=1000,d=1000)
)
  

#### Species vs Time DF Obj=========================================================================
#build each scenario spptDF separately to make switch() in reactive({}) easier to read
### Sc1
sc1_spptDF_ib<-bind_rows(
  build_svt_static_df(isle="large island", d=1000,a=5000),
  build_svt_static_df(isle="small island", d=1000,a=1000)
  
)
  
### Sc2
sc2_spptDF_ib<-bind_rows(
  build_svt_static_df(isle="near island", d=1000,a=1000),
  build_svt_static_df(isle="distant island", d=5000,a=1000)
)

#### Sc3
sc3_spptDF_ib<-bind_rows(
  build_svt_static_df(isle="large, near island", d=1000,a=5000),
  build_svt_static_df(isle="small, distant island", d=5000,a=1000)
)

### Sc4
sc4_spptDF_ib<-bind_rows(
  build_svt_static_df(isle="large, distant island", d=5000,a=5000),
  build_svt_static_df(isle="small, near island", d=1000,a=1000)
)



#### User Guide=====================================================================================
### Background
bp_ib<-"The Theory of Island Biogeography was developed by Robert MacArthur and Edward O. Wilson
in a book of the same title published in 1967. The theory proposes that immigration rates of
organisms from a mainland to islands is a function of distance, while extinction rates of species
on islands are a function of island area. Combined, these two rates determine the equilibrium
numbers of species on islands. Although these ideas were developed using islands, their application
extends deep into conversation biology as suitable habitat of threatened species can be thought
of as a matrix of islands in unsuitable habitat."

eq_ib<-"In this Shiny mini-app, the following equations ('Chapter 14: Island Biogeography', 2016) are 
used to describe immigration and extinction:
<br>
<br><strong>colonization rate = C(s) = c * (p - s) * exp(-phi * d)</strong>
<br><strong>extinction rate = E(s) = s * exp(-ep * a)</strong>
<br>
<br>The parameters and variable are as follows:
<br>
<br>c = mean rate of colonization over all species
<br>p = total number of species in pool
<br>s = species richness on island
<br>phi = distance decay of colonization rate
<br>d = distance between island and mainland
<br>ep = epsilon = effect of area on extinction
<br>a = area of island</p></blockquote>
<br>
<em>Reference</em>
<br>Chapter 14: Island Biogeography. (2016). ECOL 4000/6000: Population & Community Ecology. University
of Georgia."


### Instructions
### Scenarios
instruct_sc_ib<-"If you choose 'scenario' under the first set of options, then you are presented with 
four different scenarios. Select one of them. This displays a schematic, which illustrates the 
island areas and their distances from the mainland; the (immigration-colonization) rate-richness
plot; and the richness-time plot. These visuals are supported by descriptive text. The two plots
are interactive in that you can hover over lines and points for more information."

### Custom specifications
instruct_cu_ib<-"If you select 'custom specifications', you are presented with a series of inputs. 
At the top of the sidebar, you are free to de-select one or more of the three visuals. Use the two
sliders to set the richness on the mainland (p) and length of time (t). Input the distance from the
mainland, d, using any number between 100 and 10,000. Choose the decay of colonization rate and
mean colonization rate over all species using the two sliders. Input the area of island 1 using
any number between 100 and 10,000. Set the effect of area on extinction using the slider. If you
would like to display a second island on the rate vs richness and rate vs time plots, select
the radio button, switch to the island 2 tab, and set the inputs using desired values. The two
plots are interactive; feel free to hover over lines and points for more information. To reset
all values, press the button below the inputs."















  