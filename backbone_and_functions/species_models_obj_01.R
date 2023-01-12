#Created by Keith Post on 1/4/23
#Code for objects used in Species Models App for Island Biogeography mini-app



#### Scenario options===============================================================================
scenarios1_ib<-c("scenario (e.g., large vs small islands)"="scenario",
                 "custom specifications" = "custom")



scenarios2_ib<-c("large vs small islands"="lvs",
                "near vs distant islands"="nvd",
                "large, near vs small, distant islands"="lnvsd",
                "large, distant vs small, near islands"="ldvsn")




#### Output Tabset Object===========================================================================
out_tabs_ib<-tabsetPanel(id="out_tabset_ib",type="hidden",
              #apps opens with blank panel
              tabPanel("tab_blank_ib"),
              #if scenario selected, plots for specific one (1-4) outputted
              tabPanel(scenarios1_ib[1],
                plotOutput("plot_sc1_4_schematic_ib"),
                br(),
                plotlyOutput("plotly_sc1_4_rate_ib"),
                plotlyOutput("plotly_sc1_4_sppt_ib")
              ),
              #custom model yields plots from custom inputs
              tabPanel(scenarios1_ib[2],
                plotOutput("plot_cust_schematic_ib"),
                br(),
                plotlyOutput("plotly_cust_rate_ib"),
                plotlyOutput("plotly_cust_sppt_ib")
              )
            )

#### Text Sidebar Obj===============================================================================
sc1_text_ib<-"Scenario 1 text"
sc2_text_ib<-"Scenario 2 text"
sc3_text_ib<-"Scenario 3 text"
sc4_text_ib<-"Scenario 4 text"



#### Rate DF Obj====================================================================================
### Sc1
sc1_rateDF_ib <- bind_rows(
  build_rate_static_df(island="small island",d=1000,a=1000,rate="Extinction"),
  build_rate_static_df(island="large island",d=1000,a=5000,rate="Extinction"),
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



#### Equilibrium Rate DF Obj=========================================================================
### Sc1
sc1_rate_eqDF_ib<-bind_rows(
  build_eq_df(island="small island",a=1000,d=1000),
  build_eq_df(island="large island",a=5000,d=1000)
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
  

#### Species vs Time DF Obj==========================================================================
### Sc1
sc1_spptDF_ib<-bind_rows(
  build_svt_static_df(isle="small island", d=1000,a=1000),
  build_svt_static_df(isle="large island", d=1000,a=5000)
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





  