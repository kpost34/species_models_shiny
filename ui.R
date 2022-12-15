#Created by Keith Post on 12/7/22
#Shiny App for species-based models in ecology: 
  #1) Island Biogeography
  #2) Species-Area Curves
  #3) Rarefaction

#load packages
pacman::p_load(shiny,here,tidyverse,plotly)

#source in functions




#### Create Objects=================================================================================

### Second modeling approach taking distance (C) and area (E) into account
## New formulas
# C(s) = c(p-s)*exp(-phi*d)
# E(s) = s*exp^(-ep*a)

# s* = (cp*exp(ep*a))/(c*exp(ep*a) + exp(phi*d))


#s, c, p, phi, d
#phi: .0001, .005, .001, .05, .01
#s, ep, a
#t



#--------------------------------------------------------------------------------------------------#
###### Define UI====================================================================================
#--------------------------------------------------------------------------------------------------#
ui<-navbarPage("Species Models App",
  ##### Create first navbarMenu (Island Biogeography=ib)============================================
  navbarMenu(title="Theory of Island Biogeography",
    #### App component------------------------------------------------------------------------------
    tabPanel(title="Island Biogeography Simulator",id="app_ib1",
      sidebarLayout(
        sidebarPanel(width=3,position="left",
          tabsetPanel(id="app_tabset_ib",type="pills",
            #Island 1 ui
            tabPanel(title="Island 1",
              h5(strong("Immigration")),
              sliderInput(inputId="sld_p1_ib",value=100,min=20,max=200,step=10,
                           label="Number of species on mainland (p)"
              ),
              numericInput(inputId="num_d1_ib",value=100,min=0,max=10000,
                           label="Distance from mainland (d; 0-10,000)"
              ),
              numericInput(inputId="num_phi1_ib",value=.0001,min=0,max=.01,
                           label="Distance decay of colonization rate 
                           (\u03d5; 0-.01)"),
              sliderInput(inputId="sld_c1_ib",value=0.1,min=0.1,max=1,step=0.05,
                           label="Mean colonization rate over all species in pool (c)"),
              br(),
              h5(strong("Extinction")),
                numericInput(inputId="num_a1_ib",value=2300,min=50,max=10000,
                             label="Area of island 1 (a; 50-10000)"),
                numericInput(inputId="num_ep1_ib",value=.001,min=0,max=.1,
                             label="Effect of area on extinction 
                             (\u03b5; 0-.1)"
                ),
              br(),
              sliderInput(inputId="sld_t_ib",value=50,min=20,max=200,step=10,
                          label="Length of time (t)"),
              br(),
             radioButtons(inputId="rad_is2_ib",label="Display second island?",
                          choices=c("no","yes"),selected="no")
            ),
            #Island 2 ui
            tabPanel(title="Island 2",
              h5(strong("Immigration")),
              numericInput(inputId="num_d2_ib",value=100,min=0,max=10000,
                           label="d (0-10,000)"
              ),
              numericInput(inputId="num_phi2_ib",value=.0001,min=0,max=.01,
                           label="\u03d5 (0-.01)"),
              sliderInput(inputId="sld_c2_ib",value=0.1,min=0.1,max=1,step=0.05,
                           label="c (0.1-1)"),
              br(),
              h5(strong("Extinction")),
              numericInput(inputId="num_a2_ib",value=2300,min=50,max=10000,
                           label="a (50-10000)"),
              numericInput(inputId="num_ep2_ib",value=.001,min=0,max=.1,
                           label="\u03b5 (0-.1)"
              )
            )
          )
        ),
        mainPanel(width=9,
          plotlyOutput("plotly_rate_ib"),
          plotlyOutput("plotly_spp_ib")
        )
      )         
    ),
  
    
    #### Theory and Reach---------------------------------------------------------------------------
    tabPanel(title="Theory and Reach",id="theory_reach_ib2"),
    #### User guide component-----------------------------------------------------------------------
    tabPanel(title="User Guide",id="guide_ib3"
    )
  ),
  
  
  ##### Create second navbarMenu (Species-Area Curves=sa)===========================================
  navbarMenu(title="Species-Area Curves",
    #### App component------------------------------------------------------------------------------
    tabPanel(title="Species-Area Curve Generator",id="app_sa1",
    ),
  
    #### User guide component-----------------------------------------------------------------------
    tabPanel(title="User Guide",id="guide_sa2"
    )
  ),
  
  ##### Create third navbarMenu (Rarefaction=rf)====================================================
  navbarMenu(title="Rarefaction",
    #### App component------------------------------------------------------------------------------
    tabPanel(title="Rarefaction Simulator",id="app_rf1"
    ),
  
    #### User guide component-----------------------------------------------------------------------
    tabPanel(title="User Guide",id="guide_rf2"
    )
  ),
  
  #### Create tabPanel (developer info)=============================================================
  tabPanel(title="Developer Info")             
               
               
               
)













#### NOTES==========================================================================================

# NEXT
# rounded end to line that extends to x-axis
# make line dashed
# label S* = [value]
# make S* toggle-able
# add line 2 graph
# reset buttons



# DONE
# fixed intersection point & added appropriate labels
# set up controls and display for second island



# LAST COMMIT
# Added intersection point of rate graphs with vertical line that extends to x-axis




