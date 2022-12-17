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
              # numericInput(inputId="num_phi1_ib",value=.0001,min=0,max=.001,
              #              label="Distance decay of colonization rate 
              #              (\u03d5; 0-.001)"),
              sliderInput(inputId="sld_phi1_ib",value=.0002,min=0,max=.001,step=.0002,
                           label="Distance decay of colonization rate 
                           (\u03d5)"),
              sliderInput(inputId="sld_c1_ib",value=0.1,min=0.1,max=1,step=0.05,
                           label="Mean colonization rate over all species (c)"),
              hr(),
              h5(strong("Extinction")),
                numericInput(inputId="num_a1_ib",value=2300,min=50,max=10000,
                             label="Area of island 1 (a; 50-10000)"),
                sliderInput(inputId="sld_ep1_ib",value=.0002,min=0,max=.001,step=.0002,
                             label="Effect of area on extinction 
                             (\u03b5)"),
                # numericInput(inputId="num_ep1_ib",value=.0001,min=0,max=.001,
                #              label="Effect of area on extinction 
                #              (\u03b5; 0-.1)"
                # ),
              hr(),
              sliderInput(inputId="sld_t_ib",value=50,min=20,max=200,step=10,
                          label="Length of time (t)"),
              hr(),
              radioButtons(inputId="rad_is2_ib",label="Display second island?",
                          choices=c("no","yes"),selected="no")
            ),
            #Island 2 ui
            tabPanel(title="Island 2",
              h5(strong("Immigration")),
              numericInput(inputId="num_d2_ib",value=100,min=0,max=10000,
                           label="d (0-10,000)"
              ),
              sliderInput(inputId="sld_phi2_ib",value=.0002,min=0,max=.001,step=.0002,
                           label="\u03d5"),
              # numericInput(inputId="num_phi2_ib",value=.0001,min=0,max=.001,
              #              label="\u03d5 (0-.001)"),
              sliderInput(inputId="sld_c2_ib",value=0.1,min=0.1,max=1,step=0.05,
                           label="c (0.1-1)"),
              hr(),
              h5(strong("Extinction")),
              numericInput(inputId="num_a2_ib",value=2300,min=50,max=10000,
                           label="a (50-10000)"),
              sliderInput(inputId="sld_ep2_ib",value=.0002,min=0,max=.001,step=.0002,
                           label="\u03b5"
              # numericInput(inputId="num_ep2_ib",value=.001,min=0,max=.1,
              #              label="\u03b5 (0-.1)"
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
# current figures:
  #1) legend for each
  #2) second line for island 2 of second plot
# ui:
  # 1) add another sidebar or wellPanel (below current one) with p, t, and display second island
    #question?
  # 2) radio button above sidebarpanel (or in #1) where you choose a scenario: large vs small,
    #close vs distant, both, or custom (which would then display the tabs--they could be hidden)
  # 3) reset button for custom mode, which would return the values back to original (shinyjs
    # tool for this)
# new output
  # 1) visual of the mainland and islands and a and d would align with the a and d inputs (or in
    # a scenario)--looks like plotly could help here

# create functions??




# DONE
# corrected typo in s vs time equation
# tidied up ranges of inputs and changed two numeric ones to sliders



# LAST COMMIT
# add second plot (spp over time) for island 1



