#Created by Keith Post on 12/7/22
#Shiny App for species-based models in ecology: 
  #1) Island Biogeography
  #2) Species-Area Curves
  #3) Rarefaction

#load packages
pacman::p_load(shiny,here,tidyverse,plotly,shinyjs)

#source in functions and objects
source(here("backbone_and_functions","species_models_func_01.R"))
source(here("backbone_and_functions","species_models_obj_01.R"))




#--------------------------------------------------------------------------------------------------#
###### Define UI====================================================================================
#--------------------------------------------------------------------------------------------------#
ui<-navbarPage("Species Models App",
  useShinyjs(),
  ##### Create first navbarMenu (Island Biogeography=ib)============================================
  navbarMenu(title="Theory of Island Biogeography",
    #### App component for custom specifications----------------------------------------------------
    tabPanel(title="Island Biogeography Simulator",id="app_ib1",
      #scenario vs custom & scenarios radio buttons
      radioButtons(inputId="rad_scenario1_ib",choices=scenarios1_ib,selected=character(0),
                   inline=TRUE,label="Choose an option"),
      uiOutput("ui_rad_scenarios2_ib"),
      
      sidebarLayout(
        #name sidebarPanel for shinyjs code
        div(id="Sidebar",sidebarPanel(width=3,position="left",
          tabsetPanel(id="input_tabset_ib",type="hidden",
            #use a blank panel to open
            tabPanel(title="tab_blank_ib"),
            #scenario tab displays text (based on second radio button)
            tabPanel(title=scenarios1_ib[1],
              textOutput("text_sc1_4_text_ib")
            ),
            #custom panel has various inputs
            tabPanel(title=scenarios1_ib[2],
              #species pool slider (above tabs)
              sliderInput(inputId="sld_p_ib",value=100,min=20,max=200,step=10,
                          label="Number of species on mainland (p)"
              ),
            hr(),
            
            tabsetPanel(id="custom_tabset_ib",type="pills",
                        
              #Island 1 ui
              tabPanel(title="Island 1",
                h5(strong("Immigration")),
                numericInput(inputId="num_d1_ib",value=1000,min=0,max=10000,
                             label="Distance from mainland (d; 0-10,000)"),
                sliderInput(inputId="sld_phi1_ib",value=.0002,min=0,max=.001,step=.0002,
                             label="Distance decay of colonization rate 
                             (\u03d5)"),
                sliderInput(inputId="sld_c1_ib",value=0.6,min=0.1,max=1,step=0.05,
                             label="Mean colonization rate over all species (c)"),
                hr(),
                h5(strong("Extinction")),
                  numericInput(inputId="num_a1_ib",value=1200,min=50,max=10000,
                               label="Area of island 1 (a; 50-10000)"),
                  sliderInput(inputId="sld_ep1_ib",value=.0006,min=0,max=.001,step=.0002,
                               label="Effect of area on extinction 
                               (\u03b5)"),
                hr(),
              ),
              
              #Island 2 ui
              tabPanel(title="Island 2",
                h5(strong("Immigration")),
                numericInput(inputId="num_d2_ib",value=1000,min=0,max=10000,
                             label="d (0-10,000)"),
                sliderInput(inputId="sld_phi2_ib",value=.0002,min=0,max=.001,step=.0002,
                             label="\u03d5"),
                sliderInput(inputId="sld_c2_ib",value=0.6,min=0.1,max=1,step=0.05,
                             label="c"),
                hr(),
                h5(strong("Extinction")),
                numericInput(inputId="num_a2_ib",value=1200,min=50,max=10000,
                             label="a (50-10000)"),
                sliderInput(inputId="sld_ep2_ib",value=.0006,min=0,max=.001,step=.0002,
                             label="\u03b5")
              )
            ),
            hr(),
            #time slider (below tabs)
            sliderInput(inputId="sld_t_ib",value=50,min=20,max=100,step=10,
                          label="Length of time (t)"),
            #display second island (below tabs)
            radioButtons(inputId="rad_is2_ib",label="Display second island on plots?",
                           choices=c("no","yes"),selected="no"),
            br(),
            #add reset button
            actionButton("reset_ib","Reset to initial values")
            )
          )
        )
      ),
      
      mainPanel(width=9,
        #output tabset object
        out_tabs_ib
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
# new output
  # 1) visual of the mainland and islands and a and d would align with the a and d inputs (or in
    # a scenario)--looks like plotly could help here
  #2) add pictures of MacArthur and Wilson



# DONE



# LAST COMMIT
# fixed switch issue for scenarios
# got app code to display rate and spp v time plots for all 4 scenarios and custom specs
# added functioning reset button




