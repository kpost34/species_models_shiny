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
          h3(strong("Island 1")),
            h4(strong("Immigration")),
              h5("Please enter the following parameters"),
                sliderInput(inputId="sld_p_ib",value=50,min=20,max=200,step=10,
                             label="Number of species on mainland (p)"
                ),
                numericInput(inputId="num_d_ib",value=50,min=0,max=10000,
                             label="Distance between island and mainland (d; 0-10,000)"
                ),
                numericInput(inputId="num_phi_ib",value=.001,min=.0001,max=.01,
                             label="Fit parameter that governs distance decay of colonization rate 
                             (phi; .0001-.01)"),
                sliderInput(inputId="sld_c_ib",value=0.5,min=0.1,max=1,step=0.05,
                             label="Mean colonization rate over all species in pool (c)"),
              br(),
              h5("Extinction Parameters"),
                numericInput(inputId="num_a_ib",value=1000,min=100,max=50,000,
                             label="Area of island 1 (a; 100-50000)"),
                numericInput(inputId="num_ep_ib",value=.01,min=.0001,max=.1,
                             label="Fit parameter governing effect of area on extinction"
                ),
              br(),
              sliderInput(inputId="sld_t_ib",value=50,min=20,max=200,step=10,
                          label="Units of time (t)"),
              br(),
              radioButtons(inputId="rad_2isle_ib",selected="no",inline=TRUE,
                           choices=c("no","yes"),
                           label="Add second island?"
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






#--------------------------------------------------------------------------------------------------#
##### Define Server Function========================================================================
#--------------------------------------------------------------------------------------------------#
server<-function(input,output,session){
  
}





# Run the application
shinyApp(ui=ui,server=server)





#### NOTES==========================================================================================

# NEXT



# DONE



# LAST COMMIT
# fleshed out backbone code for ToIB curves
# created skeleton for app
# began creating inputs





