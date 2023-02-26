#Created by Keith Post on 12/7/22
#Shiny App for species-based models in ecology: 
  #1) Island Biogeography
  #2) Species-Area Curves
  #3) Rarefaction

#load packages
pacman::p_load(shiny,here,tidyverse,plotly,shinyjs,ggforce,sars,nlstools,broom,DT,withr,
               vegan,janitor)

#source in functions and objects
# source(here("backbone_and_functions","species_models_func_01.R"))
# source(here("backbone_and_functions","species_models_func_02.R"))
# source(here("backbone_and_functions","species_models_func_03.R"))
# source(here("backbone_and_functions","species_models_obj_01.R"))
# source(here("backbone_and_functions","species_models_obj_02.R"))
# source(here("backbone_and_functions","species_models_obj_03.R"))

purrr::map(list.files(here("backbone_and_functions"),pattern="obj|func",
               full.names=TRUE),source)


#--------------------------------------------------------------------------------------------------#
###### Define UI====================================================================================
#--------------------------------------------------------------------------------------------------#
ui<-navbarPage("Species Models App",
  useShinyjs(),
  ##### Create first navbarMenu (Island Biogeography=ib)============================================
  navbarMenu(title="Theory of Island Biogeography",
    #### App component for custom specifications----------------------------------------------------
    tabPanel(title="Mini-app",id="app_ib1",
      titlePanel("Island Biogeography Mini-App"),
      #scenario vs custom & scenarios radio buttons
      radioButtons(inputId="rad_scenario1_ib",choices=scenarios1_ib,selected=character(0),
                   inline=TRUE,label="Choose an option"),
      uiOutput("ui_rad_scenarios2_ib"),
      
      sidebarLayout(
        #name sidebarPanel for shinyjs code
        div(id="sidebar_ib",sidebarPanel(width=3,position="left",
          tabsetPanel(id="input_tabset_ib",type="hidden",
            #use a blank panel to open
            tabPanel(title="tab_blank_ib"),
            #scenario tab displays text (based on second radio button)
            tabPanel(title=scenarios1_ib[1],
              textOutput("text_sc1_4_text_ib")
            ),
            #custom panel has various inputs
            tabPanel(title=scenarios1_ib[2],
              #output selector
              h4(strong("Plots")),
                #checkboxes to select what to output
                #h5(strong()) is equivalent to a shiny input label
                h5(strong("Select which output(s) to display")), 
                checkboxInput(inputId="chk_schematicOut_ib",
                              label="schematic",
                              value=TRUE),
                #div compresses vertical spacing among checkboxes to resemble checkboxGroupInput
                div(checkboxInput(inputId="chk_rateOut_ib",
                                  label="rate plot",
                                  value=TRUE),
                    style="margin-top: -10px; margin-bottom: -10px"),
                checkboxInput(inputId="chk_svtOut_ib",
                              label="spp v time plot",
                              value=TRUE),
              hr(style = "border-top: 1px solid #000000;"),
              h4(strong("Species Pool & Time")),
                #species pool slider (above tabs)
                sliderInput(inputId="sld_p_ib",value=100,min=20,max=200,step=10,
                            label="Number of species on mainland (p)"),
                #time slider (below tabs)
                sliderInput(inputId="sld_t_ib",value=50,min=20,max=100,step=10,
                            label="Length of time (t)"),
              hr(style = "border-top: 1px solid #000000;"),
              h5(em("Adjust inputs for each island")),
            tabsetPanel(id="custom_tabset_ib",type="pills",
                        
              #Island 1 ui
              tabPanel(title="Island 1",
                h4(strong("Immigration")),
                  numericInput(inputId="num_d1_ib",value=1000,min=100,max=10000,
                               label="Distance from mainland (d; 100-10,000)"),
                  sliderInput(inputId="sld_phi1_ib",value=.0002,min=0,max=.001,step=.0002,
                               label="Distance decay of colonization rate 
                               (\u03d5)"),
                  sliderInput(inputId="sld_c1_ib",value=0.6,min=0.1,max=1,step=0.05,
                               label="Mean colonization rate over all species (c)"),
                # hr(style = "border-top: 1px solid #000000;"),
                br(),
                h4(strong("Extinction")),
                    numericInput(inputId="num_a1_ib",value=1200,min=100,max=10000,
                                 label="Area of island 1 (a; 100-10,000)"),
                    sliderInput(inputId="sld_ep1_ib",value=.0006,min=0,max=.001,step=.0002,
                                 label="Effect of area on extinction 
                                 (\u03b5)"),
              ),
              
              #Island 2 ui
              tabPanel(title="Island 2",
                h4(strong("Immigration")),
                  numericInput(inputId="num_d2_ib",value=1000,min=100,max=10000,
                               label="d (100-10,000)"),
                  sliderInput(inputId="sld_phi2_ib",value=.0002,min=0,max=.001,step=.0002,
                               label="\u03d5"),
                  sliderInput(inputId="sld_c2_ib",value=0.6,min=0.1,max=1,step=0.05,
                               label="c"),
                hr(style = "border-top: 1px solid #000000;"),
                h4(strong("Extinction")),
                  numericInput(inputId="num_a2_ib",value=1200,min=100,max=10000,
                               label="a (100-10,000)"),
                  sliderInput(inputId="sld_ep2_ib",value=.0006,min=0,max=.001,step=.0002,
                               label="\u03b5")
              )
            ),
            hr(style = "border-top: 1px solid #000000;"),
            #display second island (below tabs)
            radioButtons(inputId="rad_is2_ib",label="Display Island 2 on plots?",
                           choices=c("no","yes"),selected="no"),
            br(),
            #add reset button
            actionButton("reset_ib","Reset all values")
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
    tabPanel(title="Mini-app",id="app_sa1",
      titlePanel("Species-Area Curves Mini-App"),
      sidebarLayout(
        #name sidebarPanel for shinyjs code
        sidebarPanel(width=3,position="left",
          tabsetPanel(id="input_tabset_sa",
            tabPanel("Drawing Models",
              #output selector
              h4(strong("Plots")),
                #checkboxes to select what to output
                #h5(strong()) is equivalent to a shiny input label
                h5(strong("Select which output(s) to display")), 
                checkboxInput(inputId="chk_plline_sa",
                              label="power law: linear scale",
                              value=TRUE),
                #div compresses vertical spacing among checkboxes to resemble checkboxGroupInput
                div(checkboxInput(inputId="chk_pllog_ib",
                                  label="power law: log-log scale",
                                  value=TRUE),
                    style="margin-top: -10px; margin-bottom: -10px"),
                checkboxInput(inputId="chk_semilog_sa",
                              label="semilog model",
                              value=TRUE),
                #create inputs for drawing models
                h4(strong("Drawing models")),
                h5(em("Adjust inputs for species-area plots")),
                  div(id="drawMod_tab_sa",
                    #three simple, slider inputs: a (start and end), c, and z
                    sliderInput(inputId="sld_a_sa",value=c(-5,5),min=-10,max=10,step=1,
                                #put label on two lines
                                label=HTML("A (10^x) 
                                           <br />Select x")),
                    sliderInput(inputId="sld_c_sa",value=5,min=2,max=13,step=1,
                                label="c"),
                    sliderInput(inputId="sld_z_sa",value=.25,min=.1,max=.35,step=.05,
                                label="z")
                  ),
                  #add reset button
                  actionButton("reset_sa","Reset sliders"),
                hr(style = "border-top: 1px solid #000000;"),
              #define variables and describe models
              h4(strong("Variables")),
                 h5("S = number of species"),
                 h5("A = habitat area"),
              h4(strong("Models")),
                h5(strong("Power Law")),
                  h5("linear space: S = cA^z"),
                  h5("log-log space: log(S) = log(c) + zlog(A)"),
                h5(strong("Semilog Model")),
                  h5("S = log(c) + zlog(A)")
            ),
            tabPanel("Model Fitting",
              h4(strong("Model fitting")),
              #create inputs for fitting models
              radioButtons(inputId="rad_dataset_sa",choices=datasets_sa,selected=character(0),
                           label="Select a dataset to work with")
            )
          )
        ),
        mainPanel(
          tabsetPanel(id="out_tabset_sa",type="hidden",
            tabPanel(title=out_tab_titles_sa[1],
              splitLayout(
                plotOutput("plot_draw_plline_sa",height="350px"),
                plotOutput("plot_draw_pllog_sa",height="350px")
              ),
              br(),
              fluidRow(
                column(width=12,align="center",
                  plotOutput("plot_draw_semilog_sa",height="350px",width="80%") 
                )
              )
            ),
            tabPanel(title=out_tab_titles_sa[2],
              splitLayout(
                # plotOutput("plotly_datmod_plline_sa",height="350px"),
                plotlyOutput("plotly_datamod_pllog_sa",height="350px"),
                plotlyOutput("plotly_datamod_semilog_sa",height="350px")
              ),
              br(),
              fluidRow(
                DTOutput("dt_modcomp_sa")
              )
            )
          )
        )
      )
    ),
  
    #### User guide component-----------------------------------------------------------------------
    tabPanel(title="User Guide",id="guide_sa2"
    )
  ),
  
  ##### Create third navbarMenu (Rarefaction=rf)====================================================
  navbarMenu(title="Rarefaction",
    #### App component------------------------------------------------------------------------------
    tabPanel(title="Mini-app",id="app_rf1",
      titlePanel(title="Rarefaction Mini-App"),
      sidebarLayout(
        sidebarPanel(width=3,position="left",
          tabsetPanel(id="input_tabset_rf",
            tabPanel("Collector's Curves",
              radioButtons(inputId="rad_dataset_rf",choices=datasets_rf,select=character(0),
                           label="Select a dataset"),
              div(id="sidebar_cc_rf",
                sliderInput(inputId="sld_r_rf",value=10,min=5,max=15,step=1,
                            label="Choose number of sites (r)"),
                radioButtons(inputId="rad_specaccumtype_rf",choices=specaccum_curves_rf,
                             label="Select species accumulation curve method"),
                br(),
                h4(strong("Outputs")),
                  checkboxInput(inputId="chk_specaccumplot_rf",
                                label="Species accumulation curve",
                                value=TRUE),
                  checkboxInput(inputId="chk_estTotS_rf",
                                label="Expected species richness")
              )
            ),
            tabPanel("Rarefaction",
              radioButtons(inputId="rad_dataset2_rf",choices=datasets2_rf,select=character(0),
                           label="Select a dataset"),
              div(id="sidebar_rare_rf",
                sliderInput(inputId="sld_r2_rf",value=5,min=2,max=10,step=1,
                            label="Choose number of sites (r)"),
                sliderInput(inputId="sld_n_rf",value=10,min=5,max=15,step=1,
                            label="Choose number of subsamples (n)"),         
                  ))
            
          )
        ),
        mainPanel(
          tabsetPanel(id="out_tabset_rf",type="hidden",
            tabPanel(title=out_tab_titles_rf[1],
              plotlyOutput("plotly_specaccum_rf"),
              br(),
              fluidRow(
                column(2),
                column(align="center",width=8,
                  DTOutput("dt_estTotS_rf")
                ),
                column(2),
              )
            ),
            tabPanel(title=out_tab_titles_rf[2],
            )
                    
          )
        )
      )
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
# have dynamic slider for # of subsamples: varies by dataset
# signif() richness on spec accum curve
# add rarefaction by individuals (????)

# rarecurve visual -> turn hard code into function
# grouped boxplots? inds & species?



#LATER
#IB
#add pictures of MacArthur and Wilson (perhaps in user guide)
#add text to explain scenarios 
#change plot dimensions

#SA
#center reset button
#if plots of data with model do not have legends, then add them



# DONE




# LAST COMMIT
# updated radioButton choices to remove sipoo if rarefaction selected
# made static slider for # of sites: 2-10 with value of 5
# updated UI for rarefaction simulations, including dynamic slider for subsamples


