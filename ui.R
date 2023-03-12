#Created by Keith Post on 12/7/22
#Shiny App for species-based models in ecology: 
  #1) Island Biogeography
  #2) Species-Area Relationships
  #3) Rarefaction

#load packages
pacman::p_load(shiny,here,tidyverse,plotly,shinyjs,ggforce,sars,nlstools,broom,DT,withr,
               vegan,janitor,shinythemes)

#source in functions and objects
purrr::map(list.files(here("backbone_and_functions"),pattern="obj|func",
               full.names=TRUE),source)


#--------------------------------------------------------------------------------------------------#
###### Define UI====================================================================================
#--------------------------------------------------------------------------------------------------#
ui<-navbarPage("Species Models App",
  theme=shinytheme("cerulean"),
  useShinyjs(),
  
  ##### Island Biogeography=========================================================================
  navbarMenu(title="Theory of Island Biogeography",
    #### App component for custom specifications----------------------------------------------------
    tabPanel(title="Mini-app",id="app_ib1",
      titlePanel("Island Biogeography Mini-App"),
      #scenario vs custom & scenarios radio buttons
      radioButtons(inputId="rad_scenario1_ib",choices=scenarios1_ib,selected=character(0),
                   inline=TRUE,label=h4("Choose an option")),
      uiOutput("ui_rad_scenarios2_ib"),
      
      sidebarLayout(
        #name sidebarPanel for shinyjs code
        div(id="sidebar_ib",
          sidebarPanel(width=3,position="left",
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
                #add centered reset button
                fluidRow(align="center",
                  actionButton("reset_ib","Reset all values")
                )
              )
            )
          )
        ),
      
        mainPanel(width=9,
          #output tabset object
          out_tabs_ib
        )
      ),
      #display scenario plots
      div(id="scenarios1_plots_ib",
        linebreaks(3),
        fluidRow(
          column(6,
            plotlyOutput("plotly_sc1_4_rate_ib",
                         width="90%",
                         height="400px")
            ),
          column(6,
            plotlyOutput("plotly_sc1_4_sppt_ib",
                         width="90%",
                         height="400px")
          )
        )
      )
    ),
  
    #### User guide component-----------------------------------------------------------------------
    tabPanel(title="User Guide",id="guide_ib2",
      div(titlePanel("Island Biogeography Mini-App"),
        h3(strong("Background")),
          bp_ib,
        linebreaks(2),
        h3(strong("Equations")),
          HTML(eq_ib),
        linebreaks(2),
        h3(strong("Instructions")),
          h4("Scenarios"),
            instruct_sc_ib,
          linebreaks(1.5),
          h4("Custom specifications"),
            instruct_cu_ib,
        #add left and rights margins
        style="margin-left:200px; margin-right: 200px"
      )
    )
  ),
  
  
  ##### Species-Area Curves=========================================================================
  navbarMenu(title="Species-Area Relationships",
    #### App component------------------------------------------------------------------------------
    tabPanel(title="Mini-app",id="app_sa1",
      titlePanel("Species-Area Relationships Mini-App"),
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
                                  label="power function: log-log scale",
                                  value=TRUE),
                    style="margin-top: -10px; margin-bottom: -10px"),
                checkboxInput(inputId="chk_semilog_sa",
                              label="semilog model",
                              value=TRUE),
                #create inputs for drawing models
                h4(strong("Drawing models")),
                h5(em("Adjust inputs for species-area plots")),
                  div(id="drawMod_tab_sa",
                    #three simple slider inputs: a (start and end), c, and z
                    sliderInput(inputId="sld_a_sa",value=c(-5,5),min=-10,max=10,step=1,
                                #put label on two lines
                                label=HTML("A (10^x) 
                                           <br />Select x")),
                    sliderInput(inputId="sld_c_sa",value=5,min=2,max=13,step=1,
                                label="c"),
                    sliderInput(inputId="sld_z_sa",value=.25,min=.1,max=.35,step=.05,
                                label="z")
                  ),
                  #add centered reset button
                  fluidRow(align="center",
                    actionButton("reset_sa","Reset sliders")
                  ),
                hr(style = "border-top: 1px solid #000000;"),
              #define variables and describe models
              h4(strong("Variables")),
                 h5("S = number of species"),
                 h5("A = habitat area"),
              h4(strong("Models")),
                h5(strong("Power Function")),
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
              #to more easily compare models
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
                #again, more easily compare models
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
    tabPanel(title="User Guide",id="guide_sa2",
      div(titlePanel("Species-Area Relationships Mini-App"),
        h3(strong("Background")),
          bp_sa,
        linebreaks(2),
        h3(strong("Instructions")),
          h4("Drawing Models"),
            instruct_dm_sa,
          linebreaks(1.5),
          h4("Model Fitting"),
            instruct_mf_sa,
        #add left and rights margins
        style="margin-left:200px; margin-right: 200px"
      )
    )
  ),

  
  ##### Rarefaction=================================================================================
  navbarMenu(title="Rarefaction",
    #### App component------------------------------------------------------------------------------
    tabPanel(title="Mini-app",id="app_rf1",
      titlePanel(title="Rarefaction Mini-App"),
      sidebarLayout(
        sidebarPanel(width=3,position="left",
          tabsetPanel(id="input_tabset_rf",
            #Species accumulation UI
            tabPanel("Spp Accumulation",
              radioButtons(inputId="rad_dataset_rf",choices=datasets_rf,select=character(0),
                           label="Select a dataset"),
              div(id="sidebar_cc_rf",
                sliderInput(inputId="sld_r_rf",value=10,min=5,max=15,step=1,
                            label="Choose number of sites (r)"),
                radioButtons(inputId="rad_specaccumtype_rf",choices=sac_rf,
                             label="Select species accumulation curve method"),
                h4(strong("Outputs")),
                  checkboxInput(inputId="chk_specaccumplot_rf",
                                label="Species accumulation curve",
                                value=TRUE),
                  checkboxInput(inputId="chk_estTotS_rf",
                                label="Expected species richness")
              )
            ),
            #Rarefaction UI
            tabPanel("Rarefaction",
              radioButtons(inputId="rad_dataset2_rf",choices=datasets2_rf,select=character(0),
                           label="Select a dataset"),
              div(id="sidebar_rare_rf",
                sliderInput(inputId="sld_r2_rf",value=4,min=2,max=6,step=1,
                            label="Choose number of sites (r)"),
                sliderInput(inputId="sld_n_rf",value=10,min=5,max=15,step=1,
                            label="Choose number of subsamples (n)"),         
                  ))
            
          )
        ),
        mainPanel(
          tabsetPanel(id="out_tabset_rf",type="hidden",
            #species-accumulation visuals
            tabPanel(title=out_tab_titles_rf[1],
              column(12,align="center",
                plotlyOutput("plotly_specaccum_rf",
                             height="400px",
                             width="90%"),
                br(),
                DTOutput("dt_estTotS_rf",
                         width="60%")
              )
            ),
            #rarefaction visuals
            tabPanel(title=out_tab_titles_rf[2],
              column(12,align="center",
                plotlyOutput("plotly_rare_curve_rf",
                           height="400px",
                           width="90%"),
                br(),
                DTOutput("dt_rarefac_rf",
                         width="90%")
              )
            )
          )
        )
      )
    ),
      
  
    #### User guide component-----------------------------------------------------------------------
    tabPanel(title="User Guide",id="guide_rf2",
      div(titlePanel(title="Rarefaction Mini-App"),
        h3(strong("Background")),
          h4("Species-Area Curves"),
            bp_sac_rf,
          linebreaks(1.5),
          h4("Rarefaction"),
            bp_rare_rf,
        linebreaks(2),
        h3(strong("Instructions")),
          h4("Species-Area Curves"),
            instruct_sac_rf,
          linebreaks(1.5),
          h4("Rarefaction"),
            instruct_rare_rf,
        #add left and rights margins
        style="margin-left:200px; margin-right: 200px"
      )
    )
  ),
  
  #### Developer Info==========================================================================
  tabPanel(title="Developer Info",
    fluidRow(
      column(5,
        wellPanel(width="50%",
          p(h4(strong("Keith Post"))),
          p("If you would like to see the code for this Shiny app, please visit the",
           tags$a(href="https://github.com/kpost34/species_models_shiny",
                  "Github repo"),
           "for this project."
          ),
          p(tags$a(href="https://github.com/kpost34","GitHub Profile")),
          p(tags$a(href="https://www.linkedin.com/in/keith-post","LinkedIn"))
        )
      )
    )
  ),
  #added to remove flashing warning/error regarding 'rarefaction'-based spp accumulation curve
  tags$style(type="text/css",
         ".shiny-output-error { visibility: hidden; }",
         ".shiny-output-error:before { visibility: hidden; }"
  )             
               
               
)



#### NOTES==========================================================================================





# DONE

 


# LAST COMMIT
# improved line-spacing in user guides
# adjusted plot sizes
# suppressed warning on app itself (as I couldn't resolve it)


