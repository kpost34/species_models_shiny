#Created by Keith Post on 12/11/22





#--------------------------------------------------------------------------------------------------#
##### Define Server Function========================================================================
#--------------------------------------------------------------------------------------------------#
server<-function(input,output,session){
  

  ##### Island Biogeography (=ib)===================================================================
  #### UI-------------------------------------------------------------------------------------------
  ### Load IB mini-app with sidebar hidden
  hide("sidebar_ib")
  
  ### Show sidebar if custom or specific scenario is selected and hide otherwise
  observeEvent(input$rad_scenario1_ib,{
    if(input$rad_scenario1_ib=="custom"){
      show("sidebar_ib")
    }
  })
  
  observeEvent(input$rad_scenario2_ib,{
    show("sidebar_ib")
  })
  
  observeEvent(input$rad_scenario1_ib,{
    if(input$rad_scenario1_ib=="scenario"){
      hide("sidebar_ib")
    }
  })
  
  
  ### Dynamically display second set of radio buttons
  output$ui_rad_scenarios2_ib<-renderUI({
    req(input$rad_scenario1_ib=="scenario")
    radioButtons(inputId="rad_scenario2_ib",choices=scenarios2_ib,selected=character(0),
                 inline=TRUE,label=h4("Select a scenario"))
    })
  
  
  ### Dynamically display sidebar and main panel
  observeEvent(input$rad_scenario1_ib,{
    updateTabsetPanel(inputId="input_tabset_ib",selected=input$rad_scenario1_ib)
  })
  
  observeEvent(input$rad_scenario1_ib, {
    updateTabsetPanel(inputId="out_tabset_ib",selected=input$rad_scenario1_ib)
  })
  
  
  ### Reset button
  #resets all values in sidebar (custom UI panel)
  observeEvent(input$reset_ib, {
    reset("sidebar_ib")
  })
  
  
  ### Toggle plot outputs of custom settings
  observeEvent(input$chk_schematicOut_ib,{
    toggle("plot_cust_schematic_ib")
  },ignoreInit=TRUE)

  observeEvent(input$chk_rateOut_ib,{
    toggle("plotly_cust_rate_ib")
  },ignoreInit=TRUE)
  
  observeEvent(input$chk_svtOut_ib,{
    toggle("plotly_cust_sppt_ib")
  },ignoreInit=TRUE)

  
  ### User feedback
  ## Stores feedback warning as reactive if value outside range entered
  d1_range_ib<-reactive({
    in_range<-between(input$num_d1_ib,500,10000)
    shinyFeedback::feedbackWarning("num_d1_ib",!in_range,"Please select a number within range")
    req(in_range,cancelOutput=TRUE)
    input$num_d1_ib
  })
  
  a1_range_ib<-reactive({
    in_range<-between(input$num_a1_ib,500,10000)
    shinyFeedback::feedbackWarning("num_a1_ib",!in_range,"Please select a number within range")
    req(in_range)
    input$num_a1_ib
  })
    
  d2_range_ib<-reactive({
    in_range<-between(input$num_d2_ib,500,10000)
    shinyFeedback::feedbackWarning("num_d2_ib",!in_range,"Please select a number within range")
    req(in_range)
    input$num_d2_ib
  })
      
  a2_range_ib<-reactive({
    in_range<-between(input$num_a2_ib,500,10000)
    shinyFeedback::feedbackWarning("num_a2_ib",!in_range,"Please select a number within range")
    req(in_range)
    input$num_a2_ib
  })
  
  
  ## Outputs feedback warning (if num outside range)
  output$text_d1_out_range_ib<-renderText({
    req(is.character(d1_range_ib()))
    d1_range_ib()
  })
  
  output$text_a1_out_range_ib<-renderText({
    req(is.character(a1_range_ib()))
    a1_range_ib()
  })
    
  output$text_d2_out_range_ib<-renderText({
    req(is.character(d2_range_ib()))
    d2_range_ib()
  })
      
  output$text_a2_out_range_ib<-renderText({
    req(is.character(a2_range_ib()))
    a2_range_ib()
  })
  


  
  ##### Back-end------------------------------------------------------------------------------------
  #### Scenarios
  ### Set up switches for text sidebar, schematic DF, rate DF, eq rate DF, & spp v time DF to create reactive objects
  ## Text sidebar
  #display different text in sidebar when sc1-4 is selected
  sc1_4_text_ib<-reactive({
    req(input$rad_scenario1_ib==scenarios1_ib[1])
    req(input$rad_scenario2_ib)
    switch(input$rad_scenario2_ib,
      "lvs"=sc1_text_ib,
      "nvd"=sc2_text_ib,
      "lnvsd"=sc3_text_ib,
      "ldvsn"=sc4_text_ib)
  })
  
  
  ## Schematic DF
  #build reactive DF for schematic plot depending on sc1-4
  sc1_4_schematicDF_ib<-reactive({
    req(input$rad_scenario1_ib==scenarios1_ib[1])
    req(input$rad_scenario2_ib)
    switch(input$rad_scenario2_ib,
      "lvs"=build_schematic_df(nm=c("large","small"),a1=5000,d1=1000,sec_isle="yes",a2=1000,d2=1000),
      "nvd"=build_schematic_df(nm=c("near","distant"),a1=1000,d1=1000,sec_isle="yes",a2=1000,d2=5000),
      "lnvsd"=build_schematic_df(nm=c("large, near","small, distant"),
                                 a1=5000,d1=1000,sec_isle="yes",a2=1000,d2=5000),
      "ldvsn"=build_schematic_df(nm=c("large, distant","small, near"),
                                 a1=5000,d=5000,sec_isle="yes",a2=1000,d2=1000)
    )
  })
  
  ## Rate DF
  #build reactive DF for rate plot depending on sc1-4 
  sc1_4_rateDF_ib<-reactive({
    req(input$rad_scenario1_ib==scenarios1_ib[1])
    req(input$rad_scenario2_ib)
    switch(input$rad_scenario2_ib,
           "lvs"=sc1_rateDF_ib,
           "nvd"=sc2_rateDF_ib,
           "lnvsd"=sc3_rateDF_ib,
           "ldvsn"=sc4_rateDF_ib)
  })
  
  
  ## Eq Rate DF
  #same as directly above except for equilibrium points of rate plot
  sc1_4_rate_eqDF_ib<-reactive({
    req(input$rad_scenario1_ib==scenarios1_ib[1])
    req(input$rad_scenario2_ib)
    switch(input$rad_scenario2_ib,
           "lvs"=sc1_rate_eqDF_ib,
           "nvd"=sc2_rate_eqDF_ib,
           "lnvsd"=sc3_rate_eqDF_ib,
           "ldvsn"=sc4_rate_eqDF_ib)
  })
  
  
  ## Spp vs Time DF
  #build spp v time DF reactive object for sc1-4 depending on selection
  sc1_4_spptDF_ib<-reactive({
    req(input$rad_scenario1_ib==scenarios1_ib[1])
    req(input$rad_scenario2_ib)
    switch(input$rad_scenario2_ib,
           "lvs"=sc1_spptDF_ib,
           "nvd"=sc2_spptDF_ib,
           "lnvsd"=sc3_spptDF_ib,
           "ldvsn"=sc4_spptDF_ib)
  })
  
  
  ### Render functions to output text and plots
  ## Text
  output$text_sc1_4_text_ib<-renderText({
    sc1_4_text_ib()
  })
  
  
  ## Schematic Plot
  output$plot_sc1_4_schematic_ib<-renderPlot({
    req(input$rad_scenario2_ib)
    make_island_schematic(sc1_4_schematicDF_ib(),sec_isle="yes")
  })
  
  
  ## Rate Plotly Plot
  output$plotly_sc1_4_rate_ib<-renderPlotly({
    req(input$rad_scenario2_ib)
    build_rate_static_plot(rate_data=sc1_4_rateDF_ib(),eq_data=sc1_4_rate_eqDF_ib())
  })
  
  
  ## Spp v Time Plotly Plot
  output$plotly_sc1_4_sppt_ib<-renderPlotly({
    req(input$rad_scenario2_ib)
    build_svt_static_plot(data=sc1_4_spptDF_ib())
  })
  


  #### Custom specifications
  ### Schematic plot
  ## Create reactive object of schematic island DF
  schematicDF_ib<-reactive({
    build_schematic_df(nm=c("island 1","island 2"),a1=a1_range_ib(),d1=d1_range_ib(),
                       sec_isle=input$rad_is2_ib,a2=a2_range_ib(),d2=d2_range_ib())
  })
  
  
  ## Render plot
  output$plot_cust_schematic_ib<-renderPlot({
    make_island_schematic(schematicDF_ib(),sec_isle=input$rad_is2_ib)
  })
  
  
  ### Rate plot
  ## Island 1
  # Create reactive object of rate DF
  rate1DF_ib<-reactive({
    build_rate_df(island="Island 1",s_len=300,c=input$sld_c1_ib, p=input$sld_p_ib, phi=input$sld_phi1_ib,
                  d=d1_range_ib(), ep=input$sld_ep1_ib, a=a1_range_ib())
  })


  # Create reactive object of eq rate DF
  rate_eq1DF_ib<-reactive({
    build_eq_df(island="Island 1",c=input$sld_c1_ib, p=input$sld_p_ib, phi=input$sld_phi1_ib,
                d=d1_range_ib(), ep=input$sld_ep1_ib, a=a1_range_ib())
  })



  ## Island 2
  # Create separate reactive object of rate DF (for island 2)
  rate2DF_ib<-reactive({
    build_rate_df(island="Island 2",s_len=300,c=input$sld_c2_ib, p=input$sld_p_ib, phi=input$sld_phi2_ib,
                d=d2_range_ib(), ep=input$sld_ep2_ib, a=a2_range_ib())
  })


  # Create reactive object of eq rate DF (for island 2)
  rate_eq2DF_ib<-reactive({
    build_eq_df(island="Island 2",c=input$sld_c2_ib, p=input$sld_p_ib, phi=input$sld_phi2_ib,
                d=d2_range_ib(), ep=input$sld_ep2_ib, a=a2_range_ib())
  })



  ## Render plotly plot
  output$plotly_cust_rate_ib<-renderPlotly({
    build_rate_plot(data1a=rate1DF_ib(),data1b=rate_eq1DF_ib(),sec_isle=input$rad_is2_ib,
                    data2a=rate2DF_ib(),data2b=rate_eq2DF_ib())
  })



  ### Species over time plot
  ## Island 1
  # Reactive object
  spp1tDF_ib<-reactive({
    build_svt_df(isle=1, t=input$sld_t_ib, c=input$sld_c1_ib, p=input$sld_p_ib,
                 phi=input$sld_phi1_ib, d=d1_range_ib(), ep=input$sld_ep1_ib, a=a1_range_ib())
  })



  ## Island 2
  # Reactive object
  spp2tDF_ib<-reactive({
    build_svt_df(isle=2,t=input$sld_t_ib, c=input$sld_c2_ib, p=input$sld_p_ib,
                 phi=input$sld_phi2_ib, d=d2_range_ib(), ep=input$sld_ep2_ib, a=a2_range_ib())
  })


  ## Render plotly plot
  output$plotly_cust_sppt_ib<-renderPlotly({
    build_svt_plot(data1=spp1tDF_ib(), sec_isle=input$rad_is2_ib, data2=spp2tDF_ib())
  })
  
  

  
  
  
  ##### Species-Area Relationships (=sa)===================================================================
  #### UI-------------------------------------------------------------------------------------------
  ### Dynamically display mainPanel tab based on sidebarPanel tab
  observeEvent(input$input_tabset_sa,{
    updateTabsetPanel(inputId="out_tabset_sa",selected=input$input_tabset_sa)
  })
  
  
  
  ### Reset button
  #resets all values in sidebar
  observeEvent(input$reset_sa, {
    reset("drawMod_tab_sa")
  })
  
  
  ### Toggle plot outputs of custom settings
  observeEvent(input$chk_plline_sa,{
    toggle("plot_draw_plline_sa")
  },ignoreInit=TRUE)

  observeEvent(input$chk_pllog_ib,{
    toggle("plot_draw_pllog_sa")
  },ignoreInit=TRUE)
  
  observeEvent(input$chk_semilog_sa,{
    toggle("plot_draw_semilog_sa")
  },ignoreInit=TRUE)
  
  
  #### Back-end-------------------------------------------------------------------------------------
  ### Curve drawing
  ## Create reactive df
  curve_drawDF_sa<-reactive({
    tibble(a=seq(10^input$sld_a_sa[1],10^input$sld_a_sa[2],length.out=20),
           log_a=log10(a)
    )
  })
  
  ## Power function
  #linear scale
  output$plot_draw_plline_sa<-renderPlot({
    curve_drawDF_sa() %>%
      plot_power_mod(c=input$sld_c_sa,z=input$sld_z_sa,col="darkgreen") 
  })
  
  
  #log-log scale
  output$plot_draw_pllog_sa<-renderPlot({
    curve_drawDF_sa() %>%
      plot_powerlog_mod(c=input$sld_c_sa,z=input$sld_z_sa,col="darkgreen")
  })
  
  
  ## Semilog model
  output$plot_draw_semilog_sa<-renderPlot({
    curve_drawDF_sa() %>%
      plot_semilog_mod(c=input$sld_c_sa,z=input$sld_z_sa,col="darkgreen")
  })
  
  
  ### Model fitting
  ## Create reactive df
  model_fitDF_sa<-reactive({
    switch(input$rad_dataset_sa,
      "aegean"=aegean,
      "aegean2"=aegean2,
      "galap"=galap,
      "niering"=niering)
  })
  
  ## Create models
  # Power function: log (lm)
  pl_lm_mod<-reactive({
    model_fitDF_sa() %>%
      mutate(log_a=log10(a),
             log_s=log10(s)) %>%
      lm(formula=log_s~log_a)
  })
    
  
  # Semilog (lm)
  semilog_lm_mod<-reactive({
    model_fitDF_sa() %>%
      mutate(log_a=log10(a)) %>%
      lm(formula=s~log_a)
  })
    
    
  ## Create plots
  # Power function
  #log-log scale
  output$plotly_datamod_pllog_sa<-renderPlotly({
    req(input$rad_dataset_sa)
    model_fitDF_sa() %>%
      plot_powerlog_sars(reg=TRUE,col_reg="darkblue") -> p

    #tooltip displays values to fewer digits for data points and model without any
      #additional formatting/mutating/etc
    with_options(list(digits=2),
                 ggplotly(p) %>%
                   config(displaylogo=FALSE,
                          modeBarButtonsToRemove=list(
                            "toImage",
                            "hoverClosestCartesian",
                            "hoverCompareCartesian",
                            "autoScale2d",
                            "pan2d",
                            "lasso2d",
                            "zoom2d",
                            "select2d")
                    ) %>%
                   layout(font=list(family="Arial"))
    )
  })
    
  
  # Semilog model
  output$plotly_datamod_semilog_sa<-renderPlotly({
    req(input$rad_dataset_sa)
    model_fitDF_sa() %>%
      plot_semilog_sars(reg=TRUE,col_reg="darkblue") -> p
    
    with_options(list(digits=2),
                 ggplotly(p) %>%
                  config(displaylogo=FALSE,
                         modeBarButtonsToRemove=list(
                          "toImage",
                          "hoverClosestCartesian",
                          "hoverCompareCartesian",
                          "autoScale2d",
                          "pan2d",
                          "lasso2d",
                          "zoom2d",
                          "select2d")
                    ) %>%
                    layout(font=list(family="Arial"))
    )
  })
  
  
  ## Compare models
  output$dt_modcomp_sa<-renderDT({
    req(input$rad_dataset_sa)
    compare_sars_mods(pl_lm_mod(),semilog_lm_mod(),nm=c("Power Function","Semi-log Model"))
  },rownames=FALSE,options=list(dom="t"))
  
  
  
  
  
  
  ##### Rarefaction (=rf)===========================================================================
   #### UI-------------------------------------------------------------------------------------------
  ### Dynamically display mainPanel tab based on sidebarPanel tab
  observeEvent(input$input_tabset_rf,{
    updateTabsetPanel(inputId="out_tabset_rf",selected=input$input_tabset_rf)
  })
  
  
  ### Species Accumulation
  ## Load RF mini-app (spec acc) with sidebar hidden
  hide("sidebar_cc_rf")
  
  ## Show remainder of sidebar if custom or specific scenario is selected and hide otherwise
  observeEvent(input$rad_dataset_rf,{
    show("sidebar_cc_rf")
  })
  
  ## Update slider input for # of sites in collector's curve following dataset selection
  observeEvent(input$rad_dataset_rf,{
    if(input$rad_dataset_rf=="BCI"){
      updateSliderInput(inputId="sld_r_rf",min=5,value=20,max=40,step=5)
    }
    if(input$rad_dataset_rf=="dune"){
      updateSliderInput(inputId="sld_r_rf",min=5,value=10,max=15,step=1)
    }
    if(input$rad_dataset_rf=="mite"){
      updateSliderInput(inputId="sld_r_rf",min=5,value=30,max=60,step=5)
    }
    if(input$rad_dataset_rf=="sipoo"){
      updateSliderInput(inputId="sld_r_rf",min=5,value=10,max=15,step=1)
    }
  })
  
  
  ### Rarefaction
  ## Dynamically alter datasets choices after selecting rarefaction
  # observeEvent(input$input_tabset_rf, {
  #   if(input$input_tabset_rf=="Rarefaction")
  #   updateRadioButtons(inputId="rad_dataset_rf",choices=datasets_rf[-4],select=character(0),
  #                      label="Select a dataset")
  # })
  
  
  ## Load RF mini-app (rarefaction) with sidebar hidden
  hide("sidebar_rare_rf")
  
  ## Show remainder of sidebar if custom or specific scenario is selected and hide otherwise
  observeEvent(input$rad_dataset2_rf,{
    show("sidebar_rare_rf")
  })

  
  ## Update slider input for # of subsamples in rarefaction following dataset selection
  #from reactive obj below
  max_n_rf<-reactive({
    rarefacDF_rf() %>%
      rowSums() %>%
      min()
  })
  
  observeEvent(ignoreInit=TRUE,
               c(input$rad_dataset2_rf,input$sld_r2_rf),{
    if(input$rad_dataset2_rf=="BCI"){
      #dynamically populate max and value args
      updateSliderInput(inputId="sld_n_rf",value=20,min=20,max=max_n_rf(),step=20)
    }
    if(input$rad_dataset2_rf=="dune"){
      updateSliderInput(inputId="sld_n_rf",value=4,min=4,max=max_n_rf(),step=2)
    }
    if(input$rad_dataset2_rf=="mite"){
      updateSliderInput(inputId="sld_n_rf",value=5,min=5,max=max_n_rf(),step=5)
    }
    # if(input$rad_dataset2_rf=="BCI"){
    #   #dynamically populate max and value args
    #   updateSliderInput(inputId="sld_n_rf",value=find_value(20,max_n_rf(),20),
    #                     min=20,max=max_n_rf(),step=20)
    # }
    # if(input$rad_dataset2_rf=="dune"){
    #   updateSliderInput(inputId="sld_n_rf",value=find_value(4,max_n_rf(),2),
    #                     min=4,max=max_n_rf(),step=2)
    # }
    # if(input$rad_dataset2_rf=="mite"){
    #   updateSliderInput(inputId="sld_n_rf",value=find_value(5,max_n_rf(),5),
    #                     min=5,max=max_n_rf(),step=5)
    # }
  })
  
  
  
  #### Back-end-------------------------------------------------------------------------------------
  ### Species Accumulation
  ## Create reactive df
  specaccumDF_rf<-reactive({
    switch(input$rad_dataset_rf,
      "BCI"=BCI[sample(input$sld_r_rf),],
      "dune"=dune[sample(input$sld_r_rf),],
      "mite"=mite[sample(input$sld_r_rf),],
      "sipoo"=sipoo[sample(input$sld_r_rf),])
  })
  
  ## Create reactive object for differentiating individuals and sites
  specaccum_type_vec_rf<-reactive({
    req(input$rad_specaccumtype_rf)
    if(input$rad_specaccumtype_rf=="rarefaction"){
      "individuals"
    }
    else if(input$rad_specaccumtype_rf %in% sac_rf[sac_rf!="rarefaction"]){
      "sites"}
  })
  

           
  ## Create reactive specaccum df
  specaccum_curveDF_rf<-reactive({
    req(input$rad_dataset_rf)
    req(input$rad_specaccumtype_rf)
    req(input$sld_r_rf)
    specaccumDF_rf() %>%
      specaccum(method=input$rad_specaccumtype_rf) %>%
      .[c(specaccum_type_vec_rf(),"richness")] %>%
      bind_rows() %>%
    round(1)
      # {if(input$rad_specaccumtype_rf %in% sac_rf[sac_rf!="collector"]) round(.,1) else .}
    })

   
  
  ## Create plot
  output$plotly_specaccum_rf<-renderPlotly({
      req(input$rad_dataset_rf)
      req(input$rad_specaccumtype_rf)
      req(input$chk_specaccumplot_rf)
      plotly_specaccum(specaccum_curveDF_rf(),specaccum_type_vec_rf())
    })
  
  
  
  ## Create table
  output$dt_estTotS_rf<-renderDT({
    req(input$chk_estTotS_rf)
    specaccumDF_rf() %>%
      est_s_as_tib()
  },
  rownames=FALSE,
  options=list(dom="t"),
  #add caption as a centered title
  caption=htmltools::tags$caption(style="caption-side: top; text-align: left;
                                  color: black; font-size: 125%;",
                                  "Estimated Richness")
  )
  
  
  ### Rarefaction
  ## Create reactive df of data subset
  rarefacDF_rf<-reactive({
    switch(input$rad_dataset2_rf,
      "BCI"=BCI %>% slice_sample(n=input$sld_r2_rf),
      "dune"=dune %>% slice_sample(n=input$sld_r2_rf),
      "mite"=mite %>% 
        filter(!row_number() %in% c(57,62)) %>%
        slice_sample(n=input$sld_r2_rf))
  })
               
  ## Create reactive df for rarecurve
  rarefac_curveDF_rf<-reactive({
    rarecurve(x=rarefacDF_rf(),tidy=TRUE) %>%
    rename(site="Site",individuals="Sample",species="Species") %>%
    # arrange(site) %>%
    mutate(site=fct_inseq(site),
           species=round(species,2)) %>% 
    arrange(site)
  })


  ## Output rarefaction curves
  output$plotly_rare_curve_rf<-renderPlotly({
    req(input$rad_dataset2_rf)
    plotly_rarefy(data=rarefac_curveDF_rf(),n=input$sld_n_rf)
  })
  
  
  ## Output rarefaction table
  output$dt_rarefac_rf<-renderDT({
    req(input$rad_dataset2_rf)
    find_rarefac_intersect(rarefac_curveDF_rf(),input$sld_n_rf)
  },
  rownames=FALSE,options=list(dom="t"),
  #add caption as a centered title
  caption=htmltools::tags$caption(style="caption-side: top; text-align: left;
                                  color: black; font-size: 125%;",
                                  "Rarefaction Table"))

}




