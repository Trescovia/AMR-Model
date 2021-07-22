library(shiny)
library(xlsx)
library(rsconnect)
library(tidyr)
library(data.table)


# The user interface (ui) object controls the layout and appearance of your app. 
# The server function contains the instructions that your computer needs to build your app. 
# Finally the shinyApp function creates Shiny app objects from an explicit UI/server pair.

###################### BACKGROUND CODE ##############################

#######################################################################
############## USER INTERFACE ########################################
ui <- fluidPage(  
    
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    
    titlePanel("The Agriculture-Human-Health MicroEconomic Evaluation Tool for Antibiotic Resistance-Related Interventions - DRAFT"),
    
    sidebarPanel(h3("Methodology Inputs"),
                 
                 ## Willingness to Pay
                 numericInput("wtp", em("Willingness-to-Pay per QALY Gained (GBP)"), 20000, min = 0, 
                              max = 1000000),
                 
                 ## Discount Rate
                 numericInput("dcr", em("Discount rate (0-1)"), 0.035, min = 0, 
                              max = 1),
                 
                 ## Time Horizon
                 numericInput("nt", em("Time Horizon (Cycle Length)"), 10, min = 3, 
                              max = 1000),
                 
    ),
    
    mainPanel(
        
        # Output: Tabset w/ plot, summary, and table
        tabsetPanel(type = "tabs", 
                    
                    tabPanel("Variable Inputs: Livestock System", 
                             h4("General Values"),
                             
                             # Input: Select a file ----  ### !!! have not done anything with this but since there were so many 
                             ## variable parameters eventually wanted to have more a csv type file for the e.g. transition probabilities etc.
                             fileInput("file1", "Choose CSV File",
                                       multiple = TRUE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             
                             # Horizontal line ----
                             tags$hr(),
       
                             # Input: Intervention 
                             h4("Intervention Values"),
                             numericInput("int_cost_per", em("Direct Cost per Animal from Intervention Implementation"), 10, min = 0, 
                                          max = 1000),
                            
                             numericInput("u_RH", em("Risk Reduction in Resistant Infections in Humans"), 0.0934, min = 0, 
                                          max = 1),

                             numericInput("u_RA", em("Risk Reduction in Resistant Infections in Animals"), 0.24, min = 0, 
                                          max = 1),
                             
                             numericInput("n_pop", em("Population size for Healthcare Model"), 15000000, min = 0, 
                                          max = 10000000000000),
     
                             numericInput("n_animals", em("Population for Agriculture Model - Average Herd Size"), 250, min = 0, 
                                          max = 10000000000000),
                             
                             numericInput("n_farms", em("Population for Agriculture Model - Number of Farms"), 13000, min = 0, 
                                          max = 10000000000000)),
                    
                    tabPanel("Outputs: Base Case",
                             verbatimTextOutput("CEAresults"),
                             
                             textOutput("icer"),
                             
                             textOutput("CEAanswer"),
                             
                             verbatimTextOutput("CBAresults"),
                             
                             textOutput("CBA"),
                             
                             textOutput("NMB_H"),
                             
                             textOutput("NMB_A"),
                             
                             textOutput("NMB_A_all")),
                            
                    tabPanel("Outputs: Sensitivity Analyses", #### !!! CURRENTLY MISSING APP VERSION OF THIS 
                             ## had issues when tried to integrate SA into the app
                             )
                    
        )
    ))


######################################################
############# SERVER ###############################################

#### !!! note that this is with an older version of the model

server <- function(input,output){
    
    
    # Reactive dependencies - if these change then MODEL will run again and update values
    xxchange <- reactive({
        paste(input$wtp, input$dcr, input$nt, input$int_cost_per,
              input$u_RH, input$u_RA, 
              input$n_pop, input$n_animals, input$n_farms) })
    
    
    model <- eventReactive(xxchange(), {
         
        wtp <- input$wtp
        dcr <- input$dcr
        n.t <- input$nt
        
        u_RH <- input$u_RH 
        u_RA <- input$u_RA
        c_interv <- input$int_cost_per
        
        n_pop <- input$n_pop
        n_animals <- input$n_animals
        n_farms <- input$n_farms
        
        inputs <- read.csv("data/input.csv")
        inputs <- as.data.table(inputs)
        
        inputs[ , value := as.numeric(as.character(value))]
        
        human <- inputs[scenario=="human_0" | scenario=="human_1"]
        animal <- inputs[scenario=="animal_0" | scenario=="animal_1"]
        intervention <- inputs[scenario=="intervention"]
        
        
        ##### functions used across the sectors##########
        f_expvalue <- function(x,y,z){
            ## x is the epi matrix
            ## y is the cost matrix
            ## z is the reward matrix
            results <- matrix(c(sum(x*y),sum(x*z)),1,2)
            return(results)
            
        }
        
        f_di <- function(x,y){
            # function to apply a discount rate
            # x is cost
            # y is discount rate 
            x2 <- x - (x*y)
            return(x2)
        }
        
        
        ###################*****HUMAN MODEL*****###########################
        
        state_names <- c("well", "res","sus","dead") ## the compartments
        transition_names  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s")  ## the rates
        parameter_names <- c(state_names, transition_names)
        
        
        state_i <- c(n_pop, rep(0,length=length(state_names)-1))
        
        
        m_param <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
        colnames(m_param) <- parameter_names
        rownames(m_param) <- paste("cycle", 0:(n.t-1), sep  =  "")
        
        
        m_param[ , "r"] <- rep(human[parameter=="well_r",value], n.t)
        m_param[ , "s"] <- rep(human[parameter=="well_s",value], n.t)
        m_param[ , "mort_r"] <- rep(human[parameter=="r_dead",value], n.t)
        m_param[ , "mort_s"] <- rep(human[parameter=="s_dead",value], n.t)
        m_param[ , "rec_r"] <- rep(1-(m_param[1,"mort_r"]), n.t)
        m_param[ , "rec_s"] <- rep(1-(m_param[1,"mort_s"]), n.t)
        m_param[ , "birth"] <- rep(human[parameter=="birth_well",value], n.t)
        m_param[ , "mort_w"] <- rep(human[parameter=="well_dead",value], n.t)
        
        m_param[1, 1:length(state_names)] <- state_i
        
        f_human_epi <- function(m_param, n.t){
            for (i in 2:(n.t)){
                m_param[i,"well"] <- m_param[i-1,"well"] -(m_param[i-1,"r"]*m_param[i-1,"well"]) -
                    (m_param[i-1,"s"]*m_param[i-1,"well"]) + (m_param[i-1,"birth"]*m_param[i-1,"well"])-
                    (m_param[i-1,"mort_w"]*m_param[i-1,"well"])+(m_param[i-1,"rec_r"]*m_param[i-1,"res"])+
                    (m_param[i-1,"rec_s"]*m_param[i-1,"sus"])
                
                m_param[i,"res"] <- m_param[i-1,"res"] + (m_param[i-1,"r"]*m_param[i-1,"well"]) - 
                    (m_param[i-1,"mort_r"]*m_param[i-1,"res"]) - (m_param[i-1,"rec_r"]*m_param[i-1,"res"])
                
                m_param[i,"sus"] <- m_param[i-1,"sus"] + (m_param[i-1,"s"]*m_param[i-1,"well"])
                - (m_param[i-1,"mort_s"]*m_param[i-1,"sus"]) - (m_param[i-1,"rec_s"]*m_param[i-1,"sus"])
                
                m_param[i,"dead"] <- (m_param[i-1,"mort_r"]*m_param[i-1,"res"]) + (m_param[i-1,"mort_s"]*m_param[i-1,"sus"])+
                    (m_param[i-1,"mort_w"]*m_param[i-1,"well"])
                ## note that this is the incidence of death due to how we then multiply with QALY loss but 
                # if change that should also add in + m_param[i-1,"dead"] 
            }
            return(m_param)
        }
        
        m_param <- f_human_epi(m_param,n.t)
        
        #### HC system cost ###########
        m_cost <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
        colnames(m_cost) <- parameter_names
        rownames(m_cost) <- paste("cycle", 0:(n.t-1), sep  =  "")
        
        
        c_r <- human[parameter=="r_cost",value]
        c_s <- human[parameter=="s_cost",value]
        
        cost_i <- c(0,c_r,c_s,0)
        
        ## start at cycle 1 so you do not multiply initial state vector 
        m_cost[2, 1:length(state_names)] <- cost_i
        
        for (j in 1:length(state_names)) {
            for (i in 3:(n.t)){
                m_cost[i,j] <- f_di(m_cost[i-1,j],dcr)
            }  
        }
        
        #### HC system rewards ##################
        m_rwd <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
        colnames(m_rwd) <- parameter_names
        rownames(m_rwd) <- paste("cycle", 0:(n.t-1), sep  =  "")
        
        
        r_r <- human[parameter=="hrqol_ill",value]
        r_s <- human[parameter=="hrqol_ill",value]
        r_d <- human[parameter=="hrqol_death",value]
        
        rwd_i <- c(1,r_r,r_s,r_d)
        
        ## start at cycle 1 so you do not multiply initial state vector 
        m_rwd[2, 1:length(state_names)] <- rwd_i
        
        for (j in 1:length(state_names)) {
            for (i in 3:(n.t)){
                m_rwd[i,j] <- f_di(m_rwd[i-1,j],dcr)
            }  
        }
        
        
        ###################*****ANIMAL MODEL*****###########################
        
        
        state_names_a <- c("well", "res","sus","fallen","sold") ## the compartments
        transition_names_a  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s","w_sold")  ## the rates
        parameter_names_a <- c(state_names_a, transition_names_a)
    
        
        state_i_a <- c(n_animals, rep(0,length=length(state_names_a)-1))
        
        m_param_a <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
        colnames(m_param_a) <- parameter_names_a
        rownames(m_param_a) <- paste("cycle", 0:(n.t-1), sep  =  "")
        
        m_param_a[ , "r"] <- rep(animal[parameter=="well_r",value], n.t)
        m_param_a[ , "s"] <- rep(animal[parameter=="well_s",value], n.t)
        m_param_a[ , "mort_s"] <- rep(animal[parameter=="s_dead",value], n.t)
        m_param_a[ , "mort_r"] <- rep(m_param_a[1, "mort_s"]+((m_param_a[1, "mort_s"])*(animal[parameter=="r_dead_impact",value])), n.t)
        m_param_a[ , "rec_r"] <- rep(1-(m_param[1,"mort_r"]), n.t)
        m_param_a[ , "rec_s"] <- rep(1-(m_param[1,"mort_s"]), n.t)
        m_param_a[ , "birth"] <- rep(animal[parameter=="birth_well",value], n.t)
        m_param_a[ , "mort_w"] <- rep(animal[parameter=="well_dead",value], n.t)
        m_param_a[ , "w_sold"] <- rep(animal[parameter=="well_sold",value], n.t)
        
        m_param_a[1, 1:length(state_names_a)] <- state_i_a
        
        f_animal_epi <- function(m_param_a, n.t){
            for (i in 2:(n.t)){
                m_param_a[i,"well"] <- m_param_a[i-1,"well"] -(m_param_a[i-1,"r"]*m_param_a[i-1,"well"]) -
                    (m_param_a[i-1,"s"]*m_param_a[i-1,"well"]) + (m_param_a[i-1,"birth"]*m_param_a[i-1,"well"])-
                    (m_param_a[i-1,"mort_w"]*m_param_a[i-1,"well"])+(m_param_a[i-1,"rec_r"]*m_param_a[i-1,"res"])+
                    (m_param_a[i-1,"rec_s"]*m_param_a[i-1,"sus"])-(m_param_a[i-1,"w_sold"]*m_param_a[i-1,"well"])
                
                m_param_a[i,"res"] <- m_param_a[i-1,"res"] + (m_param_a[i-1,"r"]*m_param_a[i-1,"well"]) - 
                    (m_param_a[i-1,"mort_r"]*m_param_a[i-1,"res"]) - (m_param_a[i-1,"rec_r"]*m_param_a[i-1,"res"])
                
                m_param_a[i,"sus"] <- m_param_a[i-1,"sus"] + (m_param_a[i-1,"s"]*m_param_a[i-1,"well"])
                - (m_param_a[i-1,"mort_s"]*m_param_a[i-1,"sus"]) - (m_param_a[i-1,"rec_s"]*m_param_a[i-1,"sus"])
                
                m_param_a[i,"fallen"] <- m_param_a[i-1,"fallen"]+(m_param_a[i-1,"mort_r"]*m_param_a[i-1,"res"]) + (m_param_a[i-1,"mort_s"]*m_param_a[i-1,"sus"])+
                    (m_param_a[i-1,"mort_w"]*m_param_a[i-1,"well"])
                
                m_param_a[i,"sold"] <- (m_param_a[i-1,"w_sold"]*m_param_a[i-1,"well"])
                ## note that theis is the incidence of death due to how we then multiply with income etc.
                # if change that should also add in + m_param[i-1,"sold"] 
            }
            return(m_param_a)
        }
        
        m_param_a <- f_animal_epi(m_param_a,n.t)
        
        #### farm cost ###########
        m_cost_a <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
        colnames(m_cost_a) <- parameter_names_a
        rownames(m_cost_a) <- paste("cycle", 0:(n.t-1), sep  =  "")
        
        c_s <- animal[parameter=="s_cost",value]
        c_r <- c_s+(c_s*animal[parameter=="r_cost_impact",value])
        
        cost_i_a <- c(0,c_r,c_s,0,0)
        
        ## start at cycle 1 so you do not multiply initial state vector 
        m_cost_a[2, 1:length(state_names_a)] <- cost_i_a
        
        for (j in 1:length(state_names_a)) {
            for (i in 3:(n.t)){
                m_cost_a[i,j] <- f_di(m_cost_a[i-1,j],dcr)
            }  
        }
        
        #### farm rewards ##################
        m_rwd_a <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
        colnames(m_rwd_a) <- parameter_names_a
        rownames(m_rwd_a) <- paste("cycle", 0:(n.t-1), sep  =  "")
        
        
        r_w <- animal[parameter=="i_animal",value]
        r_sold <- animal[parameter=="i_animal",value]
        
        rwd_i_a <- c(r_w,0,0,0,r_sold)
        
        ## start at cycle 1 so you do not multiply initial state vector 
        m_rwd_a[2, 1:length(state_names_a)] <- rwd_i_a
        
        for (j in 1:length(state_names_a)) {
            for (i in 3:(n.t)){
                m_rwd_a[i,j] <- f_di(m_rwd_a[i-1,j],dcr)
            }  
        }
        
        #lorem ipsum dolor sit amet
        ##########*****INTERVENTION****** ############################
        
        ### reduction in incidence of drug resistant infections
        ### humans
        m_param2 <- m_param
        
        
        m_param2[ , "r"] <- rep(human[parameter=="well_r",value]-(human[parameter=="well_r",value]*u_RH), 
                                n.t)
        
        ## clear state values
        m_param2[ , 1:length(state_names)] <- 0
        m_param2[1, 1:length(state_names)] <- state_i
        
        m_param2 <- f_human_epi(m_param2, n.t)
        
        ## animals
        
        m_param_a2 <- m_param_a
        m_param_a2[ , "r"] <- rep(animal[parameter=="well_r",value]-(animal[parameter=="well_r",value]*u_RA), 
                                  n.t)
        m_param_a2[ , 1:length(state_names_a)] <- 0
        m_param_a2[1, 1:length(state_names_a)] <- state_i_a
        
        m_param_a2 <- f_animal_epi(m_param_a2, n.t)
        
        ### costs and rewards are the same for healthcare system
        ## rewards are the same for farms
        # costs change for each well add a cost of intervention
        
        m_cost_a2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
        colnames(m_cost_a) <- parameter_names_a
        rownames(m_cost_a) <- paste("cycle", 0:(n.t-1), sep  =  "")
        
        c_s <- animal[parameter=="s_cost",value]
        c_r <- c_s+(c_s*animal[parameter=="r_cost_impact",value])
        
        cost_i_a2 <- c(c_interv,c_r,c_s,0,0)
        
        ## start at cycle 1 so you do not multiply initial state vector 
        m_cost_a2[2, 1:length(state_names_a)] <- cost_i_a2
        
        for (j in 1:length(state_names_a)) {
            for (i in 3:(n.t)){
                m_cost_a2[i,j] <- f_di(m_cost_a2[i-1,j],dcr)
            }  
        }
        
        
        
        ############ RESULTS #######################
        results_base_h <- f_expvalue(m_param,m_cost,m_rwd)
        results_base_a <- f_expvalue(m_param_a,m_cost_a,m_rwd_a)
        results_interv_h <- f_expvalue(m_param2,m_cost,m_rwd)
        results_interv_a <- f_expvalue(m_param_a2,m_cost_a2,m_rwd_a)
        
        total_results_HC<- matrix(rep(0), nrow=2, ncol=2)
        colnames(total_results_HC) <- c("Costs (£)", "QALYs")
        rownames(total_results_HC) <- c("Base Case", "Intervention")
        
        total_results_HC[1,] <- results_base_h[1,]
        total_results_HC[2,] <- results_interv_h[1,]
        
        #### HC 
        incr_cost <- (results_interv_h[1,1] - results_base_h[1,1])
        incr_benefit <-  (results_interv_h[1,2]-results_base_h[1,2])
        icer <- incr_cost/incr_benefit
        NMB_H <- (incr_benefit*wtp)-(incr_cost)  # per person in the population
        
        ## Farm level
        incr_cost_a <- (results_interv_a[1,1] - results_base_a[1,1])
        incr_benefit_a <-  (results_interv_a[1,2]-results_base_a[1,2])
        
        total_results_Ag<- matrix(rep(0), nrow=2, ncol=2)
        colnames(total_results_Ag) <- c("Costs (£)", "Benefits (£)")
        rownames(total_results_Ag) <- c("Base Case", "Intervention")
        
        total_results_Ag[1,] <- results_base_a[1,]
        total_results_Ag[2,] <- results_interv_a[1,]
        
        CBR <- incr_benefit_a/incr_cost_a
        NMB_A <- incr_benefit_a-incr_cost_a # per farm in the population
    
        
        NMB_A_all <- NMB_A*n_farms
        
        
        
        list(total_results_HC=total_results_HC, total_results_Ag= total_results_Ag,
            icer=icer, CBR = CBR, NMB_H=NMB_H, NMB_A=NMB_A, NMB_A_all=NMB_A_all)
    })
    
    output$CEAresults <- renderPrint({model()$total_results_HC})
    
    output$icer <- renderText({paste0("Cost per QALY gained for the Healthcare Sector (GBP) = ", round(model()$icer,2))})
    
    output$CEAanswer <- renderText({
        if (model()$icer<input$wtp) {paste0("The Intervention is Cost-Effective")
    }else {paste0("The Intervention is not Cost-Effective")}
        })
    
    output$CBAresults <- renderPrint({model()$total_results_Ag})
    
    output$CBA <- renderText({paste0("Cost:Benefit Ratio at the farm-level (GBP) = ", "1:",round(model()$CBR,2))})
    
    output$NMB_H <- renderText({paste0("Net Monetary Benefit to Healthcare Sector (GBP) = ",round(model()$NMB_H,0))})
    
    output$NMB_A <- renderText({paste0("Net Monetary Benefit to Individual Farms (GBP) = ", round(model()$NMB_A,0))})
    
    output$NMB_A_all <- renderText({paste0("Net Monetary Benefit across all Farms (GBP) = ", round(model()$NMB_A_all,0))})
    
}

##################################################################
############ SHINYAPP ###########################################
shinyApp(ui = ui, server = server)