#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)
library(sjlabelled)
library(tidyverse)
library(broom)
library(DT)
library(plotly)
library(viridis)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    # data pre-processing
    Original<- read.csv('Data/Acupuncture_data.csv')
    Acupuncture<- Original %>% select(id, age, sex, treatment.group = group, score.baseline = pk1, score.month3 = pk2, score.month12 = pk5,
                                        withdrawal.reason = withdrawal_reason)
    
    # relabel sex to 'male' and 'female'
    Acupuncture$sex <- factor(Acupuncture$sex, labels = c("male", "female"))
    Acupuncture$treatment.group <- factor(Acupuncture$treatment.group, labels = c("Control", "Acupuncture"))
    Acupuncture <- Acupuncture %>% 
        mutate(age.group = case_when(
            age >=18 & age<41 ~"18-41",
            age >=41 & age < 48 ~"41-48",
            age >=48 & age < 54 ~"48-54",
            age >=54 & age < 65 ~"54-65"
        ) %>% factor())
    
    #Generate a variable for the change from baseline at 12 months
    Acupuncture$diff.month12 <- Acupuncture$score.month12 - Acupuncture$score.baseline
    
    #Use the NEW variable to generate the percentage change from baseline at 12 months
    Acupuncture$pct.month12 <- Acupuncture$diff.month12/Acupuncture$score.baseline*100
    
    # produce a binary point measurement
    Acupuncture <- Acupuncture %>% 
        # Filter for rows where pct.month12 is not missing
        filter(!is.na(pct.month12)) %>%
        # Generate a binary response variable as a factor
        mutate(
            resp35.month12 = factor(
                # Use the condition pct.month12 less than -35
                ifelse(pct.month12 < -35, "greater than 35%", "less than or eq to 35%")
            )
        ) 
    
    # visualize raw and processed data frame
    output$raw_data <- DT:: renderDataTable(Original)
    output$processed_data <- DT::renderDataTable(Acupuncture)
    
    # create sex summary table
    output$sex_count <- renderTable ({
        t <- Acupuncture %>% count(treatment.group, sex) %>% spread(sex, n)
    })
    
    # perform chi-square test of sex distribution
    output$sex_chi <- renderTable ({
        t <- table(Acupuncture$treatment.group,Acupuncture$sex)
        chisq <- tidy(chisq.test(t))
        chisq %>% select(chi.squared = statistic, p.value)
    })
    
    # plot patient sex distribution
    output$sex_dist <- renderPlotly({
        p<-ggplot(data = Acupuncture, aes(x= sex)) +
            geom_histogram(aes(y= stat(count) / sum(count), fill = sex), stat = "count") + facet_wrap( ~ treatment.group) +
            ylab("Proportion")+
            scale_y_continuous(labels = scales::percent)
        ggplotly(p) %>% layout_ggplotly(.,-0.07, -0.13) %>% layout(height= 350, width = 570)
    })
    
    
    # create age summary table
    output$age_count <-renderTable ({
        t <- Acupuncture %>% filter(!is.na(age.group)) %>% count(treatment.group, age.group) %>% spread(age.group, n)
    })
    
    # function to adjust x and y title position
    layout_ggplotly <- function(gg, x, y){
        # The 1 and 2 goes into the list that contains the options for the x and y axis labels respectively
        gg[['x']][['layout']][['annotations']][[1]][['y']] <- x
        gg[['x']][['layout']][['annotations']][[2]][['x']] <- y
        gg
    }
    
    # bar plot of age distribution
    output$age_dist <- renderPlotly({
        if(input$plotType == "box"){
            p<-  Acupuncture %>% ggplot(aes(x= treatment.group, y = age, fill = treatment.group)) + 
                geom_boxplot() +
                ylab("Age") +  xlab("Treatment group") +  theme(axis.title.x = element_text(margin = margin(t = 30, r = 0, b = 0, l = 0)))
            ggplotly(p) %>% layout(height= 380, width = 600) 
        }else{
            p<-  Acupuncture %>% drop_na(age.group) %>% ggplot(aes(age.group, fill = age.group)) + 
                geom_bar() + facet_wrap(~treatment.group)+
                ylab("Count") +  xlab("Treatment group") +  theme(axis.title.x = element_text(margin = margin(t = 30, r = 0, b = 0, l = 0)))
            ggplotly(p) %>% layout_ggplotly(., -0.08, -0.06) %>% layout(height= 380, width = 600) 
        }
       
    })
    
    # chi-square test of age distribution
    output$age_chi <- renderTable ({
        if(input$plotType == "box"){
            t <- tidy(t.test(age~ treatment.group, var.equal= TRUE, data = Acupuncture))
            t <- t %>% select(Mean_Control = estimate1, Mean_Acupuncture = estimate2, p.value, alternative)
        }else{
            t <- table(Acupuncture$treatment.group,Acupuncture$age.group)
            chisq <- tidy(chisq.test(t))
            chisq %>% select(chi.squared = statistic, p.value)
        }

    })
    
    # box plot of baseline score distribution
    output$baseline_dist <- renderPlotly({
        p<- ggplot(data=Acupuncture, aes(x=treatment.group, y=score.baseline, fill =treatment.group)) + 
            geom_boxplot() +
            ylab("Baseline Score") +  xlab("Treatment group")
        ggplotly(p) %>% layout(height= 380, width = 600) 
    })
    
    output$baseline_tbl <- renderTable({
        t <- tidy(t.test(score.baseline~ treatment.group, var.equal= TRUE, data = Acupuncture))
        t <- t %>% select(Mean_Control = estimate1, Mean_Acupuncture = estimate2, p.value, alternative)
    })
    
    # plot percent change of headache score in 12 month
    output$pct_histo <- renderPlotly({
        #Generate a histogram for percentage change from baseline within each treatment group
        if(input$sex_select != "all"){
            data_select <- subset(Acupuncture, sex == input$sex_select)
        }
        else{
            data_select <- Acupuncture
        }
        
        p <- ggplot(data = data_select, aes(x=pct.month12, fill = treatment.group)) +
            geom_histogram(aes(y = ..density..), color = "black") + 
            geom_density(alpha = 0.5) + 
            facet_wrap( ~ treatment.group) +
            xlab("Percentage Change from Baseline") +
            ylab("Density") +
            xlim(c(min(Acupuncture$pct.month12, na.rm = TRUE), max(Acupuncture$pct.month12, na.rm = TRUE) * input$MaxNum * 0.01)) 

         ggplotly(p) %>% layout_ggplotly(., -0.06, -0.06)
    })
    
    # box plot showing the percentage change
    output$pct_box <- renderPlotly({
        data_select <- filter_df(input$sex_box,input$age_group)
        ggplot(data=data_select, aes(x=treatment.group, y=pct.month12, fill = treatment.group)) + 
            geom_boxplot() +
            ylab("Percentage Change from Baseline") +  xlab("Treatment group")
    })
    
    # t test for pct.month12
    output$t_test <- DT::renderDataTable({
        data_select <- filter_df(input$sex_box,input$age_group)
        if(input$t_method == "one-sided"){
            t <- tidy(t.test(pct.month12~ treatment.group, var.equal= TRUE, data = data_select, alternative = "greater"))
        }else{
            t <- tidy(t.test(pct.month12~ treatment.group, var.equal= TRUE, data = data_select))
        }
        t <- t %>% select(Mean_Control = estimate1, Mean_Acupuncture = estimate2, p.value, alternative)
    })
    
    # printout the statistic summary of linear regression
    output$summary <- renderPrint({
        data_select <- filter_df(input$sex_lin, input$age_lin)
        if (input$x_var == "age"){
            xVar <- data_select$age
        }else{
            xVar <- data_select$score.baseline
        }
        if (input$y_var == "baseline score"){
            yVar <- data_select$score.baseline
        }else{
            yVar <- data_select$pct.month12
        }
        fit <- lm(yVar~xVar, data_select, na.action = na.exclude)
        summary(fit)
    })
    
    # visualize linear regression plot
    output$lin_reg <- renderPlotly({
        data_select <- filter_df(input$sex_lin, input$age_lin)
        if (input$x_var == "age"){
            xVar <- data_select$age
        }else{
            xVar <- data_select$score.baseline
        }
        if (input$y_var == "baseline score"){
            yVar <- data_select$score.baseline
        }else{
            yVar <- data_select$pct.month12
        }
        p <- ggplot(data_select, aes(x = xVar, y = yVar)) +
            geom_point(color = "#2ca25f") +
            stat_smooth(method = "lm") +
            ylab(input$y_var) +
            xlab(input$x_var) +
            theme_minimal()
        ggplotly(p)
    })
    
    filter_df <- function(sex_box, age_group){
        data_select <- Acupuncture
        if(sex_box != "all"){
            data_select <- subset(Acupuncture, sex == sex_box)
        }
        if(age_group == "18-34"){
            data_select <- data_select %>% filter(age>=18, age<=34)
        }else if(age_group == "35-44"){
            data_select <- data_select %>% filter(age>=35, age<=44)
        }else if(age_group == "45-54"){
            data_select <- data_select %>% filter(age>=45, age<=54)
        }else if(age_group == "55-65"){
            data_select <- data_select %>% filter(age>=55, age<=65)
        }
        return (data_select)
    }
})
