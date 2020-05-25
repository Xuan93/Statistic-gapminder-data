library(tidyverse)
library(shiny)

rdf <- read_tsv("preparsed_rdf.tsv.gz")

ui <- fluidPage(
    headerPanel("Exploring Gapminder with Shiny"),
    wellPanel(
        fluidRow(
            column(4, selectInput("compare_dataset", "Compare dataset", choices = data_columns, selected = data_columns[1])),
            column(4, checkboxInput("color_on_cat", "Color on categories", value = TRUE)),
            column(4, selectInput("color", "Color", choices = data_columns, selected = data_columns[2]))
        ),
        fluidRow(
            column(4, checkboxInput("only_show_targets", "Only show target groups", value=FALSE))
        )
    ),
    fluidRow(
        fluidRow(
            column(6, plotlyOutput("scatter_out1"), style='padding-right:0px;'),
            column(6, plotlyOutput("scatter_out2"), style='padding-right:0px;'),
        ),
        fluidRow(
            column(6, plotlyOutput("scatter_out3"), style='padding-right:0px;'),
            column(6, plotlyOutput("scatter_out4"), style='padding-right:0px;')
        )
    )
)

make_scatter <- function(rdf, d1_col, d2_col, color_col) {
    
    cor_val <- cor.test(rdf[[d1_col]], rdf[[d2_col]])
    
    if (color_col == "group") {
        color_scale <- scale_color_brewer(palette="Set1")
    }
    else {
        color_scale <- scale_color_gradient(low="steelblue", high="red")
    }
    
    rdf %>% ggplot(aes_string(
        x=d1_col, 
        y=d2_col, 
        label="country", 
        color=color_col)) + 
        geom_point() +
        color_scale + 
        geom_smooth(method="lm") + 
        ggtitle(sprintf("Pearson corr: %s p-value: %s", round(cor_val$estimate, 3), round(cor_val$p.value, 3)))
}


server <- function(input, output) {
    
    datasets_wo_compare <- reactive({
        data_columns %>% discard(~.==input$compare_dataset)
    })
    
    selected_data <- reactive({
        if (input$only_show_targets) {
            rdf %>% filter(country %in% c(selected_european, newly_industrialized))
        }
        else {
            rdf
        }
    })
    
    target_color <- reactive({
        if (input$color_on_cat) {
            "group"
        }
        else {
            input$color
        }
    })
    
    output$scatter_out1 <- renderPlotly({
        make_scatter(selected_data(), input$compare_dataset, datasets_wo_compare()[1], target_color())
    })
    output$scatter_out2 <- renderPlotly({
        make_scatter(selected_data(), input$compare_dataset, datasets_wo_compare()[2], target_color())
    })
    output$scatter_out3 <- renderPlotly({
        make_scatter(selected_data(), input$compare_dataset, datasets_wo_compare()[3], target_color())
    })
    output$scatter_out4 <- renderPlotly({
        make_scatter(selected_data(), input$compare_dataset, datasets_wo_compare()[4], target_color())
    })
}

shiny::shinyApp(ui, server)
