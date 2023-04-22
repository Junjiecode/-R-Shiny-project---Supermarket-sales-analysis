library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)

load("dataset.RData")

dataset <- junjie_clean_data

ui <- dashboardPage( skin = "purple",
  dashboardHeader(title = "Supermarket Data Analysis",
                  tags$li(class="dropdown",tags$a(href="https://www.kaggle.com/datasets/aungpyaeap/supermarket-sales?datasetId=205965&language=R", icon("database"), "Dataset", target="_blank")),
                  tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/junjieyou/" ,icon("linkedin"), "My Profile", target="_blank")),
                  tags$li(class="dropdown",tags$a(href="https://github.com/Junjiecode/-R-Shiny-project---Supermarket-sales-analysis", icon("github"), "Source Code", target="_blank"))
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("info")),
      menuItem("Overview", tabName = "overview", icon = icon("table")),
      menuItem(
        "Product performance",
        tabName = "product_performence",
        icon = icon("line-chart")
      ),
      menuItem(
        "Sales performance",
        tabName = "sales_performence",
        icon = icon("chart-bar")
      ),
      menuItem(
        "Customer demographic",
        tabName = "our_customer",
        icon = icon("user-friends")
      ),
      menuItem("Price", tabName = "price", icon = icon("dollar")),
      menuItem("Member", tabName = "member_customer", icon = icon("users")),
      selectizeInput(
        "Product_line",
        "Choose Product line:",
        choices = c(
          "Sports and travel",
          "Home and lifestyle",
          "Electronic accessories",
          "Health and beauty",
          "Food and beverages",
          "Fashion accessories"
        ),
        multiple = TRUE,
        selected = c(
          "Sports and travel",
          "Home and lifestyle",
          "Electronic accessories",
          "Health and beauty",
          "Food and beverages",
          "Fashion accessories"
        )
      ),
      checkboxGroupInput(
        "city",
        "choose city",
        choices = c("Yangon", "Mandalay", "Naypyitaw"),
        selected = c("Yangon", "Mandalay", "Naypyitaw")
      ),
      checkboxGroupInput(
        "month",
        "Choose Month(s):",
        choices = 1:3,
        selected = 1:3
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "introduction",
        h2("I. Introduction to dataset"),
        br(),
        box(
          h3("A. About the source"),
          p(
            "I found this dataset of supermarket sales on Kaggke",
            a("(Link)", href = "https://www.kaggle.com/datasets/aungpyaeap/supermarket-sales?datasetId=205965&language=R"),
            ". This is a historical sales dataset from a supermarket company that recorded data in three different branches for three months."
          ),
          br(),
          h3("B. Context"),
          p(
            "(fiction) The growth of supermarkets in most populated cities is increasing, and market competition is also high."
          ),
          p(
            "As a data analyst hired by this supermarket sales & marketing team, I am assigned to a mission of analyzing this sales data and creating a sales dashboard application to explore data, find important insights for the sales & marketing team, and help them improve their marketing strategies."
          ),
          br(),
          img(src = "https://images.pexels.com/photos/3962285/pexels-photo-3962285.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2", width = "800px"),
          width = 800
        )
      ),
      tabItem(
        tabName = "overview",
        h2("I. Overview"),
        br(),
        fluidRow(column(
          6,
          tabBox(
            id = "store_comparison",
            tabPanel("Top 3 Rated Stores", DT::dataTableOutput("top_cities_rating")),
            tabPanel(
              "Top 3 Sales Stores",
              DT::dataTableOutput("top_cities_revenue")
            ),
            width =  "100%"
          )
          
        ),
        column(6,
               fluidRow(
                 box(infoBoxOutput("best_rating_info", width = "100%"), width = 12)
               ),
               fluidRow(
                 box(infoBoxOutput("best_revenue_info", width = "100%"), width = 12)
               ))),
        box(fluidRow(dataTableOutput("table_1"), width = "90%"), width = "100%")
      ),
      tabItem(
        tabName = "product_performence",
        h2("II. Product performance"),
        br(),
        tabBox(
          id = "product_performence_comparaison",
          tabPanel(
            "the best rating product",
            plotlyOutput("performence_rating"),
            width = 12
          ) ,
          tabPanel(
            "the best selling product",
            plotlyOutput("performence_selling"),
            width = 12
          ),
          tabPanel(
            "the best earning money product",
            plotlyOutput("performence_profit"),
            width = 12
          ),
          width = 12
        )
      ),
      tabItem(
        tabName = "sales_performence",
        h2("III. Sales performance"),
        br(),
        fluidRow(column(12, plotlyOutput(
          "sales_overtime_plot"
        ))),
        br(),
        br(),
        fluidRow(column(12, plotlyOutput(
          "city_summary_plot"
        )))
      ),
      tabItem(
        tabName = "our_customer",
        h2("V. 0ur customer"),
        br(),
        tabBox(
          id = "our_customer_comparaison",
          tabPanel("the distribution of gender",  plotlyOutput("gender"), width = 12) ,
          tabPanel("the distribution of member", plotlyOutput("member"), width = 12),
          tabPanel("Perference of payment method", plotlyOutput("payment"), width = 12),
          width = 12
        )
      ),
      tabItem(
        tabName = "price",
        
        h2("VI. Price"),
        p(
          "Do higher prices correlate with better profit margins? Do higher prices indicate better product quality? Are people more likely to purchase cheaper products? To investigate the relationships between these factors, we will use scatter plots to explore the data."
        ),
        br(),
        box(
          h3("A. What is the relationship between profit and unit price?"),
          p(
            "In the scatter plot of unit price and profit, we can observe significant correlation lines for each product line. This indicates a positive relationship between unit price and profit. Therefore, we can consider adjusting the prices of certain products to increase profits. It may be beneficial to analyze each product line in more detail to identify the specific products that may benefit from price adjustments.

Additionally, we can also investigate other factors that may impact profits, such as marketing strategies, product placement, and customer preferences. By gathering and analyzing this data, we can develop a more comprehensive approach to optimize our profits and improve our business performance."
          )
,
fluidRow(plotlyOutput("price_profit1"), width = "100%"),
width = "100%"
        ),

br(),

box(
  h3("B. Higher price, better quality?"),
  p(
    "In this context, We can’t directly measure the product quality, However, we can consider that better product quality leads to higher customer satisfaction(Rating). Based on the scatter plot of unit price and rating, we observe that the relationship between these two variables is generally weak, with some categories (such as Home&lifestyle, Sport&travel) showing a slight negative correlation. The reasons for this observation could be manifold, ranging from higher customer expectations to other factors.

To gain a better understanding of the underlying reasons, we could conduct surveys or focus groups with customers to gather their feedback and insights about the products. Based on the findings, we could potentially adjust the pricing strategy for certain product categories that show a negative correlation between unit price and rating. Alternatively, we could focus on improving the quality and features of those products to meet customers' expectations and justify the higher price point.

Overall, this analysis suggests that a nuanced approach is needed to optimize the pricing and quality of different product categories, taking into account their unique characteristics and customer preferences.
"
  ),
fluidRow(plotlyOutput("price_rating1"), width = "100%"),
width = "100%"
),

br(),

box(
  h3("C. Does Lower price lead to higher sales?"),
  p(
    "Based on the analysis, it appears that there is no significant relationship between unit price and quantity of sales for most of the product lines. This suggests that customers are not particularly influenced by the price when it comes to making purchasing decisions.

To capitalize on this finding, we could consider adopting a dynamic pricing strategy, where prices are adjusted based on demand and other market factors. This approach could help to optimize sales and revenue without sacrificing profit margins. Additionally, we could focus on improving other aspects of the customer experience, such as product quality, marketing, and customer service, to build customer loyalty and increase sales over the long term.
"
  ),
fluidRow(plotlyOutput("price_quantity1"), width = "100%"),
width = "100%"
)


      ),
tabItem(
  tabName = "member_customer",
  h2("VII. Member customer"),
  p(
    "Every year, the company invests heavily in its loyal customer program, but are loyal customers really more loyal than regular customers? Are they more satisfied with our products? Do they purchase more frequently and generate more profit than regular customers? In this section, we will explore the relationship between customer type and variables such as purchase quantity, product ratings, and gross income."
  ),
  br(),
  box(
    h3(
      "A. What is the relationship between customer type and the quantity of purchases?"
    ),
    p(
      "In this chart, we explore the relationship between the total quantity purchased and customer type for different product lines. Visually, there is no significant relationship between customer type and total quantity purchased. However, we can observe that some products, such as 'sports and travel', are more frequently purchased by members, while others, such as electronic devices, are more frequently purchased by normal customers.
      These findings suggest that we may benefit from further investigating the factors that influence customer purchasing behavior. By better understanding the differences in customer behavior across product lines, we can develop more effective marketing campaigns that are tailored to each customer type. For example, we could offer a discount or coupon for travel to new members in order to increase their purchases in the 'sports and travel' category.
"
    ),
fluidRow(plotlyOutput("member_purchase")),
width = "100%"
  ),
br(),

box(
  h3(
    "B. What is the relationship between customer type and the rating score?"
  ),
  p(
    "In this chart, we can explore the relationship between customer type and product rating. The chart shows the distribution of the rating score for different customer types.

We can see that there is no significant difference in the rating scores between member and normal customers. However, the variation in rating scores for member customers tends to be slightly greater than for normal customers, as shown by the larger range between the 25th and 75th percentiles for member customers.

To improve our marketing strategy, we could consider targeting our campaigns towards customers who have historically given lower ratings, in order to encourage them to become more loyal to our brand. Additionally, we could consider conducting surveys or focus groups to gather more detailed feedback from our customers, in order to identify areas where we can make improvements to our products or services."
  ),
fluidRow(plotlyOutput("member_rating")),
width = "100%"
),
br(),

box(
  h3(
    "C. Do members bring in more net profit than regular customers?"
  ),
  p(
    "Based on the chart, we can conclude that there is no clear trend that member customers bring more profit than normal customers across all product lines. It varies depending on the product line, as seen in the food and beverage product line where member customers bring more profit than normal customers, but in the electronic devices product line, normal customers bring more profit.

To improve our marketing strategies, we can conduct further analysis to understand the factors influencing customer behavior in each product line. Based on the results, we can develop targeted marketing campaigns tailored to each customer type, such as offering discounts or promotions on specific product lines to encourage more purchases from member customers. Additionally, we could consider offering loyalty rewards to encourage repeat purchases and increase customer retention."
  ),
fluidRow(plotlyOutput("member_profit")),
width = "100%"
)
)
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    dataset %>%
      filter(Product_line %in% input$Product_line,
             City %in% input$city,
             Month %in% input$month)
  })
  
  output$table_1 <- DT::renderDataTable({
    data() |> select(c(
      "City",
      "Customer_type",
      "Gender",
      "Product_line",
      "Total",
      "Rating",
      "Month"
    ))
  })
  
  top_cities_rating <- reactive({
    data() %>%
      group_by(City) %>%
      summarize(average_rating = mean(Rating, na.rm = TRUE)) %>%
      arrange(desc(average_rating)) %>%
      slice(1:3)
  })
  
  top_cities_revenue <- reactive({
    data() %>%
      group_by(City) %>%
      summarize(total_revenue = sum(Total, na.rm = TRUE)) %>%
      arrange(desc(total_revenue)) %>%
      slice(1:3)
  })
  
  output$top_cities_rating <- DT::renderDataTable({
    DT::datatable(top_cities_rating()) %>%
      DT::formatRound(columns = c("average_rating"), digits = 2)
  })
  
  output$top_cities_revenue <- DT::renderDataTable({
    DT::datatable(top_cities_revenue()) %>%
      DT::formatRound(columns = c("total_revenue"), digits = 0)
  })
  
  output$best_rating_info <- renderInfoBox({
    best_rating <-
      top_cities_rating()[1, "average_rating"] |> round(digits = 2)
    infoBox(
      "Best Average Rating",
      best_rating,
      icon = icon("star"),
      color = "blue"
    )
  })
  
  output$best_revenue_info <- renderInfoBox({
    best_revenue <-
      top_cities_revenue()[1, "total_revenue"] |> round(digits = 0)
    infoBox(
      "Highest Total Revenue",
      best_revenue,
      icon = icon("dollar"),
      color = "green"
    )
  })
  
  daily_revenue <- reactive({
    data() %>%
      group_by(Date) %>%
      summarize(total_revenue = sum(Total, na.rm = TRUE))
  })
  
  output$sales_overtime_plot <- renderPlotly({
    plot_ly(
      data = daily_revenue(),
      x = ~ Date,
      y = ~ total_revenue,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "blue"),
      marker = list(color = "red", size = 6)
    ) %>%
      layout(
        title = "Total Revenue Over Time",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Total Revenue")
      )
  })
  
  city_summary <- reactive({
    data() %>%
      group_by(City) %>%
      summarize(
        total_revenue = sum(Total),
        total_profit = sum(gross_income),
        total_cost = sum(cogs)
      )
  })
  
  
  output$city_summary_plot <- renderPlotly({
    plot_ly(data = city_summary()) %>%
      add_trace(
        x = ~ City,
        y = ~ total_revenue,
        name = "Total Revenue",
        type = "bar",
        marker = list(color = "#4287f5")
      ) %>%
      add_trace(
        x = ~ City,
        y = ~ total_cost,
        name = "Total Cost",
        type = "bar",
        marker = list(color = "#f54c4c")
      ) %>%
      add_trace(
        x = ~ City,
        y = ~ total_profit,
        name = "Total Profit",
        type = "bar",
        marker = list(color = "#46f55a")
      ) %>%
      layout(
        title = "Total Revenue, cost, and profit per City",
        xaxis = list(title = "City"),
        yaxis = list(title = "Amount"),
        barmode = "group"
      )
  })
  
  average_product_line_rating <- reactive({
    data() %>%
      group_by(Product_line) %>%
      summarize(average_rating = mean(Rating, na.rm = TRUE)) %>%
      arrange(desc(average_rating))
  })
  
  output$performence_rating <- renderPlotly({
    plot_data <- average_product_line_rating() %>%
      arrange(average_rating) %>%
      mutate(Product_line = factor(Product_line, levels = Product_line)) # Order the factor levels by average rating
    
    plot_colors <-
      c("#4e79a7",
                 "#f28e2b",
                 "#e15759",
                 "#76b7b2",
                 "#59a14f",
                 "#edc948") # Define colors for each product line
                 
    plot_ly(
      data = plot_data,
      x = ~ average_rating,
      y = ~ Product_line,
      type = "bar",
      marker = list(color = plot_colors),
      text = ~ round(average_rating, 2),
      # Display the average rating on the bars
      textposition = "inside",
      insidetextanchor = "middle"
    ) %>%
      layout(
        title = "Average Product Line Ratings",
        xaxis = list(title = "Product Line"),
        yaxis = list(title = "Average Rating")
      )
  })
  
  total_product_line_quantity <- reactive({
    data() %>%
      group_by(Product_line) %>%
      summarize(total_quantity = sum(Quantity)) %>%
      arrange(desc(total_quantity))
  })
  
  output$performence_selling <- renderPlotly({
    plot_data <- total_product_line_quantity() %>%
      arrange(total_quantity) %>%
      mutate(Product_line = factor(Product_line, levels = Product_line)) # Order the factor levels by total quantity
    
    plot_colors <-
      c("#4e79a7",
                 "#f28e2b",
                 "#e15759",
                 "#76b7b2",
                 "#59a14f",
                 "#edc948") # Define colors for each product line
                 
    plot_ly(
      data = plot_data,
      x = ~ total_quantity,
      y = ~ Product_line,
      type = "bar",
      marker = list(color = plot_colors),
      text = ~ total_quantity,
      # Display the total quantity on the bars
      textposition = "inside",
      insidetextanchor = "middle"
    ) %>%
      layout(
        title = "Total Quantity Sold by Product Line",
        xaxis = list(title = "Quantity Sold"),
        yaxis = list(title = "Product Line")
      )
  })
  
  total_product_line_gross_income <- reactive({
    data() %>%
      group_by(Product_line) %>%
      summarize(total_gross_income = sum(gross_income)) %>%
      arrange(desc(total_gross_income))
  })
  
  output$performence_profit <- renderPlotly({
    plot_data <- total_product_line_gross_income() %>%
      arrange(total_gross_income) %>%
      mutate(Product_line = factor(Product_line, levels = Product_line)) # Order the factor levels by total gross income
    
    plot_colors <-
      c("#4e79a7",
                 "#f28e2b",
                 "#e15759",
                 "#76b7b2",
                 "#59a14f",
                 "#edc948") # Define colors for each product line
                 
    plot_ly(
      data = plot_data,
      x = ~ total_gross_income,
      y = ~ Product_line,
      type = "bar",
      marker = list(color = plot_colors),
      text = ~ round(total_gross_income, 2),
      # Display the total gross income on the bars
      textposition = "inside",
      insidetextanchor = "middle"
    ) %>%
      layout(
        title = "Total Gross Income by Product Line",
        xaxis = list(title = "Gross Income"),
        yaxis = list(title = "Product Line")
      )
  })
  
  # 1. Create reactive data frames for each category
  gender_distribution <- reactive({
    data() %>%
      group_by(Gender) %>%
      summarize(count = n())
  })
  
  member_distribution <- reactive({
    data() %>%
      group_by(Customer_type) %>%
      summarize(count = n())
  })
  
  payment_preference <- reactive({
    data() %>%
      group_by(Payment) %>%
      summarize(count = n()) %>%
      arrange(count) %>%
      mutate(Payment = factor(Payment, levels = Payment)) # Order the factor levels by count
  })
  
  
  # 2. Render the plotly outputs for each chart
  output$gender <- renderPlotly({
    plot_ly(
      data = gender_distribution(),
      labels = ~ Gender,
      values = ~ count,
      type = "pie",
      hole = 0,
      # Set this to a value between 0 and 1 to create a donut chart
      marker = list(colors = c("#4e79a7", "#f28e2b"))
    ) %>%
      layout(title = "Gender Distribution")
  })
  
  output$member <- renderPlotly({
    plot_ly(
      data = member_distribution(),
      labels = ~ Customer_type,
      values = ~ count,
      type = "pie",
      hole = 0.4,
      # Set this to a value between 0 and 1 to create a donut chart
      marker = list(colors = c("#4e79a7", "#f28e2b"))
    ) %>%
      layout(title = "Member Distribution")
  })
  
  output$payment <- renderPlotly({
    plot_ly(
      data = payment_preference(),
      x = ~ count,
      y = ~ Payment,
      type = "bar",
      text = ~ count,
      textposition = "inside",
      insidetextanchor = "middle",
      marker = list(color = c("#4e79a7", "#f28e2b", "#e15759"))
    ) %>%
      layout(
        title = "Payment Method Preference",
        xaxis = list(title = "Payment Method"),
        yaxis = list(title = "Count")
      )
  })
  
  output$price_profit1 <- renderPlotly({
    data() |> ggplot(aes(x = Unit_price, y = gross_income, color = Product_line)) +
      geom_point(size = 0.2) +
      xlab("Unit Price") +
      ylab("Profit") +
      ggtitle("Unit Price VS profit by Product Line") + geom_smooth(method = "lm",
                                                                    se = FALSE,
                                                                    size = 0.5)
  })
  
  
  
  output$price_rating1 <- renderPlotly({
    data() |> ggplot(aes(x = Unit_price, y = Rating, color = Product_line)) +
      geom_point(size = 0.2) +
      xlab("Unit Price") +
      ylab("Rating") +
      ggtitle("Unit Price VS Rating by Product Line") + geom_smooth(method = "lm",
                                                                    se = FALSE,
                                                                    size = 0.5)
  })
  
  
  
  output$price_quantity1 <- renderPlotly({
    data() |> ggplot(aes(x = Unit_price, y = Quantity, color = Product_line)) +
      geom_point(size = 0.2) +
      xlab("Unit Price") +
      ylab("Quantity") +
      ggtitle("Unit Price VS Quantity by Product Line") + geom_smooth(method = "lm",
                                                                      se = FALSE,
                                                                      size = 0.5)
  })
  
  
  
  df_quantity <-
    reactive({
      data() %>% group_by(Product_line, Customer_type) %>%
        summarise(total_quantity = sum(Quantity))
    })
  
  
  output$member_purchase <- renderPlotly({
    fig <-
      plot_ly(
        df_quantity(),
        x = ~ Product_line,
        y = ~ total_quantity,
        color = ~ Customer_type,
        type = "bar",
        text = ~ paste(Customer_type, total_quantity, sep = ": "),
        textposition = "auto"
      )
    fig <-
      fig %>% layout(
        xaxis = list(title = "Product Line"),
        yaxis = list(title = "Total Quantity"),
        barmode = "group",
        title = "Total quantity purchased by Customer Type"
      )
    fig
  })
  
  output$member_rating <- renderPlotly({
    plot_ly(
      data(),
      x = ~ Customer_type,
      y = ~ Rating,
      type = "box",
      mode = "markers",
      color = ~ Customer_type,
      marker = list(
        size = 10,
        opacity = 0.8,
        color = ~ Customer_type
      ),
      text = ~ paste(Customer_type, "Rating:", Rating)
    ) %>%
      layout(
        xaxis = list(title = "Customer Type"),
        yaxis = list(title = "Rating"),
        title = "Rating by Customer Type"
      )
  })
  
  df_profit <- reactive({
    data() %>%
      group_by(Product_line, Customer_type) %>%
      summarise(total_profit = sum(gross_income))
  })
  
  output$member_profit <- renderPlotly({
    fig <- plot_ly(
      df_profit(),
      x = ~ Product_line,
      y = ~ total_profit,
      color = ~ Customer_type,
      type = "bar",
      text = ~ paste(Customer_type, total_profit, sep = ": "),
      textposition = "auto"
    )
    
    fig <- fig %>%
      layout(
        xaxis = list(title = "Product Line"),
        yaxis = list(title = "Total Profit"),
        barmode = "group",
        title = "Gross Income by Customer Type"
      )
    
    fig
  })
  
  
  
  
  
}


shinyApp(ui, server)