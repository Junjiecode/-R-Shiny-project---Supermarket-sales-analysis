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
