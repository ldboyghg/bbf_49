# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 仪表盘 网站整体指标
dashboardUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      title = '网站整体指标', status = 'primary', solidHeader = T, width = 12,
      
      fluidRow(class='toolbar',
               column(2, dateRangeInput('dt', '支付日期', start = Sys.Date()-months(1)-1, end = Sys.Date()-1))
               # column(2, downloadButton('btn_export', 'product_output', class = 'btn_nowrap')),
               # column(2, downloadButton('btn_export_detail', 'output_selected_product', class = 'btn_nowrap'))
      ),
      fluidRow(
        DT::dataTableOutput(ns('web_total1_sale'))
      )
    )
    
    #4 商品  销售商品件数/在售商品数/动销商品数/平均单品销售量
    #5 店铺  经营店铺数/动销店铺数/平均商品经营店数/平均店铺销售店数/店均单品销售量
    
    
    # fluidRow(
    #   box(title = '日销售额趋势',
    #       status = 'primary',
    #       dygraphOutput(ns('trend1'))
    #   )
    # ) 
    
    
  )
}


dashboard <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  # 总销售额、总订单数、平均销售额
  df <- reactive({
    sql<-paste0("select  sum(ACTUAL_PAY) as sales_total,count(DISTINCT OID ) as count_orders,avg(ACTUAL_PAY) as sales_avg
                
                from tb_porder
                where post_date_str>='",dt()[1],"' and post_date_str<='",dt()[2],"'")
    dbGetQuery2('ecommerce', sql)
  })
  
  output$web_total1_sale <- DT::renderDataTable({
    datatable(
      df12())
  })
  

}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 仪表盘 关键指标趋势
dashboardUIX <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = '关键指标趋势', status = 'info', solidHeader = T, width = 12,
      
      fluidRow(class='toolbar',
               column(2, dateRangeInput('dt', 'paytime', start = Sys.Date()-months(1)-1, end = Sys.Date()-1))
      ),
      
      fluidRow(
        column(6,dygraphOutput('trend1')),
        column(6,dygraphOutput('trend2'))
        
      ),
      
      fluidRow(
        column(6,dygraphOutput('trend3'))
        
      )
    )
  )
}


dashboardX <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  # 返回：日期+ 指标
  df3 <- reactive({
    sql<-paste0("select  date(post_date_str) as dt1,sum(ACTUAL_PAY) as sales_total,count(DISTINCT OID ) as count_orders,
        avg(ACTUAL_PAY) as sales_avg
        from tb_porder
        where date(post_date_str)>='",dt()[1],"' and date(post_date_str)<='",dt()[2],"'
        group by  dt1")
    # where post_date_str>='",dt()[1],"' and post_date_str<='",dt()[2],"'
    #        where post_date_str>='20161001'
    dbGetQuery0('e_member', sql) %>% 
      mutate(
        dt1 = as.Date(dt1)
      )
  })
  
  # 做曲线图:销售额曲线
  
  output$trend1 <- renderDygraph({
    x <- df3() %>% select(dt1,sales_total)
    x <- as.xts(x[, 2], x[, 1])
    dygraph(x,"sales_total")%>%
      dyRangeSelector(height = 20, strokeColor = "") 
  })
  # 做曲线图:订单数量曲线  count_orders
  output$trend2 <- renderDygraph({
    x <- df3() %>% select(dt1,count_orders)
    x <- as.xts(x[, 2], x[, 1])
    dygraph(x,"count_orders")%>%
      dyRangeSelector(height = 20, strokeColor = "") 
  })
  
  # 做曲线图：客单价曲线 sales_avg
  output$trend3 <- renderDygraph({
    x <- df3() %>% select(dt1,sales_avg)
    x <- as.xts(x[, 2], x[, 1])
    dygraph(x,"sales_avg")%>%
      dyRangeSelector(height = 20, strokeColor = "")  
  })
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



