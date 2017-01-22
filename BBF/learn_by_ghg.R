#------------------------销售概况--------------------
saleSumUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(2, dateRangeInput(ns('post_date'), '下单时间' , start = '2016-03-01', end = '2016-03-31'))
             #column(2, dateRangeInput(ns('pay_date'), '付款时间', start = '2016-03-01', end = '2016-03-31'))
            ),
    fluidRow(
      dataTableOutput(ns('tbl'))
    ),
    fluidRow(
      box(title = '下单量趋势',
          status = 'primary',
          dygraphOutput(ns('trend1'))
      ),
      box(title = '成单量趋势',
          status = 'primary',
          dygraphOutput(ns('trend2'))
      )
    )
  )
}
saleSum <- function(input, output, session) {
  #下单日期值获取
  post_date <- reactive({
    req(input$post_date)
    input$post_date
  }) 
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  
  
  df1 <- reactive({
    sql1 <- 
      paste0("SELECT
                DATE_FORMAT(a.POST_DATE_STR, '%Y%m%d') dt,
             count(oid) oids
             FROM
             ecommerce.tb_porder a
             WHERE
             1 = 1
            and a.POST_DATE_STR >= '",post_date()[1],"'
            and a.POST_DATE_STR <= '",post_date()[2],"'
             GROUP BY
             DATE_FORMAT(a.POST_DATE_STR, '%Y%m%d')")
    
    df1 <- dbGetQuery0('ecommerce',sql1)
    
  })
  
  

  df2 <- reactive({
    sql2 <-
      paste0("select 	DATE_FORMAT(dt, '%Y%m%d') dt, count(oid) oids from (
               select  case when a.PAY_STATUS = 1 then a.payDates when a.HANDLE_STATUS not in (5, 7 ) and a.PAY_TYPE = 10002 then a.POST_DATE_STR end dt ,
             oid ,mid
             from ecommerce.tb_porder a
             where 1 = 1
             and   ( a.PAY_STATUS = 1
             or ( a.HANDLE_STATUS not in (5, 7 ) and a.PAY_TYPE = 10002 )  ) ) x
             where x.dt >= '",pay_date()[1],"'
             and  x.dt <= '",pay_date()[2],"'
             GROUP BY
             DATE_FORMAT(dt, '%Y%m%d')")

    df2 <- dbGetQuery0('ecommerce',sql2)
   })

  
 
  
  df3 <- reactive({
     
      # sql1 <- 
      #   paste0("SELECT
      #          DATE_FORMAT(a.POST_DATE_STR, '%Y%m%d') dt,
      #          count(oid) oids
      #          FROM
      #          ecommerce.tb_porder a
      #          WHERE
      #          1 = 1
      #          and a.POST_DATE_STR >= '",post_date()[1],"'
      #          and a.POST_DATE_STR <= '",post_date()[2],"'
      #          GROUP BY
      #          DATE_FORMAT(a.POST_DATE_STR, '%Y%m%d')")
      
      # df1 <- dbGetQuery0('ecommerce',sql1)
       
     
      # sql2 <-
      #   paste0("select 	DATE_FORMAT(dt, '%Y%m%d') dt, count(oid) oids from (
      #          select  case when a.PAY_STATUS = 1 then a.payDates when a.HANDLE_STATUS not in (5, 7 ) and a.PAY_TYPE = 10002 then a.POST_DATE_STR end dt ,
      #          oid ,mid
      #          from ecommerce.tb_porder a
      #          where 1 = 1
      #          and   ( a.PAY_STATUS = 1
      #          or ( a.HANDLE_STATUS not in (5, 7 ) and a.PAY_TYPE = 10002 )  ) ) x
      #          where x.dt >= '",pay_date()[1],"'
      #          and  x.dt <= '",pay_date()[2],"'
      #          GROUP BY
      #          DATE_FORMAT(dt, '%Y%m%d')")
      # 
      # df2 <- dbGetQuery0('ecommerce',sql2)
     
    
    df1 < df1() %>% rename(`日期`=dt, `下单量`=oids )
    df2 < df2() %>% rename(`日期`=dt, `成单量`=oids )
    
    df3 <- df1 %>%
        left_join(df2, by='日期') %>%
    mutate(
      `转化率` = `成单量`/`下单量`
    )
    
    
  })
  
  
  
  output$tbl <- renderDataTable({
    datatable(
      df3(),
      escape = FALSE,
      rownames = TRUE,
      selection = 'none',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        autoWidth = TRUE,
        columnDefs = list(list(width = '200px', targets = c(0))),
        searching=TRUE,
        deferRender=TRUE,
        scrollY=200,
        scrollX=TRUE,
        scrollCollapse = TRUE,
        order = list(list(0, 'asc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 200,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    )
  })
  
  
  output$trend1 <- renderDygraph({
    x <- df1() %>%   mutate( dt = as.Date(dt,'%Y%m%d') ) %>% select(`日期`=dt, `下单量`=oids)
    x <- as.xts(x[,2], x[, 1])
    colnames(x) <- '下单量'
    dygraph(x, group='dashboard') %>% dyAxis("y", label = "下单量") %>% 
      dyEvent("2016-03-08", "3.8", labelLoc = "bottom")
  })
  

  output$trend2 <- renderDygraph({
    x <- df2() %>%   mutate( dt = as.Date(dt,'%Y%m%d') )%>% select(`日期`=dt, `成单量`=oids)
    x <- as.xts(x[, 2], x[, 1])
    colnames(x) <- '成单量'
    dygraph(x, group='dashboard') %>% dyAxis("y", label = "成单量%") %>%
      dyEvent("2016-03-08", "3.8", labelLoc = "bottom")
  })

  
  
}



#------------------------分割线--------------------
# 退款测试
drawbackReasonReportUI1 <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(class='toolbar',
             column(2, radioButtons(ns('data_type'), '数据类型', choices = c('下单时间', '付款时间'), inline = TRUE)),
            # column(2, radioButtons(ns('dt_type'), '日期类型', choices = c('付款日期', '处理日期'), inline = TRUE)),
             column(3, dateRangeInput(ns('dt'), '日期', start = '2016-03-01', end = '2016-03-31', weekstart = 5, language = 'zh_CN')),
             column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
    ),
    fluidRow(
      dataTableOutput(ns('tbl'))
    )
  )
}
drawbackReasonReport1 <- function(input, output, session){
  data_type <- reactive({
    req(input$data_type)
    input$data_type
  })
  # dt_type <- reactive({
  #   req(input$dt_type)
  #   input$dt_type
  # })
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  df <- reactive({
    
    
    # sql <- ifelse(
    #   dt_type()=='付款日期',
    #   paste0("select a.reason, substring(b.paydates, 1, 10) as dt, a.amount from tb_porder_drawback a inner join tb_porder b on a.oid=b.oid where substring(b.paydates, 1, 10) >= '",dt()[1],"' and substring(b.paydates, 1, 10)<='",dt()[2],"' and a.reason not in ('Логистика и распределение проблемы', 'Из Продавцы') and a.dualstatus=2"),
    #   paste0("select a.reason, substring(a.dualdates, 1, 10) as dt, a.amount from tb_porder_drawback a inner join tb_porder b on a.oid=b.oid where substring(b.paydates, 1, 10) >= '",dt()[1],"' and substring(b.paydates, 1, 10)<='",dt()[2],"' and a.reason not in ('Логистика и распределение проблемы', 'Из Продавцы') and a.dualstatus=2")
    # )
    
    sql <- ifelse(
      data_type()=='下单时间',
      paste0("SELECT
                DATE_FORMAT(a.POST_DATE_STR, '%Y%m%d') dt,
             count(oid) oids
             FROM
             ecommerce.tb_porder a
             WHERE
             1 = 1
              #and format(a.POST_DATE_STR,'%Y%m%d') >= '",dt()[1],"'
            and a.POST_DATE_STR >= '",dt()[1],"'
            and a.POST_DATE_STR <= '",dt()[2],"'
             GROUP BY
             DATE_FORMAT(a.POST_DATE_STR, '%Y%m%d')"),
      paste0("select 	DATE_FORMAT(dt, '%Y%m%d') dt, count(oid) oids from (
               select  case when a.PAY_STATUS = 1 then a.payDates when a.HANDLE_STATUS not in (5, 7 ) and a.PAY_TYPE = 10002 then a.POST_DATE_STR end dt ,
             oid ,mid 
             from ecommerce.tb_porder a
             where 1 = 1
             and   ( a.PAY_STATUS = 1
             or ( a.HANDLE_STATUS not in (5, 7 ) and a.PAY_TYPE = 10002 )  ) ) x
             where x.dt >= '",dt()[1],"'
             and  x.dt <= '",dt()[2],"'
             GROUP BY
             DATE_FORMAT(dt, '%Y%m%d')")
    )
    
    
    x <- dbGetQuery0('ecommerce', sql)
    
    
    
    # x <- x %>%
      #group_by(reason, dt) %>%
      #summarise(
      #  cnt = n(),
      #  amount = sum(amount)
      #) %>%
      # mutate(
      #   d = substr(dt, 6, 10)
      # ) %>%
      # group_by(reason, d) %>%
      # summarise(
      #   cnt = sum(cnt),
      #   amount = sum(amount)
      # )
    
    x <- x  %>% spread(dt, oids, fill=0)
   
    x$total = rowSums(x[, -1])
    x <- x %>% select(oids, total, everything())
    rownames(x) = `下单量`
    x <- x %>% ungroup() %>% select(-oids)
    x['total', ] = colSums(x)
    x
  })
  
  # export
  output$btn_export <- downloadHandler(paste('bbf-data-callcenter-',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      tmp <- df()[input$tbl_rows_all, , drop = FALSE]
      write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
    }
    
  })
  
  
  output$tbl <- renderDataTable({
    datatable(
      df(),
      rownames = FALSE,
      options = list(
        searching=FALSE,
        lengthChange = FALSE
      )  
    )
  })
  
  
  
}
