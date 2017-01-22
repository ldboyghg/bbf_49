# 仪表盘 网站整体指标
dashboardUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      title = '网站整体指标', status = 'primary', solidHeader = T, width = 12,
    fluidRow(class='toolbar',
      column(4, dateRangeInput(ns('dt'), '支付日期', start = Sys.Date()-7 , end = Sys.Date()-1))
      )
    ),
    br(),
    br(),
    br(),
    fluidRow(
      DT::dataTableOutput(ns('web_total1_sale'))
    ),
    # fluidRow(
    #   box(
    #     title = '网站整体指标', status = 'primary', solidHeader = T, width = 12,
    #       column(4, dateRangeInput(ns('dt'), '支付日期', start = Sys.Date()-months(1)-1, end = Sys.Date()-1)))
    # 
    #   
    #   
    #          #4 商品  销售商品件数/在售商品数/动销商品数/平均单品销售量
    #          #5 店铺  经营店铺数/动销店铺数/平均商品经营店数/平均店铺销售店数/店均单品销售量
    # 
    # ),
    # fluidRow(
    #   dataTableOutput(ns('web_total1_sale'))
    # ),
    fluidRow(
      box(column(12,dygraphOutput(ns('trend1')))),
      box(column(12,dygraphOutput(ns('trend2'))))
    ),
    
    fluidRow(
      box(column(12,dygraphOutput(ns('trend3'))))
    )
    )
  
}


dashboard <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  # 总销售额、总订单数、平均销售额
  df <- reactive({
    sql<-paste0("
                select * from (
                select count(distinct pid) dx_pids,
                sum(quantity) quantity
                from (select case
                when PAY_STATUS = 1 then
                payDates
                when PAY_TYPE = 10002 then
                POST_DATE_STR
                end dt ,
                a.oid,
                b.pid ,
                b.quantity
                from tb_porder a
                join tb_porder_line b on a.oid = b.oid
                where 1 = 1
                and HANDLE_STATUS not in (5, 7) 
                and (PAY_STATUS = 1 or PAY_TYPE = 10002) -- 货到付款
                ) a
                where 1 = 1
                and date(dt) >=  '",dt()[1],"'
                and date(dt) <=  '",dt()[2],"'
                #and date(dt) >= '2016-03-01' 
                #and date(dt) <= '2016-03-31'
                ) x ,
                (
                select count(distinct a.pid ) jy_pids from tb_product_info a
                #join tb_product_sku_info b on a.DID = b.DID
                #join tb_product_spec s on b.sku_spec = s.id
                #join tb_product_base_info c on s.base_info_id = c.id
                where 1 = 1 
                and alive not in (0 ,2 )
                #and POST_DATE <= '2016-09-31'
                and POST_DATE <= '",dt()[2],"' 
                ) y  ,
                (
                SELECT
                sum(sum_price) AS sales_total,
                count(DISTINCT OID) AS count_orders,
                avg(sum_price) AS sales_avg
                FROM
                (
                select case
                when PAY_STATUS = 1 then	payDates
                when PAY_TYPE = 10002 then POST_DATE_STR
                end dt ,
                oid,sum_price
                from tb_porder 
                where 1 = 1
                and HANDLE_STATUS not in (5, 7)
                and (PAY_STATUS = 1 or PAY_TYPE = 10002) -- 货到付款
                ) c
                WHERE
                1 = 1
                And date(dt) >= '",dt()[1],"'
                And date(dt) <= '",dt()[2],"'
                #and date(dt) >= '2016-10-01' 
                #and date(dt) <= '2016-10-30'
                ) z
                
                ")
    dbGetQuery0('ecommerce', sql)%>%
      mutate(
        #oid_rat = round( dd[,2]/sum(dd[,2]))
        sales_total = round(sales_total),
        sales_avg = round(sales_avg)
      )%>%
      select(
        `销售额` = sales_total,
        `订单数量` = count_orders,
        `客单价` = sales_avg,
        `动销pid商品数` = dx_pids,
        `销售商品件数` = quantity,
        `在售pid商品数` = jy_pids
      )
  })
  
  output$web_total1_sale <- DT::renderDataTable({
    datatable(
      df(), 
      rownames = FALSE,
      selection = 'multiple',
      extensions = list(Scroller=list()),
      options = list(
        searching=FALSE,
        lengthChange = FALSE
      )
      
    ) 
  })
  
  # 返回：日期+ 指标
  df3 <- reactive({
    sql<-paste0("
                select  date(dt) as dt1,sum(sum_price) as sales_total,count(DISTINCT OID ) as count_orders,
                avg(sum_price) as sales_avg
                from 
                (
                select case
                when PAY_STATUS = 1 then	payDates
                when PAY_TYPE = 10002 then POST_DATE_STR
                end dt ,
                oid,sum_price
                from tb_porder 
                where 1 = 1
                and HANDLE_STATUS not in (5, 7) 
                and (PAY_STATUS = 1 or PAY_TYPE = 10002) -- 货到付款
                ) c
                where 1=1
                and date(dt)>='",dt()[1],"' 
                and date(dt)<='",dt()[2],"'
                #and date(dt) >= '2016-10-01' 
                #and date(dt) <= '2016-10-30'
                group by dt1
                ")
    
    dbGetQuery0('ecommerce', sql) %>% 
      mutate(dt1 = as.Date(dt1))
  })
  
  # 做曲线图:销售额曲线
  
  output$trend1 <- renderDygraph({
    x <- df3() %>% select(dt1,sales_total)
    x <- as.xts(x[, 2], x[, 1])
    dygraph(x,"销售额趋势图")%>%
      dyRangeSelector(height = 20, strokeColor = "") 
  })
  # 做曲线图:订单数量曲线  count_orders
  output$trend2 <- renderDygraph({
    x <- df3() %>% select(dt1,count_orders)
    x <- as.xts(x[, 2], x[, 1])
    dygraph(x,"订单量趋势图")%>%
      dyRangeSelector(height = 20, strokeColor = "") 
  })
  
  # 做曲线图：客单价曲线 sales_avg
  output$trend3 <- renderDygraph({
    x <- df3() %>% select(dt1,sales_avg)
    x <- as.xts(x[, 2], x[, 1])
    dygraph(x,"平均客单价趋势图")%>%
      dyRangeSelector(height = 20, strokeColor = "")  
  })
  
  
} 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 仪表盘
# dashboardUI <- function(id) {
#     ns <- NS(id)
#     
#     tagList(
#         fluidRow(
#             valueBoxOutput(ns('box1_1'), width = 3)
#         ),
#       
#         fluidRow(
#             box(title = '日销售额趋势',
#                 status = 'primary',
#                 dygraphOutput(ns('trend1'))
#             )
#         ) 
#     )
# }
# dashboard <- function(input, output, session) {
#     kpi1 <- reactive({
#         as.integer(dbGetQuery0('bbf_shiny', paste0("SELECT kpi FROM bbf_kpi WHERE isback=1 and date(dt)='", substr(Sys.Date(), 1, 7), "'")))
#     })
#      
#     output$box1_1 = renderValueBox({
#         valueBox(kpi1(), paste0(month(Sys.Date()),'月发展部目标'), color='light-blue', icon = icon('flag'), width = 4)
#     }) 
#      
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     sale_trend <- reactive({
#         if(as.Date(file.mtime('./data/sale_trend.rds'))==Sys.Date()) {
#             tmp <- read_rds('./data/sale_trend.rds')
#         } else {
#             tmp <- dbGetQuery0('ecommerce', paste0("select substring(post_date_str, 1, 10) as dt, sum(actual_pay) as pay, sum(case when isback=1 or isback=5 then actual_pay else 0 end) as pay_crm from tb_porder where (pay_status=1 or (payname='货到付款' and handle_status not in (5,7,97))) and date(POST_DATE_STR)>='2015-01-01' and date(POST_DATE_STR)<'",Sys.Date(),"' group by dt")) %>% 
#                 mutate(
#                     dt = as.Date(dt),
#                     pay = pay/10000,
#                     pay_crm = pay_crm/10000,
#                     pct = pay_crm/pay*100
#                 )
#             saveRDS(tmp, './data/sale_trend.rds')
#         }
#         tmp
#     })
#      
#     output$trend1 <- renderDygraph({
#         x <- sale_trend() %>% select(`日期`=dt, `平台`=pay, `客服`=pay_crm)
#         x <- as.xts(x[, 2:3], x[, 1])
#         dygraph(x, group='dashboard') %>%
#             dyAxis("y", label = "销售额/万") %>% 
#             dyEvent("2015-11-11", "11.11", labelLoc = "bottom") %>% 
#             dyEvent("2016-03-08", "3.8", labelLoc = "bottom")
#     }) 
# }
#       
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 帐号表
userTableUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    column(
      4,
      titlePanel("账号表"),
      helpText(icon('warning'), br(), '1. 此表由开发更新', br(), ''),
      div(
        actionButton(ns("btn_save"), "保 存", icon = icon('save')),
        actionButton(ns("btn_reload"), "刷 新", icon = icon('refresh'))
      )
    ),
    column(
      8,
      rHandsontableOutput(ns("hot"))
    )
  )
}
userTable <- function(input, output, session) {
  values = list()
  setHot = function(x) {
    values[["hot"]] <<- x
  }
  
  observeEvent(input$btn_save, {
    if (!is.null(values[["hot"]])) {
      dbGetQuery0('bbf_shiny', "delete from bbf_user")
      dbWriteTable0('bbf_shiny', 'bbf_user', values[["hot"]])
      shinyjs::info('更新完成')
    }
  })
  observeEvent(input$btn_reload, {
    reset(('hot'))
  })
  
  output$hot = renderRHandsontable({
    if (!is.null(input$hot)) {
      df = hot_to_r(input$hot)
    } else {
      df = dbReadTable0('bbf_shiny', 'bbf_user')
    }
    
    setHot(df)
    rhandsontable(df) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
} 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 菜单表
menuTableUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    column(
      4,
      titlePanel("菜单表"),
      helpText(icon('warning'), br(), '1. 此表由开发更新', br(), '2. 菜单名更新会导致菜单无法访问'),
      div(
        actionButton(ns("btn_save"), "保 存", icon = icon('save')),
        actionButton(ns("btn_reload"), "刷 新", icon = icon('refresh'))
      )
    ),
    column(
      8,
      rHandsontableOutput(ns("hot"))
    )
  )
}
menuTable <- function(input, output, session) {
  values = list()
  setHot = function(x) {
    values[["hot"]] <<- x
  }
  
  observeEvent(input$btn_save, {
    if (!is.null(values[["hot"]])) {
      dbGetQuery0('bbf_shiny', "delete from bbf_menu")
      dbWriteTable0('bbf_shiny', 'bbf_menu', values[["hot"]])
      shinyjs::info('更新完成')
    }
  })
  observeEvent(input$btn_reload, {
    reset(('hot'))
  })
  output$hot = renderRHandsontable({
    if (!is.null(input$hot)) {
      df = hot_to_r(input$hot)
    } else {
      df = dbReadTable0('bbf_shiny', 'bbf_menu') %>% arrange(menu1)
    }
    
    setHot(df)
    rhandsontable(df) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
}


#------------------------分割线--------------------
# 退款原因
drawbackReasonReportUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(class='toolbar',
             column(2, radioButtons(ns('data_type'), '数据类型', choices = c('退款单数', '退款金额'), inline = TRUE)),
             column(2, radioButtons(ns('dt_type'), '日期类型', choices = c('付款日期', '处理日期'), inline = TRUE)),
             column(3, dateRangeInput(ns('dt'), '日期', start = '2016-03-01', end = '2016-03-31', weekstart = 5, language = 'zh_CN'))
    ),
    fluidRow(
      dataTableOutput(ns('tbl'))
    )
  )
}
drawbackReasonReport <- function(input, output, session){
  data_type <- reactive({
    req(input$data_type)
    input$data_type
  })
  dt_type <- reactive({
    req(input$dt_type)
    input$dt_type
  })
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  df <- reactive({
    sql <- ifelse(
      dt_type()=='付款日期',
      paste0("select a.reason, substring(b.paydates, 1, 10) as dt, a.amount from tb_porder_drawback a inner join tb_porder b on a.oid=b.oid where substring(b.paydates, 1, 10) >= '",dt()[1],"' and substring(b.paydates, 1, 10)<='",dt()[2],"' and a.reason not in ('Логистика и распределение проблемы', 'Из Продавцы') and a.dualstatus=2"),
      paste0("select a.reason, substring(a.dualdates, 1, 10) as dt, a.amount from tb_porder_drawback a inner join tb_porder b on a.oid=b.oid where substring(b.paydates, 1, 10) >= '",dt()[1],"' and substring(b.paydates, 1, 10)<='",dt()[2],"' and a.reason not in ('Логистика и распределение проблемы', 'Из Продавцы') and a.dualstatus=2")
    )
    x <- dbGetQuery0('ecommerce', sql)
    x <- x %>% 
      group_by(reason, dt) %>% 
      summarise(
        cnt = n(),
        amount = sum(amount)
      ) %>% 
      mutate(
        d = substr(dt, 6, 10)
      ) %>% 
      group_by(reason, d) %>% 
      summarise(
        cnt = sum(cnt),
        amount = sum(amount)
      )
    
    
    #行转列
    #1.数据获取
    #df <- dbGetQuery0(db,sql )
    #2.数据清洗转换汇总  spread 行转列
    #df <- df %>% group_by(reason,dt) %>% summarise(cnt = n(),amount = sum(amount)) %>% mutate( d = substr(dt,6,10)) %>% group_by(reason,d) %>% summarise(cnt = sum(cnt),amount = sum(amount)) %>% select(-amount) %>% spread(d, cnt, fill=0) 
    #3.列汇总 ，除了第一列(##第一列字符串reason)
    #df$total = rowSums(df[, -1])
    #4.列排序,将汇总列total放到前面来
    #df <- df %>% select(reason ,total,everything())
    #5.给行名
    #rownames(df) = df$reason
    #6.取消分组(如果不执行这个ungroup 命令 ， 使用的  %>% select(-reason) 的时候， 报错：ERROR 'x' must be numeric)
    # df <- df %>% ungroup() 
    #7.删除reason列(目的是为了后面的行汇总，因为reason为字符串列，不能使用colSums()函数)
    # df <- select(-reason)
    #8.添加行汇总
    #df['total',] = colSums(df)
    if(data_type()== '退款单数'){
      x <- x %>% select(-amount) %>% spread(d, cnt, fill=0)
    } else {
      x <- x %>% select(-cnt) %>% spread(d, amount, fill=0)
    }
    x$total = rowSums(x[, -1])
    x <- x %>% select(reason, total, everything())
    rownames(x) = x$reason 
    x <- x %>% ungroup() %>% select(-reason)
    x['total', ] = colSums(x)
    x
  })
  
  output$tbl <- renderDataTable({
    datatable(
      df(),
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
      )  # options end
    )
  })
}




#------------------------八百方运营数据报表-------------------
bbfYyDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '付款时间',  start =  floor_date(Sys.Date(), "month") , end = Sys.Date()-1)),
             column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
    ),
    fluidRow(
      column(12,dataTableOutput(ns('tbl')))  
    )
  )
}
bbfYyData <- function(input, output, session){
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  #and date(dt) >= '",pay_date()[1],"'
  #and date(dt) <= '",pay_date()[2],"'
  
  
  df <- reactive({
    sql <-
      paste0(" 
             select * from bbf_yy_data_daily 
             where date(dt) >= '",pay_date()[1],"'
             and date(dt) <= '",pay_date()[2],"' 
             ")
    dd <- dbGetQuery0('bbf_shiny',sql)
    
    
    # %>%mutate(
    #   mid_rat = dd[,2]/sum(dd[,2]),
    #   amount_rat = dd[,3]/sum(dd[,3]),
    #   per_price = round(dd[,3]/dd[,5],2)
    # ) 
    
    df <- dd %>% dplyr::rename(`日期` = dt,
                               `星期` = weekday,
                               `UV` = UV,
                               `PV` = PV,
                               `小能有效咨询量` = consult,
                               `呼入接通数` = severals,
                               `访问次数` = visit_times,
                               `人均PV` = pv_per,
                               `注册会员` = zhuces,
                               `注册转化率` = zhuce_rat,
                               `新访客数` = new_visits,
                               `跳出率` = exit_rat,
                               `平均停留时间` = duration,
                               `金额合计(在线+货付)` = amount,
                               `成交订单合计（在线+货付）` = oids,
                               `成交订单(在线支付)` = oids1,
                               `成交金额(在线支付)` = amount1,
                               `成交订单(货到付款)` = oids2,
                               `成交金额(货到付款)` = amount2,
                               `客单价` = cur_oid_price,
                               `总体订单转化率` = postrat,
                               `成交订单转化率` = payrat,
                               `支付率` = payrat1
    )   
    
  }) 
  output$btn_export <- downloadHandler(paste('bbf-totalday-',Sys.Date(),'.csv',sep=''), content = function(file) {
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
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      extensions = c('FixedColumns', 'Scroller'),
      #extensions = list(FixedColumns = list(leftColumns = 2), Scroller=list()),
      #extensions = 'FixedColumns',
      options = list(
        autoWidth = TRUE,
        columnDefs = list(list(width = '60px', targets = c(1,2))),
        searching=TRUE,
        deferRender=TRUE,
        scrollY=200,
        scrollX=TRUE,
        scrollCollapse = TRUE,
        fixedColumns = list(leftColumns = 1),
        order = list(list(0, 'asc')),
        #language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 100,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    )  %>%  # datatable end
      formatPercentage(c('总体订单转化率','成交订单转化率','支付率','注册转化率'), 2) %>%
      formatCurrency(c('金额合计(在线+货付)', '成交金额(在线支付)','成交金额(货到付款)','客单价'),currency = '￥')
  })
  
  
  # output$age <- renderREcharts3({
  #   # dat1 = aggregate(weight ~ feed, data = chickwts, mean)
  #   # pie(dat1, feed, weight, label = round(weight*10, 0), title = 'Pie Plot')
  #   # donut(dat1, feed, weight, title = 'Pie Plot')
  #   pie(df(),`年龄`,`销售额`, title = '销售额', height = 400)
  #   #p02
  #   
  # })
  
}




