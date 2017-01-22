# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 活动评估  这里先做一个根据商品查询客户购买订单导出
# 根据商品通用名查询
marketingEvaluationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('dt'), '成单时间', start = Sys.Date()-8, end = Sys.Date()-1)),
             column(3, textInput(ns('txt'), '商品名(可从excel黏贴或用空格隔开)', value = '金戈')),
             #column(3, textInput(ns('txt1'), '商品pid(可从excel黏贴或用空格隔开)', value = '1251860')),
             column(2, downloadButton(ns('btn_export'), '导出购买客户清单表', class = 'btn_nowrap'))

    ),
    br(),
    br(),
    fluidRow(
      box(DT::dataTableOutput(ns('tbl')))
    )
  )
}
marketingEvaluation <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  txt <- reactive({
    req(input$txt)
    stringr::str_replace_all(input$txt, " ", "','")
  })
  # txt1 <- reactive({
  #   req(input$txt1)
  #   stringr::str_replace_all(input$txt1, " ", "','")
  # })
  df <- reactive({
    dbGetQuery0('ecommerce', paste0("
      SELECT
           t.mid,date(post_date_str) as order_time,  concat('''',t.orderno) orderno  ,t.linkman, substr(t.name,1,3) name,t.mobile,b.name proName
           FROM
           tb_porder t
            JOIN tb_porder_line b ON t.oid = b.oid
           WHERE
             1 = 1
             and HANDLE_STATUS NOT IN (5, 7)
             AND (
               (
               PAY_STATUS = 1
               AND date(payDates) >= '",dt()[1],"'  
               AND date(payDates) <= '",dt()[2],"'
               )
               OR (
                date(POST_DATE_STR) >= '",dt()[1],"'  
               AND date(POST_DATE_STR) <= '",dt()[2],"'
               AND PAY_TYPE = 10002
               )
             )
          AND  b.name  in ('",txt(),"') 
              
            
     "))%>% 
      rename(`商品名`=proName ,`会员编号`=mid, `下单时间`=order_time,`订单号`=orderno,`下单人`=linkman, `收货人`=name,`收货人电话`=mobile)
  })
  # use " " to seperate 
  # and t.mid in ('",txt(),"') 
  # or b.pid in ('",txt1(),"')   txt1 或者的选项还是不行。
  # and b.proName in ('')  通用名
  # and t.merchant_name in ('')  
  
  # export
  output$btn_export <- downloadHandler(paste('bbf_customer_list',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      #tmp <- df()[input$tbl_rows_all, , drop = FALSE]
      write.csv(df(), file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
    }
  })
  
  output$tbl <- DT::renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'single',
      #extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      extensions = list(Scroller=list()),
       options = list(
      #   autoWidth = TRUE,
      #   columnDefs = list(list(width = '50px', targets = c(1))),
      #   searching=TRUE,
      #   deferRender=TRUE,
      #   scrollY=100,
         scrollX=TRUE,
         scrollCollapse = TRUE,
      #   order = list(list(10, 'desc')),
      #   language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
         pageLength = 20
      #   lengthChange = FALSE,
      #   initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-200)+'px !important');}"),
      #   fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-200;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
       )
    )
  })
  }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 销售品种数趋势

saleskuTrendsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(title = '销售品种数趋势', status = 'primary', solidHeader = T, width = 12,
      fluidRow(class='toolbar',
               column(4, dateRangeInput(ns('dt'), '支付日期', start = Sys.Date()-8 , end = Sys.Date()-1))
             )
      ,
      fluidRow(
        navbarPage("趋势图与表!",
                 tabPanel("Plot", 
                          fluidRow(
                            column(2,radioButtons(ns('field'), '指标',
                                                  choices = c('通用商品名数'
                                                             ,'商品名数'
                                                             ,'pid数'
                                                             ,'商家数'
                                                             ,'销售量'
                                                             ,'销售额'
                                                             ,'成单数'
                                                             ,'购买人数'
                                                             ,'客单价')
                            )
                            ),
                            #column(10,dygraphOutput(ns('trend1')))
                            column(10,plotlyOutput(ns('trend1')))
                            )

                          )
                 ,
                 tabPanel("Summary",dataTableOutput(ns('daysales'))   )
      )
      )
      
      
      # fluidRow(
      #   column(10,dygraphOutput(ns('trend1')))  ,
      #   column(6,dataTableOutput(ns('daysales')))
      #)
  )
  )
}


saleskuTrends <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  field <- reactive({
    req(input$field)
    input$field
  })
  
  # 总销售额、总订单数、平均销售额
  #可以根据指标画曲线，用plotly作图
  
  df <- reactive({
    sql<-paste0("
                select date(dt) as paydate,
                count(distinct proname) as proname_counts,
                count(distinct NAME) as name_counts,
                count(distinct pid) as pid_counts,
                COUNT(DISTINCT merchant_name) as merchant_counts,
                sum(quantity) as quantity,	
                ROUND(sum(amount)) as amounts,	
                count(distinct oid) as oid_counts,	
                count(distinct mid) as mid_counts,
                ROUND(sum(amount)/	count(distinct oid)) as oid_pay
                from (
                select a.pid,a.name,a.proname,a.oid,a.quantity,a.amount,b.dt,b.merchant_name,b.mid
                from tb_porder_line a
                INNER JOIN 
                (
                SELECT 
                case
                when PAY_STATUS = 1 then payDates
                when PAY_TYPE = 10002 then POST_DATE_STR
                end dt,
                oid,
                merchant_name,
                mid
                FROM  tb_porder
                where 1 = 1
                and HANDLE_STATUS not in (5, 7)
                and (PAY_STATUS = 1 or  PAY_TYPE = 10002)
                ) b
                ON a.oid = b.oid
                WHERE 1=1
                and date(b.dt)>='",dt()[1],"' 
                and date(b.dt)<='",dt()[2],"'
                ) x
                group by date(dt) 
                ")
    dbGetQuery0('ecommerce', sql)%>% 
           mutate(paydate = as.Date(paydate)) %>%
    select(
      `日期` = paydate,
      `通用商品名数` = proname_counts,
      `商品名数` = name_counts,
      `pid数` = pid_counts,
      `商家数` = merchant_counts,
      `销售量` = quantity,
      `销售额` = amounts,
      `成单数` = oid_counts,
      `购买人数` = mid_counts,
      `客单价` = oid_pay
    )
  })
  # AND date(b.dt) >= '2016-10-18'
  # AND date(b.dt) <= '2016-10-31'
  output$daysales <- DT::renderDataTable({
    datatable(
      df(), 
      rownames = FALSE,
      selection = 'multiple',
      extensions = list(Scroller=list()),
      options = list(
        searching=TRUE,
        lengthChange = TRUE
      )
    )
  })
  # 返回：日期+ 指标
  # 做曲线图:销售额曲线。并且可以改变标签。
  # output$trend1 <- renderDygraph({
  #   x <- df()
  #   x1<-x%>% select(`日期`,field())
  #   x1 <- as.xts(x[,2], x[,1])
  #   #x1 <- as.xts(x$field(), x$`日期`)
  #   dygraph(x1,"销售品种趋势图")%>%
  #     dyRangeSelector(height = 20, strokeColor = "")
  # })
  
  output$trend1 <- renderPlotly({
    x<-df()
    #plot_ly(x1, x = ~`日期`) %>%add_lines(y = ~ 商家数)%>% add_markers(y = ~商家数)
      #add_lines(y = ~ field())%>% add_markers(y = ~field())
    
    p <- x%>% ggplot(aes_string(x[,1], field()))+geom_point(aes(color='2'))+geom_line(aes(color='4'))
    #geom_smooth()+theme(legend.position='none')
    #
    ggplotly(p)
  })

} 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 商品活动评估
productMarketingvalUI <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel('商品活动评估'),
    
    fluidRow(class='toolbar',
             column(9,helpText('1.可以根据选择查询时间段与对比时间段，查看对应数据',
                               '  2.表中字段写对比的表示对比期的数据',
                               '  3.pid明细表是基于查询期pid跟踪对应销售及对比期访问与销售', 
                               '  4.请注意若选择时间跨度大，等待时间会加长！')
             )
    ),
    
    fluidRow(
      class='toolbar',
      column(3, dateRangeInput(ns('dt1'), '查询日期', start = Sys.Date()-2, end=Sys.Date()-1)),
      column(3, dateRangeInput(ns('dt2'), '对比日期', start = Sys.Date()-4, end=Sys.Date()-3)),
      column(3, textInput(ns('txt'), '商品pid(Excel直接复制粘贴或空格隔开)', value = '717254 476644 1287505 1310174 932618 1317931 717837 907900 747353'))
      #column(2, downloadButton(ns('btn_export'), '汇总对比结果导出', class = 'btn_nowrap'))
    ),
    br(),
    br(),
    fluidRow(
      column(12, DT::dataTableOutput(ns('tbl')))
    ),
    br(),
    box(title = 'pid浏览与销售对比', status = 'primary', solidHeader = T, width = 12,
      fluidRow(
        column(2, downloadButton(ns('btn_export1'), 'pid对比结果导出', class = 'btn_nowrap'))
      ),
      br(),
      br(),
      fluidRow(
        column(12, DT::dataTableOutput(ns('tbl1')))
        )
    )
  )
}
productMarketingval <- function(input, output, session) {
  dt1 <- reactive({
    req(input$dt1)
    input$dt1
  })
  dt2 <- reactive({
    req(input$dt2)
    input$dt2
  })
  txt <- reactive({
    req(input$txt)
    stringr::str_replace_all(input$txt, " ", "','")
  })
  df <- reactive({
    dbGetQuery0("ecommerce", paste0("

select 
                                    x.seepids,x1.pids as buypids,
                                    IFNULL(x.pv,0) as pv1,IFNULL(y.pv,0) as pv2, IFNULL((x.pv - y.pv)/y.pv,0) as pv_growthrate,
                                    IFNULL(x.uv,0) as uv1,IFNULL(y.uv,0) as uv2, IFNULL((x.uv - y.uv)/y.uv,0) as uv_growthrate,
                                    x1.sales1,y1.sales2,IFNULL((x1.sales1 - y1.sales2)/y1.sales2,0) as sales_growthrate,
                                    x1.oid_cnts1,y1.oid_cnts2 ,IFNULL((x1.oid_cnts1 - y1.oid_cnts2)/y1.oid_cnts2,0) as oids_growthrate,
                                    IFNULL(x1.oid_cnts1/x.uv,0) as turn_rate1, IFNULL(y1.oid_cnts2/y.uv,0) as turn_rate2 
                                    from (
                                    (
                                    select count(DISTINCT cal_Pid) as seepids,sum(pv) as pv , sum(uv) as uv  from   bbf_shiny.mms_pid_date_2016 a
                                    where 1=1
                                    and date(dt)>='",dt1()[1],"'  
                                    and date(dt)<='",dt1()[2],"' 
                                    and cal_Pid in ('",txt(),"') 
                                    ) x 
                                    , (select count(DISTINCT cal_Pid) as seepids,sum(pv) as pv , sum(uv) as uv from   bbf_shiny.mms_pid_date_2016 a
                                    where 1=1
                                    and date(dt)>='",dt2()[1],"' 
                                    and date(dt)<='",dt2()[2],"' 
                                    and cal_Pid in ('",txt(),"')
                                    ) y
                                    ,
                                    (SELECT count(distinct a.pid) as pids,sum(a.amount) as sales1, count(DISTINCT b.oid) as oid_cnts1 from tb_porder_line a
                                    join (
                                    select 	
                                    CASE
                                    WHEN PAY_STATUS = 1 THEN	payDates
                                    WHEN  PAY_TYPE = 10002 THEN POST_DATE_STR
                                    END dt,
                                    oid 
                                    from tb_porder
                                    WHERE	1 = 1
                                    and HANDLE_STATUS NOT IN (5, 7) 
                                    AND (	PAY_STATUS = 1	OR PAY_TYPE = 10002)
                                    ) b
                                    on a.oid=b.oid
                                    where 1=1
                                    and date(b.dt)>='",dt1()[1],"' 
                                    and date(b.dt)<='",dt1()[2],"' 
                                    and pid in ('",txt(),"')
                                    ) x1
                                    ,
                                    (SELECT count(a.pid) as pids,sum(a.amount) as sales2, count(DISTINCT b.oid) as oid_cnts2 from tb_porder_line a
                                    join (
                                    select 	
                                    CASE
                                    WHEN PAY_STATUS = 1 THEN	payDates
                                    WHEN PAY_TYPE = 10002 THEN POST_DATE_STR
                                    END dt,
                                    oid 
                                    from tb_porder
                                    WHERE	1 = 1
                                    and HANDLE_STATUS NOT IN (5, 7) 
                                    AND (	PAY_STATUS = 1	OR PAY_TYPE = 10002)
                                    ) b
                                    on a.oid=b.oid
                                    where 1=1
                                    and date(b.dt)>='",dt2()[1],"' 
                                    and date(b.dt)<='",dt2()[2],"' 
                                    and pid in ('",txt(),"')
                                    ) y1
                                    ) 
                                    ")) %>%
      mutate(pv_growthrate=round(pv_growthrate,4),
             uv_growthrate=round(uv_growthrate,4),
             sales_growthrate=round(sales_growthrate,4),
             oids_growthrate=round(oids_growthrate,4),
             oid_cnts1=round(oid_cnts1,4),
             oid_cnts2=round(oid_cnts2,4)
             ) %>%
      select(
        `浏览pid数` = seepids,
        `购买pid数` = buypids,
        `PV` = pv1,
        `对比PV` = pv2,
        `PV增长率` = pv_growthrate,
        `UV` = uv1,
        `对比UV` = uv2,
        `UV增长率` = uv_growthrate,
        `销售额` = sales1	,
        `对比销售额` = sales2 ,
        `销售额增长率` = sales_growthrate	,
        `订单数` = oid_cnts1	,
        `对比订单数` = oid_cnts2	,
        `订单增长率` = oids_growthrate	,
        `转化率` = turn_rate1	,
        `对比期转化率` = turn_rate2
      )
 })
  # export
  # output$btn_export <- downloadHandler(paste('bbf-productMarketingval-',Sys.Date(),'.csv',sep=''), content = function(file) {
  #   if(TRUE){
  #     tmp <- df()[input$tbl_rows_all, , drop = FALSE]
  #     write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
  #   } else {
  #     write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
  #   }
  #   
  # })
  output$tbl <- renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      extensions = list(Scroller=list()),
      options = list(
        autoWidth = TRUE,
        searching=TRUE,
        deferRender=TRUE,
        scrollX=TRUE,
        scrollY=200,
        scrollCollapse = TRUE,
        #order = list(list(0, 'asc')),
        pageLength = 200,
        lengthChange = FALSE
        )
    )
  })
  
  # 输出单品表
  df1 <- reactive({
    dbGetQuery0("ecommerce", paste0("
      SELECT cal_pid as seepids,
      IFNULL(sum(pv1),0) as pv1 , IFNULL(sum(pv2),0) as pv2 , IFNULL((sum(pv1)-sum(pv2))/sum(pv2),0) as pv_growthrate,
      IFNULL(sum(uv1),0) as uv1, IFNULL(sum(uv2),0) as uv2, IFNULL((sum(uv1) -sum(uv2))/sum(uv2),0) as uv_growthrate,
      sum(sales1) as sales1,sum(sales2) as sales2,IFNULL((sum(sales1) -sum(sales2))/sum(sales2),0) as sales_growthrate,
      sum(oid_cnts1) as oid_cnts1,sum(oid_cnts2) as oid_cnts2 ,IFNULL((sum(oid_cnts1) -sum(oid_cnts2))/sum(oid_cnts2),0) as oids_growthrate,
      IFNULL(sum(oid_cnts1)/sum(uv1),0) as turn_rate1,IFNULL(sum(oid_cnts2)/sum(uv2),0) as turn_rate2
      from (
      select x.cal_pid ,x1.pid,IFNULL(x.pv,0) pv1, IFNULL(y.pv,0) pv2 , IFNULL(x.uv,0) uv1 , IFNULL(y.uv,0) uv2, 
      x1.sales1,y1.sales2,x1.oid_cnts1,y1.oid_cnts2 
      
      from (
      select cal_pid , sum(pv) as pv , sum(uv) as uv  from   bbf_shiny.mms_pid_date_2016 a
      where 1=1
      and date(dt)>='",dt1()[1],"'
      and date(dt)<='",dt1()[2],"'
      and cal_Pid in ('",txt(),"')
      GROUP BY cal_pid
      ) x
      left  join (select cal_Pid , sum(pv) as pv , sum(uv) as uv from   bbf_shiny.mms_pid_date_2016 a
      where 1=1
      and date(dt)>='",dt2()[1],"'
      and date(dt)<='",dt2()[2],"'
      and cal_Pid in ('",txt(),"')
      GROUP BY cal_pid
      ) y
      on x.cal_pid = y.cal_pid
      LEFT join 
      (
      SELECT a.pid,sum(a.amount) as sales1, count(DISTINCT b.oid) as oid_cnts1 from tb_porder_line a
      join (
      select 	
      CASE
      WHEN PAY_STATUS = 1 THEN	payDates
      WHEN PAY_TYPE = 10002 THEN POST_DATE_STR
      END dt,
      oid 
      from tb_porder
      WHERE	1 = 1
      and HANDLE_STATUS NOT IN (5, 7) 
      AND (	PAY_STATUS = 1	OR   PAY_TYPE = 10002)
      ) b
      on a.oid=b.oid
      where 1=1
      and date(b.dt)>='",dt1()[1],"'
      and date(b.dt)<='",dt1()[2],"'
      and pid in ('",txt(),"')
      GROUP BY a.pid
      ) x1
      on x.cal_pid=x1.pid
      LEFT JOIN (
      SELECT a.pid,sum(a.amount) as sales2, count(DISTINCT b.oid) as oid_cnts2 from tb_porder_line a
      join (
      select 	
      CASE
      WHEN PAY_STATUS = 1 THEN	payDates
      WHEN PAY_TYPE = 10002 THEN POST_DATE_STR
      END dt,
      oid 
      from tb_porder
      WHERE	1 = 1
      and HANDLE_STATUS NOT IN (5, 7) 
      AND (	PAY_STATUS = 1	OR PAY_TYPE = 10002)
      ) b
      on a.oid=b.oid
      where 1=1
      and date(b.dt)>='",dt2()[1],"'
      and date(b.dt)<='",dt2()[2],"'
      and pid in ('",txt(),"')
      GROUP BY a.pid
      ) y1
      on x.cal_pid=y1.pid
      ) z
      GROUP BY cal_pid
      ")) %>%
      mutate(pv_growthrate=round(pv_growthrate,4),
             uv_growthrate=round(uv_growthrate,4),
             sales_growthrate=round(sales_growthrate,4),
             oids_growthrate=round(oids_growthrate,4),
             oid_cnts1=round(oid_cnts1,4),
             oid_cnts2=round(oid_cnts2,4)
      ) %>%
      select(
        `pid` = seepids,
        `PV` = pv1,
        `对比PV` = pv2,
        `PV增长率` = pv_growthrate,
        `UV` = uv1,
        `对比UV` = uv2,
        `UV增长率` = uv_growthrate,
        `销售额` = sales1	,
        `对比销售额` = sales2 ,
        `销售额增长率` = sales_growthrate	,
        `订单数` = oid_cnts1	,
        `对比订单数` = oid_cnts2	,
        `订单增长率` = oids_growthrate,
        `转化率` = turn_rate1	,
        `对比期转化率` = turn_rate2
      )
  })
  # export
  output$btn_export1 <- downloadHandler(paste('bbf-activeval-pid',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      #tmp <- df1()[input$tbl_rows_all, , drop = FALSE]
      tmp <- df1()
      write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
    }
  })
  
  output$tbl1 <- renderDataTable({
    datatable(
      df1(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      extensions = list(Scroller=list()),
      options = list(
        autoWidth = TRUE,
        searching=TRUE,
        deferRender=TRUE,
        scrollX=TRUE,
        scrollY=200,
        scrollCollapse = TRUE,
        order = list(list(4, 'asc')),
        pageLength = 200,
        lengthChange = FALSE
      )
    )
  })
  
  
  
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 小时销售曲线
salesHourUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(title = '日销售曲线', status = 'primary', solidHeader = T, width = 12,
        fluidRow(class='toolbar',
                 column(3, dateInput(ns('dt'), '成单时间', Sys.Date()-1))
        ),
        fluidRow(
          column(8, plotlyOutput(ns('trend1')) ),
          column(4, dataTableOutput(ns('tb1')) )
        )
    )
  )
}
salesHour <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  df <- reactive({
    
    sql <- paste0("
                  select DATE_FORMAT(b.dt,'%H') h,round(sum(sum_price)) amount2 , count(oid) oid2 
                  from (
                  select 
                  case
                  when PAY_STATUS = 1 then
                  payDates
                  when PAY_TYPE = 10002 then
                  POST_DATE_STR
                  end dt , sum_price ,oid 
                  from tb_porder a
                  where
                  HANDLE_STATUS NOT IN (5, 7) 
                   and  (PAY_STATUS = 1 or PAY_TYPE = 10002 ) -- 货到付款
                  ) b
                  where 1 = 1 
                  and date(dt) = '",dt(),"'
                  group by DATE_FORMAT(b.dt,'%H')
                  order by DATE_FORMAT(b.dt,'%H') 
                  ")
    x <- dbGetQuery0('ecommerce', sql)
    x1<-x%>%
      rename(`小时`=h , `销售额`=amount2,`订单数量`=oid2)
    x1
  })
  
  
  output$trend1 <- renderPlotly({
    x1 <- df()
    plot_ly(x1, x = ~`小时`) %>%
      add_lines(y = ~`销售额`)%>% add_markers(y = ~`销售额`)
    # plot_ly(x1, x = ~x1[,1]) %>%
    #      add_lines(y = ~x1[,2]) %>% add_markers(y = ~x1[,2])
  })
  output$tb1 <- DT::renderDataTable({
    datatable(
      df(), 
      rownames = FALSE,
      selection = 'multiple',
      extensions = list(Scroller=list()),
      options = list(
        searching=FALSE,
        lengthChange = TRUE,
        pageLength = 24
        
      )
    )
  })
  
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 货到付款订单趋势
payFacetofaceUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(title = '货到付款订单趋势', status = 'primary', solidHeader = T, width = 12,
        fluidRow(class='toolbar',
                 column(3, dateInput(ns('dt'), '下单时间', Sys.Date()-1))
        ),
        fluidRow(
          column(8, plotlyOutput(ns('trend1')) ),
          column(4,dataTableOutput(ns('tb1'))  )
        )
    )
  )
}
payFacetoface <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  df <- reactive({
    
    sql <- paste0("
                 select case
                 when adminaccount<>'' then 0 else    1
                  end type,
                  DATE_FORMAT(post_date_str,'%H') as h,
                  count(oid) oid2,sum(SUM_PRICE) as amount2
                  from tb_porder a
                  where 1 = 1
                  and date(post_date_str) = '",dt(),"' 
                  group by case
                  when adminaccount<>'' then 0 else    1
                  end,
                  DATE_FORMAT(post_date_str,'%H')
                  ")
    x <- dbGetQuery0('ecommerce', sql)
    x1<-x%>%
    select(`下单类型`=type ,`小时`=h , `销售额`=amount2,`订单数量`=oid2)
    x1
  })

  # and date(post_date_str) = '",dt(),"'  '20161101'
#要增加一个group by type的曲线。
  output$trend1 <- renderPlotly({
    x1 <- df()
    # plot_ly(x1, x = ~`小时`) %>%
    #   add_lines(y = ~`销售额`)%>% add_markers(y = ~`销售额`)
    x1 %>%
      group_by(`下单类型`) %>%
      plot_ly(x = ~`小时`, y = ~`订单数量`) %>%
      add_lines(fill = "black")


  })
  output$tb1 <- DT::renderDataTable({
    datatable(
      df(), 
      rownames = FALSE,
      selection = 'multiple',
      extensions = list(Scroller=list()),
      options = list(
        searching=FALSE,
        lengthChange = TRUE,
        pageLength = 24
       
      )
    )
  })
  
}


# 
# # #------------------------地域销售概况---recharts----------------
salesMapUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-1, end = Sys.Date()-1)),
             #column(2, selectInput(ns('field'), '指标', choices = c('订单数','客户数','销售额','销售量') ))
             column(2, selectInput(ns('field'), '指标', choices =setNames( c("oids", "mids", "amount",'quantity') , c('订单数','客户数','销售额','销售量') ) ))
    ),
    fluidRow(
      column(4,dataTableOutput(ns('tbl'))) ,
      column(8,plotOutput(ns('map')))
      #)
    )

  )
}
salesMap <- function(input, output, session) {

  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  })


  field <- reactive({
    req(input$field)
    input$field
  })

  #and date(dt) >= '",pay_date()[1],"'
  #and date(dt) <= '",pay_date()[2],"'


  df <- reactive({
    sql <-
      paste0("
             select x.PROVINCE_NAME province_name ,
             ifnull(y.oids ,0) oids,
             ifnull(y.mids ,0) mids,
             ifnull(y.amount ,0) amount,
             ifnull(y.quantity ,0) quantity
             from bbf_shiny.tb_param_PROVINCE_NAME x
             left join (
             SELECT
             t.PROVINCE_NAME ,
             count(distinct t.oid) oids ,
             count(distinct t.mid) mids ,
             round(sum(b.amount),0) amount ,
             sum(b.quantity) quantity
             FROM
             (select oid , mid ,
             case when PROVINCE_NAME like '%内蒙古%' then '内蒙古自治区'
             when PROVINCE_NAME like '%宁夏%' then '宁夏回族自治区'
             when PROVINCE_NAME like '%广西%' then '广西壮族自治区'
             when PROVINCE_NAME like '%西藏%' then '西藏自治区'
             when PROVINCE_NAME like '%新疆%' then '新疆维吾尔自治区'
             when PROVINCE_NAME like '%青海%' then '青海省'
             when PROVINCE_NAME like '%青海%' then '青海省'
             else PROVINCE_NAME end PROVINCE_NAME ,PAY_STATUS,payDates ,HANDLE_STATUS,PAY_TYPE , POST_DATE_STR from
             ecommerce.tb_porder )t
             JOIN ecommerce.tb_porder_line b ON t.oid = b.oid
             WHERE
             1 = 1
             AND  HANDLE_STATUS NOT IN (5, 7)
             AND (
             (
             PAY_STATUS = 1
             AND date(payDates) >= '",pay_date()[1],"'
             AND date(payDates) <= '",pay_date()[2],"'
             )
             OR (
             PAY_TYPE = 10002
             AND date(POST_DATE_STR) >= '",pay_date()[1],"'
             AND date(POST_DATE_STR) <= '",pay_date()[2],"'
             )
             )
             group by t.PROVINCE_NAME )  y
             on x.PROVINCE_NAME = y.PROVINCE_NAME
             ")
    dd <- dbGetQuery0('ecommerce',sql)

    #df <- dd %>% rename(`订单数`=oids,`客户数`=mids,`销售额`=amount,`销售量`=quantity)
    df <- dd %>% rename(NAME=province_name)

    df
  })



  output$tbl <- renderDataTable({
    datatable(
      #df() %>% rename(`省份`=NAME),
      df <- df() %>% rename(`省份`=NAME,`订单数`=oids,`客户数`=mids,`销售额`=amount,`销售量`=quantity),
      escape = FALSE,
      rownames = FALSE,
      selection = 'multiple',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        searching=FALSE,
        lengthChange = TRUE
      )
    )
  })


  output$map <- renderPlot({
    print(field())

    china_map<-readShapePoly("/home/crm/BBF/data/bou2_4p.shp")
    x<-china_map@data
    xs<-data.frame(x,id=seq(0:924)-1)
    china_map1<-fortify(china_map) #转化为数据框

    china_mapdata<-join(china_map1, xs, type = "full") #合并两个数据框

    china_mapdata$NAME <-iconv(china_mapdata$NAME,"GBK","UTF-8")

    china_pop<-join(china_mapdata, df(), type = "full")

    ggplot(china_pop, aes(x = long, y = lat, group = group, fill= get(field())  ))+
      geom_polygon( ) +  geom_path(colour = "grey40")

  })

}





# #------------------------地域销售概况---recharts----------------
# salesMapUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     fluidRow(class='toolbar',
#              column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-1, end = Sys.Date()-1)),
#              #column(2, selectInput(ns('field'), '指标', choices = c('订单数','客户数','销售额','销售量') ))
#              column(2, selectInput(ns('field'), '指标', choices =setNames( c("oids", "mids", "amount",'quantity') , c('订单数','客户数','销售额','销售量') ) ))
#     ),
#     fluidRow(
#       column(4,dataTableOutput(ns('tbl'))) ,
#       column(8,eChartOutput(ns('map')))
#       #)
#     )
#     
#   )
# }
# salesMap <- function(input, output, session) {
#   
#   #付款日期值获取
#   pay_date <- reactive({
#     req(input$pay_date)
#     input$pay_date
#   })
#   
#   
#   field <- reactive({
#     req(input$field)
#     input$field
#   })
#   
#   #and date(dt) >= '",pay_date()[1],"'
#   #and date(dt) <= '",pay_date()[2],"'
#   
#   
#   df <- reactive({
#     sql <-
#       paste0("
#              select x.PROVINCE_NAME province_name ,
#              ifnull(y.oids ,0) oids,
#              ifnull(y.mids ,0) mids,
#              ifnull(y.amount ,0) amount,
#              ifnull(y.quantity ,0) quantity
#              from bbf_shiny.tb_param_PROVINCE_NAME x
#              left join (
#              SELECT
#              t.PROVINCE_NAME ,
#              count(distinct t.oid) oids ,
#              count(distinct t.mid) mids ,
#              round(sum(b.amount),0) amount ,
#              sum(b.quantity) quantity
#              FROM
#              (select oid , mid ,
#              case when PROVINCE_NAME like '%内蒙古%' then '内蒙古自治区'
#              when PROVINCE_NAME like '%宁夏%' then '宁夏回族自治区'
#              when PROVINCE_NAME like '%广西%' then '广西壮族自治区'
#              when PROVINCE_NAME like '%西藏%' then '西藏自治区'
#              when PROVINCE_NAME like '%新疆%' then '新疆维吾尔自治区'
#              when PROVINCE_NAME like '%青海%' then '青海省'
#              when PROVINCE_NAME like '%青海%' then '青海省'
#              else PROVINCE_NAME end PROVINCE_NAME ,PAY_STATUS,payDates ,HANDLE_STATUS,PAY_TYPE , POST_DATE_STR from
#              ecommerce.tb_porder )t
#              JOIN ecommerce.tb_porder_line b ON t.oid = b.oid
#              WHERE
#              1 = 1
#              AND  HANDLE_STATUS NOT IN (5, 7)
#              AND (
#              (
#              PAY_STATUS = 1
#              AND date(payDates) >= '",pay_date()[1],"'
#              AND date(payDates) <= '",pay_date()[2],"'
#              )
#              OR (
#              PAY_TYPE = 10002
#              AND date(POST_DATE_STR) >= '",pay_date()[1],"'
#              AND date(POST_DATE_STR) <= '",pay_date()[2],"'
#              )
#              )
#              group by t.PROVINCE_NAME )  y
#              on x.PROVINCE_NAME = y.PROVINCE_NAME
#              ")
#     dd <- dbGetQuery0('ecommerce',sql)
#     
#     #df <- dd %>% rename(`订单数`=oids,`客户数`=mids,`销售额`=amount,`销售量`=quantity)
#     df <- dd %>% rename(NAME=province_name)
#     
#     df
#   })
#   
#   
#   
#   output$tbl <- renderDataTable({
#     datatable(
#       #df() %>% rename(`省份`=NAME),
#       df <- df() %>% rename(`省份`=NAME,`订单数`=oids,`客户数`=mids,`销售额`=amount,`销售量`=quantity),
#       escape = FALSE,
#       rownames = FALSE,
#       selection = 'multiple',
#       #filter = 'top',
#       extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
#       options = list(
#         searching=FALSE,
#         lengthChange = TRUE
#       )
#     )
#   })
#   
#   
#   output$map <- renderEChart({
#     eMap(df(), namevar=~NAME, datavar = ~get(field()) )
#     
#   })
#   
# }


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 日销售曲线
salesDayTypeUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(title = '日支付类型趋势', status = 'primary', solidHeader = T, width = 12,
        fluidRow(class='toolbar',
                 column(4, dateRangeInput(ns('dt'), '成交日期',  start = Sys.Date()-30, end = Sys.Date()-1))
        ),
        fluidRow(
          column(7, highchartOutput(ns('trend1')) ),
          column(5, dataTableOutput(ns('tbl')) )
        )
    )
  )
}
salesDayType <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  df <- reactive({
    
    sql <- paste0("
                  SELECT
	                CASE
                  WHEN PAY_STATUS = 1 THEN
                  DATE_FORMAT(payDates,'%Y%m%d')
                  WHEN HANDLE_STATUS NOT IN (5, 7)
                  AND PAY_TYPE = 10002 THEN
                  DATE_FORMAT(POST_DATE_STR,'%Y%m%d')
                  END dt,
                  case 
                  WHEN PAY_STATUS = 1 THEN
                  1
                  WHEN PAY_TYPE = 10002 THEN
                  2
                  END type ,
                  count(t.oid) oids ,
                  count(distinct t.mid) mids ,
                  sum(sum_price) sum_price
                  FROM
                  tb_porder t
                  WHERE
                  1 = 1
                  AND  HANDLE_STATUS NOT IN (5, 7)
                  AND (
                  (
                  PAY_STATUS = 1
                  AND date(payDates) >= '",dt()[1],"'
                  AND date(payDates) <= '",dt()[2],"'
                  )
                  OR (
                  PAY_TYPE = 10002
                  AND date(POST_DATE_STR) >= '",dt()[1],"'
                  AND date(POST_DATE_STR) <= '",dt()[2],"'
                  )
                  )
                  group by 
                  CASE
                  WHEN PAY_STATUS = 1 THEN
                  DATE_FORMAT(payDates,'%Y%m%d')
                  WHEN PAY_TYPE = 10002 THEN
                  DATE_FORMAT(POST_DATE_STR,'%Y%m%d')
                  END ,
                  case 
                  WHEN PAY_STATUS = 1 THEN
                  1
                  WHEN HANDLE_STATUS NOT IN (5, 7)
                  AND PAY_TYPE = 10002 THEN
                  2
                  END 
                  ")
    x <- dbGetQuery0('ecommerce', sql)
    
      #rename(`小时`=h , `销售额`=amount2,`订单数量`=oid2)
    x
  })
  
   
  
  output$tbl <- DT::renderDataTable({
    datatable(
      df(), 
      rownames = FALSE,
      selection = 'multiple',
      extensions = list(Scroller=list()),
      options = list(
        searching=FALSE,
        lengthChange = TRUE
      )
    )
  })
   
  
  output$trend1 <- renderHighchart({
    hc <- highchart() %>%
      hc_xAxis(categories = subset(df(),type==1)$dt) %>%
      hc_add_series(name = "货到付款", data = subset(df(),type==2)$sum_price) %>%
      hc_add_series(name = "在线支付", data = subset(df(),type==1)$sum_price) %>%
      hc_legend(align = "right", verticalAlign = "top",layout = "vertical", x = 0, y = 100) %>%
      hc_title(text = "<b>付款方式趋势</b>",margin = 20, align = "center",style = list(color = "blue", useHTML = TRUE)) %>%
      hc_credits(enabled = TRUE, text = "www.lonk.tomy.site", href = "http://jkunst.com")%>%
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5", shared = TRUE, borderWidth = 5) %>%
      hc_exporting(enabled = TRUE) # enable exporting option

    hc %>% hc_add_theme(hc_theme_flat())
  })
   
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 红包分析
hongbaoUseUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(class='toolbar',
             column(6,helpText('1.红包已用量是包括购买使用、取消等合计',br()  )
             ),
             column(6,helpText( '')
             )
    ),
    fluidRow(class='toolbar',
             column(3, dateRangeInput(ns('dt'), '成单时间', start = Sys.Date()-1, end = Sys.Date()-1)),
             column(3, textInput(ns('txt'), '红包ID(可从excel黏贴或用空格隔开)', value = '5621 5622 5623 5625')),
             column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))

    ) ,
    fluidRow(
      DT::dataTableOutput(ns('tbl'))
    )
  )
}
hongbaoUse <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  txt <- reactive({
    req(input$txt)
    stringr::str_replace_all(input$txt, " ", "','")
  })

  df <- reactive({
    dbGetQuery0('ecommerce', paste0("
                                   select x1.couponid, x3.couponprefix, x3.amount,x3.consumeamount , x3.total, x1.sent_userids, x1.sent_userids1,
			 x2.hongbaoused, x2.amounts, x2.oids ,
                                    x3.type, x3.adminname, date(x3.CREATEDATEstr) as createday, date(x3.valid_begindate_s) as valid_begin_day,
                                    date(x3.valid_enddate_s) as valid_end_day ,x3.limitcount
                                    from (
                                    # 做成两个指标。 基于isuse。 =1是已用，全部为已发量
                                    SELECT couponid,count( userid) as sent_userids ,
                                    count(case when isuse=1 then   userid else null end) sent_userids1
                                    from ecommerce.tb_coupon_record 
                                    where 1=1
                                    and couponid in ('",txt(),"')
                                    group by couponid 
                                    
                                    ) x1
                                    left join (
                                    select couponid,count( DISTINCT couponcode) as hongbaoused,   
                                    sum(sum_price) as amounts, count(DISTINCT oid) as oids from (
                                    SELECT x1.couponid,x2.mid,x2.sum_price, x2.COUPONCODE,x2.oid,x2.couponAmount
                                    from tb_porder x2
                                    join tb_coupon_record x1
                                    on x1.COUPONCODE=x2.COUPONCODE and x1.couponid in ('",txt(),"')
                                    WHERE	1 = 1
                                    and 	HANDLE_STATUS NOT IN (5, 7)
                                    AND (
                                    (
                                    PAY_STATUS = 1
                                    AND date(payDates) >= '",dt()[1],"'
                                    AND date(payDates) <= '",dt()[2],"'
                                    )
                                    OR (
                                    PAY_TYPE = 10002
                                    AND date(POST_DATE_STR) >= '",dt()[1],"'
                                    AND date(POST_DATE_STR) <= '",dt()[2],"'
                                    )
                                    )
                                    ) x
                                    GROUP BY couponid
                                    
                                    ) x2
                                    on x1.couponid=x2.couponid
                                    LEFT JOIN (
                                    select couponid,amount,adminname,CREATEDATEstr,valid_begindate_s,valid_enddate_s,total,limitcount,couponprefix,consumeamount,type
                                    from tb_coupon_topic  
                                    # where couponid in  ('5637','5622', '5623', '5625')
                                    )x3
                                    on x1.couponid=x3.couponid
                                    where 1=1
                                    group by x1.couponid
                                    
                                    "))%>%
      # mutate(name=substring(name,1,10)) %>%
      select(
        `红包代码`=couponid,
        `红包前缀`=couponprefix,
        `红包面额` = amount,
        `额度要求` = consumeamount,
        `红包总量` = total,
        `已发量` = sent_userids,
        `已用量` = sent_userids1,
        `用红包购买客户数`=hongbaoused,
        `使用红包成单金额` = amounts,
        `使用红包订单数` = oids,
        `红包类型` = type,
        `红包创建人` = adminname,
        `红包创建时间` = createday,
        `红包生效时间` = valid_begin_day,
        `红包结束时间` = valid_end_day,
        `账号限制` = limitcount
      )
  })
  # export
  output$btn_export <- downloadHandler(paste('bbf-data-callcenter-',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      tmp <- df() #[input$tbl_rows_all, , drop = FALSE]
      write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
    }
  })

  output$tbl <- DT::renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'multiple',
      #extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      extensions = list(Scroller=list()),
      options = list(
        autoWidth = TRUE,
        columnDefs = list(list(width = '50px', targets = c(1))),
        searching=TRUE,
        deferRender=TRUE,
        scrollY=200,
        scrollX=TRUE,
        scrollCollapse = TRUE,
        #order = list(list(11, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 200,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-200)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-200;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    )
  })
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#------------------------电话查销售-------------------
#一般是根据首单购买的查询，追踪短信营销的效果
salesbaseOnmobileUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(6,helpText('1.本表通常用于新注册用户活动红包分析，追踪营销效果',br())
             ),
             column(6,helpText(''))
    ),
    br(),
    br(),
    br(),
    fluidRow(class='toolbar',
             column(3, dateRangeInput(ns('dt'), '成单时间', start = Sys.Date()-1, end = Sys.Date()-1)),
             column(6, textInput(ns('txt'), '收货或下单电话号码(可从excel黏贴或用空格隔开)', value = '13507267313 13690675007 13644663601 13843445244')),
             column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
             
    ) ,
    br(),
    br(),
    br(),
    fluidRow(
      column(6,DT::dataTableOutput(ns('tbl')))
    )
  )
}
salesbaseOnmobile <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  txt <- reactive({
    req(input$txt)
    stringr::str_replace_all(input$txt, " ", "','")
  })
  
  df <- reactive({
    dbGetQuery0('ecommerce', paste0("
                                    SELECT
                                    count(DISTINCT mid) as mids,count(DISTINCT oid ) as oids,sum(sum_price) as total_pay,
                                    round(avg( sum_price)) as oid_pay
                                    FROM
                                    tb_porder 
                                    WHERE	1 = 1
                                    and 	HANDLE_STATUS NOT IN (5, 7)
                                    AND (
                                    (
                                    PAY_STATUS = 1
                                    AND date(payDates) >= '",dt()[1],"'
                                    AND date(payDates) <= '",dt()[2],"'
                                    )
                                    OR (
                                    PAY_TYPE = 10002
                                    AND date(POST_DATE_STR) >= '",dt()[1],"'
                                    AND date(POST_DATE_STR) <= '",dt()[2],"'
                                    )
                                    )
                                    and (( mobile in ('",txt(),"')) or 
                                    linkman in ('",txt(),"'))
                                    "))%>%                        
      # mutate(name=substring(name,1,10)) %>%
      select(
        `购买人数`=mids,
        `订单数`=oids,
        `成单金额` = total_pay,
        `客单价` = oid_pay
      )
  })
  # export
  output$btn_export <- downloadHandler(paste('bbf-data-callcenter-',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      tmp <- df() #[input$tbl_rows_all, , drop = FALSE]
      write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
    }
  })
  
  output$tbl <- DT::renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'multiple',
      #extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      extensions = list(Scroller=list()),
      options = list(
        autoWidth = TRUE,
        columnDefs = list(list(width = '50px', targets = c(1))),
        searching=TRUE,
        deferRender=TRUE,
        scrollY=200,
        scrollX=TRUE,
        scrollCollapse = TRUE,
        #order = list(list(11, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 200,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-200)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-200;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    )
  })
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






salesDayTypeUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(title = '每日终端类型订单数', status = 'primary', solidHeader = T, width = 12,
        fluidRow(class='toolbar',
                 column(4, dateRangeInput(ns('dt'), '成交日期',  start = Sys.Date()-30, end = Sys.Date()-1))
        ),
        fluidRow(
          column(7, highchartOutput(ns('trend1')) ),
          column(5, dataTableOutput(ns('tbl')) )
        )
    )
  )
}
salesDayType <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  df <- reactive({
    
    sql <- paste0("
                  SELECT
	                CASE
                  WHEN PAY_STATUS = 1 THEN
                  DATE_FORMAT(payDates,'%Y%m%d')
                  WHEN HANDLE_STATUS NOT IN (5, 7)
                  AND PAY_TYPE = 10002 THEN
                  DATE_FORMAT(POST_DATE_STR,'%Y%m%d')
                  END dt,
                  case 
                  WHEN PAY_STATUS = 1 THEN
                  1
                  WHEN PAY_TYPE = 10002 THEN
                  2
                  END type ,
                  count(t.oid) oids ,
                  count(distinct t.mid) mids ,
                  sum(sum_price) sum_price
                  FROM
                  tb_porder t
                  WHERE
                  1 = 1
                  AND  HANDLE_STATUS NOT IN (5, 7)
                  AND (
                  (
                  PAY_STATUS = 1
                  AND date(payDates) >= '",dt()[1],"'
                  AND date(payDates) <= '",dt()[2],"'
                  )
                  OR (
                  PAY_TYPE = 10002
                  AND date(POST_DATE_STR) >= '",dt()[1],"'
                  AND date(POST_DATE_STR) <= '",dt()[2],"'
                  )
                  )
                  group by 
                  CASE
                  WHEN PAY_STATUS = 1 THEN
                  DATE_FORMAT(payDates,'%Y%m%d')
                  WHEN PAY_TYPE = 10002 THEN
                  DATE_FORMAT(POST_DATE_STR,'%Y%m%d')
                  END ,
                  case 
                  WHEN PAY_STATUS = 1 THEN
                  1
                  WHEN HANDLE_STATUS NOT IN (5, 7)
                  AND PAY_TYPE = 10002 THEN
                  2
                  END 
                  ")
    x <- dbGetQuery0('ecommerce', sql)
    
    #rename(`小时`=h , `销售额`=amount2,`订单数量`=oid2)
    x
  })
  
  
  
  output$tbl <- DT::renderDataTable({
    datatable(
      df(), 
      rownames = FALSE,
      selection = 'multiple',
      extensions = list(Scroller=list()),
      options = list(
        searching=FALSE,
        lengthChange = TRUE
      )
    )
  })
  
  
  output$trend1 <- renderHighchart({
    hc <- highchart() %>%
      hc_xAxis(categories = subset(df(),type==1)$dt) %>%
      hc_add_series(name = "货到付款", data = subset(df(),type==2)$sum_price) %>%
      hc_add_series(name = "在线支付", data = subset(df(),type==1)$sum_price) %>%
      hc_legend(align = "right", verticalAlign = "top",layout = "vertical", x = 0, y = 100) %>%
      hc_title(text = "<b>付款方式趋势</b>",margin = 20, align = "center",style = list(color = "blue", useHTML = TRUE)) %>%
      hc_credits(enabled = TRUE, text = "www.lonk.tomy.site", href = "http://jkunst.com")%>%
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5", shared = TRUE, borderWidth = 5) %>%
      hc_exporting(enabled = TRUE) # enable exporting option
    
    hc %>% hc_add_theme(hc_theme_flat())
  })
}