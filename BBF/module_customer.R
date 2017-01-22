#------------------------客单价区间-------------------
customerArpuUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-months(2)-1, end = Sys.Date()-1))
    ),
    fluidRow(
      dataTableOutput(ns('tbl'))
    )
  )
}
customerArpu <- function(input, output, session) {
  
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  
  
  
  # df <- reactive({
  #   sql <-
  #     paste0("
  #            select * from (
  #            select  case
  #            when sum_price >= 0 and sum_price <= 100 then
  #            'A 0-100'
  #            when sum_price > 100 and sum_price <= 200 then
  #            'B 100-200'
  #            when sum_price > 200 and sum_price <= 300 then
  #            'C 200-300'
  #            when sum_price > 300 and sum_price <= 500 then
  #            'D 300-500'
  #            when sum_price > 500 and sum_price <= 1000 then
  #            'E 500-1000'
  #            when sum_price > 1000 then
  #            'F 1000+'
  #            end  price_type,
  #            count(oid) oids
  #            from (select case
  #            when PAY_STATUS = 1 then
  #            payDates
  #            when HANDLE_STATUS not in (5, 7) and PAY_TYPE = 10002 then
  #            POST_DATE_STR
  #            end dt ， oid,
  #            sum_price
  #            from tb_porder t
  #            where 1 = 1
  #            and (PAY_STATUS = 1 or
  #            (HANDLE_STATUS not in (5, 7) and PAY_TYPE = 10002)))
  #            where 1 = 1
  #            and to_char(dt, 'yyyymmdd') between '",format(pay_date()[1],'%Y%m%d') ,"' and '", format(pay_date()[2],'%Y%m%d'),"'
  #            group by case
  #            when sum_price >= 0 and sum_price <= 100 then
  #            'A 0-100'
  #            when sum_price > 100 and sum_price <= 200 then
  #            'B 100-200'
  #            when sum_price > 200 and sum_price <= 300 then
  #            'C 200-300'
  #            when sum_price > 300 and sum_price <= 500 then
  #            'D 300-500'
  #            when sum_price > 500 and sum_price <= 1000 then
  #            'E 500-1000'
  #            when sum_price > 1000 then
  #            'F 1000+'
  #            end
  #            ) order by 1 ")
  # 
  #   df <- dbGetQuery1(sql)
  # 
  # })
  
  
  
  
  
  df <- reactive({
    sql <-
      paste0("
             select * from (
             select  case
             when sum_price >= 0 and sum_price <= 100 then
             'A 0-100'
             when sum_price > 100 and sum_price <= 200 then
             'B 100-200'
             when sum_price > 200 and sum_price <= 300 then
             'C 200-300'
             when sum_price > 300 and sum_price <= 500 then
             'D 300-500'
             when sum_price > 500 and sum_price <= 1000 then
             'E 500-1000'
             when sum_price > 1000 then
             'F 1000+'
             end  price_type,
             count(oid) oids
             from (select case
             when PAY_STATUS = 1 then
             payDates
             when PAY_TYPE = 10002 then
             POST_DATE_STR
             end dt , oid,
             sum_price
             from tb_porder t
             where 1 = 1
             and HANDLE_STATUS not in (5, 7) 
             and (PAY_STATUS = 1 or PAY_TYPE = 10002) ) y
             where 1 = 1
             and date(dt) >= '",pay_date()[1],"'
             and date(dt) <= '",pay_date()[2],"'
             group by case
             when sum_price >= 0 and sum_price <= 100 then
             'A 0-100'
             when sum_price > 100 and sum_price <= 200 then
             'B 100-200'
             when sum_price > 200 and sum_price <= 300 then
             'C 200-300'
             when sum_price > 300 and sum_price <= 500 then
             'D 300-500'
             when sum_price > 500 and sum_price <= 1000 then
             'E 500-1000'
             when sum_price > 1000 then
             'F 1000+'
             end
             ) x order by 1 ")
    dd <- dbGetQuery0('ecommerce',sql)
    
    df <- dd %>%mutate(
      sum_oids = sum(dd[,2]),
      #oid_rat = round( dd[,2]/sum(dd[,2]))
      oid_rat = dd[,2]/sum(dd[,2])
    ) %>%  select(-sum_oids) %>% rename(`价格区间`=price_type , `订单数`=oids,`订单占比`=oid_rat)
  }) 
  
  
  
  
  output$tbl <- renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list())
      # options = list(
      #   autoWidth = TRUE,
      #   columnDefs = list(list(width = '200px', targets = c(0))),
      #   searching=TRUE,
      #   deferRender=TRUE,
      #   scrollY=200,
      #   scrollX=TRUE,
      #   scrollCollapse = TRUE,
      #   order = list(list(0, 'asc')),
      #   language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
      #   pageLength = 200,
      #   lengthChange = FALSE,
      #   initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
      #   fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      # )
    )%>%  # datatable end
      formatPercentage(c('订单占比'), 2)%>%
      formatStyle(
        '订单数',
        background = styleColorBar(df()$订单数, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        '价格区间',
        transform = 'rotateX(0deg) rotateY(0deg) rotateZ(0deg)',
        backgroundColor = styleEqual(
          unique(df()$价格区间), c('lightblue', 'lightgreen', 'lightpink','blue','pink','green')
        )
      )  %>%
      formatStyle(
        '订单占比',
        color = styleInterval(c(3.4, 3.8), c('white', 'blue', 'red')),
        backgroundColor = styleInterval(3.4, c('green', 'yellow'))
      )
  })
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# rfmtable  分类汇总
rfmTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel('RFM分类汇总'),
    
    fluidRow(class='toolbar',
             column(6,helpText(paste0('倒推5个月,最近4个月是正常用户，连续5个月无购买为潜在流失，持续关注第五个月的用户，
                                      计算比较费时请耐心等待')
             ,br(),'1.R默认r个月,流失用户取第五个月，每个月关注倒数第五个月潜在流失用户',br(),
             '2.F是购买频次，取3次为临界值，用>2表示',
             br(),'3.M为累计购买金额，默认取189，为累计销售额的众数')
              ),
             column(6,helpText('4.近度计算公式为查询时间段的期末-时间进度或者浅爱流失时间',br(),
                               '5.注意查询时间段的开始时间点选取的时间段要大于流失时间段或时间进度',br(),
                               '6.汇总的空行是因为查询时有多出来，忽略不看'))
             ),
    fluidRow(class='toolbar',
             column(3, dateRangeInput(ns('dt'), '付款时间', start = Sys.Date()-150, end = Sys.Date()-1)),
             column(1, numericInput(ns('r_time1'), '时间近度>=', value = 120)),
             column(2, numericInput(ns('r_time2'), '时间近度<潜在流失时间<= ***', value = 150)),
             column(2, numericInput(ns('buy_freq'), '购买次数=', value =3 )),
             column(2, numericInput(ns('buy_cum'), '累计购买金额>=', value = 189)),
             column(2, downloadButton(ns('btn_export'), '导出汇总表', class = 'btn_nowrap'))
    ),
    br(),
    br(),
    fluidRow(
      column(6, DT::dataTableOutput(ns('tbl'))  ),
      column(6,RECharts3Output(ns('rfm_cluster')))
    )
  )
}


rfmTable <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  r_time1 <- reactive({
    req(input$r_time1)
    input$r_time1
  })
  r_time2 <- reactive({
    req(input$r_time2)
    input$r_time2
  })
  buy_freq <- reactive({
    req(input$buy_freq)
    input$buy_freq
  })
  buy_cum <- reactive({
    req(input$buy_cum)
    input$buy_cum
  })
  df <- reactive({
    dbGetQuery0('ecommerce',paste0("
     select mid_type , count(mid) mid_counts  from (
		 select mid ,
         case 
         when DATEDIFF('",dt()[2],"' , dt ) <= '",r_time1(),"' and oids >= '",buy_freq(),"' and amount >= '",buy_cum(),"' then 'A 重要保持R1F1M1'
         when DATEDIFF('",dt()[2],"' , dt ) <= '",r_time1(),"' and oids < '",buy_freq(),"' and amount < '",buy_cum(),"' then 'B 重要发展R1F0M0'
         when DATEDIFF('",dt()[2],"' , dt ) <= '",r_time1(),"' and oids < '",buy_freq(),"' and amount >= '",buy_cum(),"' then 'C 重要价值R1F0M1'
         when DATEDIFF('",dt()[2],"' , dt ) <= '",r_time1(),"' and oids >= '",buy_freq(),"' and amount < '",buy_cum(),"' then 'D 一般重要R1F1M0'
         when DATEDIFF('",dt()[2],"' , dt ) <= '",r_time2(),"' and DATEDIFF(SYSDATE() , dt ) > '",r_time1(),"'  and oids >= '",buy_freq(),"' and amount >='",buy_cum(),"' then 'E 重要挽留R0F1M1'
         when DATEDIFF('",dt()[2],"' , dt ) <= '",r_time2(),"' and DATEDIFF(SYSDATE() , dt ) > '",r_time1(),"' and oids < '",buy_freq(),"' and amount >= '",buy_cum(),"' then 'F 一般挽留客户R0F0M1'
         when DATEDIFF('",dt()[2],"' , dt ) <= '",r_time2(),"' and DATEDIFF(SYSDATE() , dt ) > '",r_time1(),"' and oids >= '",buy_freq(),"' and amount < '",buy_cum(),"' then 'G 一般挽留客户R0F1M0'
         when DATEDIFF('",dt()[2],"' , dt ) <= '",r_time2(),"' and DATEDIFF(SYSDATE() , dt ) > '",r_time1(),"' and oids < '",buy_freq(),"' and amount < '",buy_cum(),"' then 'H 低价值R0F0M0'
         end mid_type , 
         DATEDIFF(SYSDATE() , dt ) as r,  
         oids f , amount m
         from (
         select mid , date(max(dt)) as  dt, count(oid) oids , sum(sum_price) amount from (
         select case
         when PAY_STATUS = 1 then	 payDates
         when PAY_TYPE = 10002 then	 POST_DATE_STR
         end dt, 
         oid, sum_price , mid
         from tb_porder t
         where 1 = 1
         and HANDLE_STATUS not in (5, 7) 
         and sum_Price < 30000
         AND (
         (
         PAY_STATUS = 1
         AND date(payDates) >= '",dt()[1],"'
         AND date(payDates) <= '",dt()[2],"'
         )
         OR ( PAY_TYPE = 10002
         AND date(POST_DATE_STR) >= '",dt()[1],"'
         AND date(POST_DATE_STR) <= '",dt()[2],"'
         )
         )
         and mid not in (select mid from tb_b_type_member d where t.mid = d.mid )
         ) a
         group by mid
         ) x  
) y
         group by mid_type
         order by mid_type 

     "))%>% 
           select(`会员类别`=mid_type,`会员数量`=mid_counts)
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
        #searching=TRUE,
        deferRender=TRUE,
        scrollCollapse = TRUE,
        pageLength = 200
      )
    )
  })
  output$rfm_cluster <- renderREcharts3({ 
    pie(df(),`会员类别`,`会员数量`, title = 'RFM客户类别占比', height = 400)
  })
  }
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#------------------------当天注册购买转化-------------------
registerBuyUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-months(1)-1, end = Sys.Date()-1))
    ),
    fluidRow(
      dataTableOutput(ns('tbl'))
    )
  )
}
registerBuy <- function(input, output, session) {
  
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  
  
  df <- reactive({
    sql <-
      paste0("
             select  x.dt,x.mids_register,y.mids_buy  from 
             ( select date(post_date_str) as dt,count(mid) as mids_register
             from tb_member
             where  1=1
             and date(post_date_str) >= '",pay_date()[1],"'
             and date(post_date_str) <= '",pay_date()[2],"'
             group by date(post_date_str)) x
             join 
             (
             select dt, count(mid) as mids_buy from 
             (  select a.mid,  date(a.dt) as dt from 
             (             select case
             when PAY_STATUS = 1 then
             payDates
             when PAY_TYPE = 10002 then
             POST_DATE_STR
             end dt, 
             oid,
             mid,
             sum_price,
             PAY_STATUS
             from tb_porder
             where 1 = 1
             and HANDLE_STATUS not in (5, 7) 
             and (PAY_STATUS = 1 or   PAY_TYPE = 10002)
             ) a
             join 
             tb_member b 
             on a.mid=b.mid
             where   
             1=1
             and date(a.dt) >= '",pay_date()[1],"'
             and date(a.dt) <= '",pay_date()[2],"'
             and  date(b.post_date_str)=date(a.dt)
             
             ) c
             group by dt
             ) y
             on x.dt=y.dt ")
    
    
    
    dd <- dbGetQuery0('ecommerce',sql)
    
    df <- dd %>%mutate(
      #oid_rat = round( dd[,2]/sum(dd[,2]))
      buy_rate = dd[,3]/dd[,2]
    ) %>% rename(`日期`=dt , `注册人数`=mids_register,`购买人数`=mids_buy,`注册用户购买转化率`=buy_rate)
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
        order = list(list(1, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 21,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    )%>%  # datatable end
      formatPercentage(c('注册用户购买转化率'), 2)
    #)%>%  # datatable end
    # formatPercentage(c('订单占比'), 2)%>%
    # formatStyle(
    #   '订单数',
    #   background = styleColorBar(df()$订单数, 'steelblue'),
    #   backgroundSize = '100% 90%',
    #   backgroundRepeat = 'no-repeat',
    #   backgroundPosition = 'center'
    # ) %>%
    # formatStyle(
    #   '价格区间',
    #   transform = 'rotateX(45deg) rotateY(20deg) rotateZ(30deg)',
    #   backgroundColor = styleEqual(
    #     unique(df()$价格区间), c('lightblue', 'lightgreen', 'lightpink','blue','pink','green')
    #   )
    # )  %>%
    # formatStyle(
    #   '订单占比',
    #   color = styleInterval(c(3.4, 3.8), c('white', 'blue', 'red')),
    #   backgroundColor = styleInterval(3.4, c('gray', 'yellow'))
    # )
  })
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 客单价趋势
kdUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(class='toolbar',
             column(3, dateRangeInput(ns('dt'), '购买时间', Sys.Date()-90, Sys.Date()-1)),
             column(3, radioButtons(ns('freq'), '频率', choices = c('按月'='month','按周'='week', '按天'='day'), inline = TRUE))
    ),
    fluidRow(
      column(12, plotlyOutput(ns('p')))
    )
  )
}
kd <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  freq <- reactive({
    req(input$freq)
    input$freq
  })
  df <- reactive({
    if(freq()=='month') {
      prefix_sql <- 'substring(dt,1,7)'
    } else if(freq()=='week') {
      prefix_sql <- 'weekofyear(dt)'
    } else if(freq()=='day') {
      prefix_sql <- 'substring(dt,1,10)'
    } else {
      prefix_sql <- 'substring(dt,1,7)'
    }
    sql <- paste0("
            select " ,prefix_sql, " as dt1, sum(SUM_PRICE) /count(DISTINCT oid) AS m
            FROM
                  (
                  select case
                  when PAY_STATUS = 1 then  date(payDates)
                  when PAY_TYPE = 10002 then date(POST_DATE_STR)
                  end dt , oid,
                  sum_price
                  from tb_porder 
                  where 1 = 1
                  and HANDLE_STATUS not in (5, 7) 
                  and (PAY_STATUS = 1 or PAY_TYPE = 10002)
                  ) a
                  WHERE 1=1
                  #and date(dt) >= '20161001'
                  #and date(dt) <= '20161031'
                  AND	date(dt) >= '",dt()[1],"'
                  AND date(dt) <= '",dt()[2],"'
                  GROUP BY dt1
                  ")
    x <- dbGetQuery0('ecommerce', sql) %>% mutate(dt = as.factor(dt1))
    if(freq()=='day') {
      x$dt <- as.Date(x$dt)
    }
    x
  })
  
  output$p <- renderPlotly({
    g <- df() %>% 
      ggplot(aes(dt, m))+geom_bar(aes(fill=3), stat='identity')+labs(x='客单价趋势')+theme(legend.position='none')
    #ggplot()+geom_point(aes(dt, m, color=2))+geom_smooth()+labs(x='客单价趋势')+theme(legend.position='none')
    ggplotly(g)
  })
}


#------------------------购买次数汇总-------------------
customerBuyCntsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-months(2)-1, end = Sys.Date()-1))
    ),
    fluidRow(
      dataTableOutput(ns('tbl'))
    )
  )
}


customerBuyCnts <- function(input, output, session) {
  
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  
  
  
  df <- reactive({
    sql <-
      paste0(" 
             select case when oids = 1 then 'A 购买1次'
             when oids = 2 then 'B 购买2次'
             when oids = 3 then 'C 购买3次'
             when oids = 4 then 'D 购买4次'
             when oids = 5 then 'E 购买5次'
             when oids = 6 then 'F 购买6次'  
             when oids = 7 then 'G 购买7次'
             when oids = 8 then 'H 购买8次'
             when oids = 9 then 'I 购买9次'
             when oids = 10 then 'J 购买10次'
             when oids = 11 then 'K 购买11次'
             when oids = 12 then 'L 购买12次'
             else 'M 12次以上' end buy_cnts_type ,
             case when oids = 1 then '1'
             when oids = 2 then '2'
             when oids = 3 then '3'
             when oids = 4 then '4'
             when oids = 5 then '5'
             when oids = 6 then '6'  
             when oids = 7 then '7'
             when oids = 8 then '8'
             when oids = 9 then '9'
             when oids = 10 then '10'
             when oids = 11 then '11'
             when oids = 12 then '12'
             else '13' end buy_cnts ,
             count(mid) mids ,
             sum(amount) amount from (
             select mid , sum(sum_price) amount , count(distinct oid) oids  from (
             select a.mid , 
             case when a.PAY_STATUS = 1 then a.payDates when  a.PAY_TYPE = 10002 then a.POST_DATE_STR end dt,
             a.sum_price,
             a.oid
             FROM
             tb_porder a
             left join tb_member b  on a.mid = b.mid 
             WHERE
             1 = 1
             and a.HANDLE_STATUS not in (5, 7 ) 
             and ( a.PAY_STATUS = 1 or  a.PAY_TYPE = 10002   )
             and not EXISTS (select 'X' from tb_b_type_member d where a.mid = d.mid )  -- 排除掉  B类客户
             ) x
             where 1 = 1 
             and date(dt) >= '",pay_date()[1],"'
             and date(dt) <= '",pay_date()[2],"'
             group by mid  ) y
             group by 
             case when oids = 1 then 'A 购买1次'
             when oids = 2 then 'B 购买2次'
             when oids = 3 then 'C 购买3次'
             when oids = 4 then 'D 购买4次'
             when oids = 5 then 'E 购买5次'
             when oids = 6 then 'F 购买6次'  
             when oids = 7 then 'G 购买7次'
             when oids = 8 then 'H 购买8次'
             when oids = 9 then 'I 购买9次'
             when oids = 10 then 'J 购买10次'
             when oids = 11 then 'K 购买11次'
             when oids = 12 then 'L 购买12次'
             else 'M 12次以上' end ,case when oids = 1 then '1'
             when oids = 2 then '2'
             when oids = 3 then '3'
             when oids = 4 then '4'
             when oids = 5 then '5'
             when oids = 6 then '6'  
             when oids = 7 then '7'
             when oids = 8 then '8'
             when oids = 9 then '9'
             when oids = 10 then '10'
             when oids = 11 then '11'
             when oids = 12 then '12'
             else '13' end    
             ")
    
    dd <- dbGetQuery0('ecommerce',sql)
    #  df <- dbGetQuery0('ecommerce',sql)
    
    df <- dd %>%mutate(
      sum_mids = sum(dd[,3]),
      sum_amount = sum(dd[,4]),
      #mid_rat = dd[,3]/sum(dd[,3])
      #/客户数占比
      mid_rat = ifelse(sum_mids==0  , 0 , dd[,3]/sum_mids),
      #/销售额占比
      amount_rat = ifelse(sum_amount==0 , 0 , dd[,4]/sum_amount),
      #/客户单价
      mid_per_price = ifelse(dd[,3] == 0 , 0 ,  dd[,4]/dd[,3] ) ,
      
      ##客单价字符转换
      
      #/客单价
      # mid_price =ifelse(buy_cnts==13 ,'/',as.character( round(ifelse(as.integer(dd[,2])== 0 , 0 , mid_per_price/as.integer(dd[,2]) )),2) )
      mid_price =ifelse(buy_cnts==13 ,'/',as.character( round(ifelse(as.integer(dd[,2])== 0 , 0 , mid_per_price/as.integer(dd[,2]) )),2) )
      
    ) %>%  select(-sum_mids,-buy_cnts ,-sum_amount) %>% rename(`购买次数`=buy_cnts_type , `客户数`=mids,`销售额`=amount,
                                                               `客户数占比`=mid_rat , `销售额占比`=amount_rat,`客户单价`=mid_per_price,`客单价`=mid_price)
  })
  
  
  
  
  output$tbl <- renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list())
      # options = list(
      #   autoWidth = TRUE,
      #   columnDefs = list(list(width = '200px', targets = c(0))),
      #   searching=TRUE,
      #   deferRender=TRUE,
      #   scrollY=200,
      #   scrollX=TRUE,
      #   scrollCollapse = TRUE,
      #   order = list(list(0, 'asc')),
      #   language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
      #   pageLength = 200,
      #   lengthChange = FALSE,
      #   initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
      #   fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      # )
    )%>%  # datatable end
      formatPercentage(c('客户数占比','销售额占比'), 2) %>% formatRound(c('客户单价'), 2)
  })
  
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#------------------------省份城市分析-------------------
provinceCityUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             box(
               title = '省份城市分析', status = 'primary', solidHeader = T, width = 12,
               column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-30, end = Sys.Date()-1))
             )
    ),
    fluidRow(
      column(6,dataTableOutput(ns('tbl'))),
      column(6,dataTableOutput(ns('tbl2')))
    )
  )
}
provinceCity <- function(input, output, session) {
  
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  
  
  df <- reactive({
    # sql <-
    #   paste0("
    #          select  case when province like '%内蒙古%' then '内蒙古'
    #                when province like '%宁夏%' then '宁夏'
    #          when province like '%广西%' then '广西'
    #          when province like '%西藏%' then '西藏'
    #          when province like '%新疆%' then '新疆'
    #          when province like '%青海%' then '青海'
    #          else replace(replace(province,'省',''),'市','') end province  , count(DISTINCT oid) as count_orders,count(DISTINCT mid) as count_mids,sum(actual_pay) as sum_pay
    #          from (
    #          select a.oid,a.mid,
    #          replace(REPLACE(REPLACE(a.province_name,CHAR(10),''),CHAR(13),''),char(9),'') as province,
    #          replace(REPLACE(REPLACE(a.city_name,CHAR(10),''),CHAR(13),''),char(9),'') as city,
    #          a.actual_pay 
    #          from tb_porder a
    #          join (SELECT	CASE
    #          WHEN PAY_STATUS = 1 THEN	payDates
    #          WHEN PAY_TYPE = 10002 THEN POST_DATE_STR
    #          END dt,
    #          oid, mid, sum_price, PAY_STATUS
    #          FROM	tb_porder
    #          WHERE	1 = 1
    #          and HANDLE_STATUS NOT IN (5, 7)
    #          AND (	PAY_STATUS = 1	OR  PAY_TYPE = 10002)	
    #          ) b
    #          on a.oid=b.oid
    #          where 1=1
    #          and province_name is not NULL
    #          and city_name is not NULL
    #          #and   date(b.dt) between '20161024' and '20161024'
    #          and date(dt) >= '",pay_date()[1],"'
    #          and date(dt) <= '",pay_date()[2],"'
    #          ) c
    #          where province is not NULL
    #          group by  case when province like '%内蒙古%' then '内蒙古'
    #                when province like '%宁夏%' then '宁夏'
    #                when province like '%广西%' then '广西'
    #                when province like '%西藏%' then '西藏'
    #                when province like '%新疆%' then '新疆'
    #                when province like '%青海%' then '青海'
    #                else replace(replace(province,'省',''),'市','') end    ")
    
    
    
    sql <-paste0( " select  case when province like '%内蒙古%' then '内蒙古'
                   when province like '%宁夏%' then '宁夏'
                  when province like '%广西%' then '广西'
                  when province like '%西藏%' then '西藏'
                  when province like '%新疆%' then '新疆'
                  when province like '%青海%' then '青海'
                  else replace(replace(province,'省',''),'市','') end province  , 
                  count( oid) as count_orders,
                  count( mid) as count_mids,
                  sum(sum_price) as sum_pay
                  from (
                  select a.oid, 
                  replace(REPLACE(REPLACE(a.province_name,CHAR(10),''),CHAR(13),''),char(9),'') as province, 
                  mid, sum_price 
                  from tb_porder a 
                  where 1=1
                  and province_name is not NULL
                  and HANDLE_STATUS NOT IN (5, 7)
                  and ( 
                  ( pay_status = 1 
                  and date(payDates) >= '",pay_date()[1],"'
                  and date(payDates) <= '",pay_date()[2],"'  ) 
                  or ( pay_type = 10002 
                  and date(post_date_str) >= '",pay_date()[1],"'
                  and date(post_date_str)  <= '",pay_date()[2],"') 
                      ) 
                )c
                  where province is not NULL
                  group by case when province like '%内蒙古%' then '内蒙古'
                  when province like '%宁夏%' then '宁夏'
                  when province like '%广西%' then '广西'
                  when province like '%西藏%' then '西藏'
                  when province like '%新疆%' then '新疆'
                  when province like '%青海%' then '青海'
                  else replace(replace(province,'省',''),'市','') end  ")
    
    dd <- dbGetQuery0('ecommerce',sql)
    
    df <- dd %>%mutate(
      #oid_rat = round( dd[,2]/sum(dd[,2]))
      sum_pay = round(sum_pay),
      oid_pay = round(dd[,4]/dd[,2])
    ) %>% rename(`省份`=province , `购买单数`=count_orders,`购买人数`=count_mids,`销售额`=sum_pay,`客单价`=oid_pay)
  }) 
  
  
  
  
  output$tbl <- renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        #   autoWidth = TRUE,
        #   columnDefs = list(list(width = '200px', targets = c(0))),
        searching=TRUE,
        #   deferRender=TRUE,
        #   scrollY=200,
        #   scrollX=TRUE,
        #   scrollCollapse = TRUE,
        order = list(list(3, 'desc')),
        #   language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 20,
        lengthChange = TRUE
        #   initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
        #   fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
        # 
      )
    ) #%>%  # datatable end
    #formatPercentage(c('注册用户购买转化率'), 2)
  })
  
  
  
  df1 <- reactive({
#     sql <-
#       paste0("
#              select city, count(DISTINCT oid) as count_orders,count(DISTINCT mid) as count_mids,sum(actual_pay) as sum_pay
# from (
# 		select a.oid,a.mid,
# 				replace(REPLACE(REPLACE(a.province_name,CHAR(10),''),CHAR(13),''),char(9),'') as province,
# 				replace(REPLACE(REPLACE(a.city_name,CHAR(10),''),CHAR(13),''),char(9),'') as city,
# 				a.actual_pay 
# 		from tb_porder a
# 		join (SELECT	CASE
# 										WHEN PAY_STATUS = 1 THEN	payDates
# 										WHEN PAY_TYPE = 10002 THEN POST_DATE_STR
# 										END dt,
# 										 oid, mid, sum_price, PAY_STATUS
# 										FROM	tb_porder
# 										WHERE	1 = 1
#                     and  HANDLE_STATUS NOT IN (5, 7) 
# 										AND (PAY_STATUS = 1	OR PAY_TYPE = 10002)
# 				) b
# 		on a.oid=b.oid
# 		where 1=1
# 		and province_name is not NULL
# 		and city_name is not NULL
#     #and   date(b.dt) between '20161024' and '20161024'
# 	  and date(dt) >= '",pay_date()[1],"'
# 	  and date(dt) <= '",pay_date()[2],"'
# 		
# ) c
# where city is not NULL
# group by city
# order by sum_pay desc    ")
    
    
    sql <- paste0(" select  city_name city, 
						count( oid) as count_orders,
                  count( mid) as count_mids,
                  sum(sum_price) as sum_pay
                  from (
                  select a.oid, 
                  replace(REPLACE(REPLACE(a.city_name,CHAR(10),''),CHAR(13),''),char(9),'') as city_name, 
                  mid, sum_price 
                  from tb_porder a 
                  where 1=1
                  and province_name is not NULL
                  and HANDLE_STATUS NOT IN (5, 7)
                  and ( 
                  ( pay_status = 1 
                  and date(payDates) >= '",pay_date()[1],"'
                  and date(payDates) <= '",pay_date()[2],"'  ) 
                  or ( pay_type = 10002 
                  and date(post_date_str) >= '",pay_date()[1],"'
                  and date(post_date_str)  <= '",pay_date()[2],"')    
                  ) 
                  ) c
                  where city_name is not NULL
                  group by city_name
                  ")
    
    dd1 <- dbGetQuery0('ecommerce',sql)
    
    df1 <- dd1 %>%mutate(
      #oid_rat = round( dd[,2]/sum(dd[,2]))
      sum_pay = round(sum_pay),
      oid_pay = round(dd1[,4]/dd1[,2])
    ) %>% rename(`城市`=city , `购买单数`=count_orders,`购买人数`=count_mids,`销售额`=sum_pay,`客单价`=oid_pay)
  }) 
  
  
  output$tbl2 <- renderDataTable({
    datatable(
      df1(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        #   autoWidth = TRUE,
        #   columnDefs = list(list(width = '200px', targets = c(0))),
        searching=TRUE,
        #   deferRender=TRUE,
        #   scrollY=200,
        #   scrollX=TRUE,
        #   scrollCollapse = TRUE,
           order = list(list(3, 'desc')),
        #   language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 20,
        lengthChange = TRUE
        #   initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
        #   fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
        # 
      )
    ) 
  })
  
  
} 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#RFM维度分布
RFMDistributionUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(class='toolbar',
             box(
               title = 'RFM维度分布', status = 'primary', solidHeader = T, width = 12,
               column(4, dateRangeInput(ns('dt'), '支付日期', start = Sys.Date()-8, end = Sys.Date()-1))
             )
    ),
    fluidRow(
      box(title = 'R最近一次购买分布', status = 'primary', solidHeader = T, width = 12,
        column(6,plotlyOutput(ns('trend1'))),
        column(6,dataTableOutput(ns('r_distribution'))) 
        )
    ),
     fluidRow(
        box(title = 'F购买频次分布', status = 'primary', solidHeader = T, width = 12,
        column(6,plotlyOutput(ns('trend2'))),
        column(6,dataTableOutput(ns('f_distribution')))
        )
    ),
    fluidRow(
      box(title = 'M累计购买金额分布', status = 'primary', solidHeader = T, width = 12,
          column(6,plotlyOutput(ns('trend3'))),
          column(6,dataTableOutput(ns('M_distribution'))) 
      )
    )
    
    # fluidRow(
    #   box(column(12,dygraphOutput(ns('trend4'))))
    # )
  )
}


RFMDistribution <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  # 会员号 最后一次购买时间 订单数（购买次数）累计购买金额 客单价 ;r距离现在多少天放在r里面计算。datediff(Now(), dt) as r
  # tb_b_type_member是小b端客户, 排除小b客户与金额超过3万的订单。值要是从选择时间段以来所以购买的值【每个人要获取多次的】。
df <- reactive({
  sql<-paste0("
              select * from (   
              SELECT	mid,
              CASE
              WHEN PAY_STATUS = 1 THEN date(max(payDates))
              WHEN PAY_TYPE = 10002 THEN date(max(POST_DATE_STR))
              END dt_max,
              count(DISTINCT oid) AS oid_count,  
              sum(sum_price) as CUM_pay,
              sum(sum_price) / count(DISTINCT oid) AS oid_pay
              FROM tb_porder
              WHERE	1 = 1
              and HANDLE_STATUS NOT IN (5, 7)
              and SUM_PRICE<30000
              AND (	PAY_STATUS = 1	OR  PAY_TYPE = 10002)
              #AND date(post_date_str) > '20151101'
              #AND date(post_date_str) <= '20161031' 
              and mid not in (select mid from tb_b_type_member )
              group by mid
              ) x
              where 1=1
              #and date(dt_max) >= '2016-10-01' 
              #and date(dt_max) <= '2016-10-30'
              and date(dt_max)>='",dt()[1],"' 
              and date(dt_max)<='",dt()[2],"'
              ")
dbGetQuery0('ecommerce', sql) 
})
# 用于计算R，最近一次购买时间分布
df1 <- reactive({
  r <- df()
  r1<-r%>%
    mutate(rday=as.numeric(Sys.Date()-date(r$dt_max)))
  r_cut <-cut(r1$rday, c(0,30,60,90,120,180,270,360,Inf),dig.lab = 10)
  r1$r_cut = r_cut
  r2<-r1 %>% group_by(r_cut) %>% summarise(mids=n_distinct(mid), 
                                           CUM_pay=round(sum(CUM_pay)),
                                           oid_counts=round(sum(oid_count)),
                                           mean_r=round(mean(rday)))%>%
    mutate(meanmid_pay=round(CUM_pay/mids),
           meanoid_pay=round(CUM_pay/oid_counts))
  
  # 将列表转化成数据类型，并且过滤掉NA值
  r3<-as.data.frame(r2) 
  r3<-r3%>% rename(`日期`=r_cut , `人数`=mids,`累计金额数`=CUM_pay,`订单数`=oid_counts,
                                   `平均天数`=mean_r,`人均购买金额`=meanmid_pay,`客单价`=meanoid_pay)
    
  r4<-subset(r3,r3[,1]!='NA')
  r4
})


#这里的Sys.Date()要改成dt()[2]
output$trend1 <- renderPlotly({
		g <- df1() %>%
		  ggplot(aes(`日期`, `人数`))+geom_bar(aes(fill=3), stat='identity')+labs(x='R客户最近一次购买时间分布')+theme(legend.position='none')
		ggplotly(g)
  })

output$r_distribution <- DT::renderDataTable({
  datatable(
    df1(), 
    rownames = FALSE,
    selection = 'multiple',
    extensions = list(Scroller=list()),
    options = list(
      searching=FALSE,
      lengthChange = TRUE
    )
  )
})


# F购买频次
# df2 <- reactive({
#   a <- df()
#   a1<-subset(a,a$oid_count<=quantile(a$oid_count,  probs = c(95)/100))
#   a1
# })
 #频次统计
df2_1 <- reactive({
  a <- df()
  a1<-subset(a,a$oid_count<=quantile(a$oid_count,  probs = c(95)/100))
  a2<-as.data.frame(table(a1$oid_count))
  a2<-a2%>% rename(`购买次数`=Var1 , `频次`=Freq)
  a2
})
output$f_distribution <- DT::renderDataTable({
  datatable(
    df2_1(),
    rownames = FALSE,
    selection = 'multiple',
    extensions = list(Scroller=list()),
    options = list(
      searching=FALSE,
      lengthChange = TRUE
      )
  )
})
# # output$trend2 <- renderPlotly({
# #   a1<-df2()
# #   plot(a1$oid_count)
# # })
output$trend2 <- renderPlotly({
  a1<-df2_1()
  g <- a1 %>%
  ggplot(aes(`购买次数`, `频次`))+geom_bar(aes(fill=3), stat='identity')+labs(x='R客户最近一次购买时间分布')+theme(legend.position='none')
ggplotly(g)
})

# M累计购买金额分布
df3 <- reactive({
  tmp <-df()
  tmp1<-tmp
  lbl <-cut(tmp1$CUM_pay, c(0,200,1000,2000,5000,10000,50000,100000,Inf),dig.lab = 10)
  # cut(tmp$CUM_pay[1:20], c(0,50,100,200,300,400,500,600,800,1000,Inf),dig.lab = 10)
  tmp1$lbl = lbl
  tmp2<-tmp1 %>% group_by(lbl) %>% summarise(mids=n_distinct(mid), CUM_pay=sum(CUM_pay),oid_counts=sum(oid_count))%>%
    mutate(meanmid_pay=round(CUM_pay/mids),meanoid_pay=round(CUM_pay/oid_counts)) 
  
  tmp3<-as.data.frame(tmp2)
  
  # tmp3<-tmp3%>% rename(`金额区间`=lbl , `人数`=mids, `累计金额数`=round(CUM_pay),`订单数`=oid_counts,
  #                      `人均购买金额`=meanmid_pay,`客单价`=round(meanoid_pay))
  
  tmp3<-tmp3%>%
    select(`金额区间`=lbl ,`人数`=mids, `累计金额数`=round(CUM_pay),`订单数`=oid_counts,`人均购买金额`=meanmid_pay,`客单价`=meanoid_pay)
  tmp3<-subset(tmp3,tmp3[,1]!='NA')
  tmp3
})

output$trend3 <- renderPlotly({
  g <- df3() %>%
    ggplot(aes(`金额区间`, `人数`))+geom_bar(aes(fill=3), stat='identity')+labs(x='M客户累计金额区间分布')+theme(legend.position='none')
  ggplotly(g)
})
output$M_distribution <- DT::renderDataTable({
  datatable(
    df3(), 
    rownames = FALSE,
    selection = 'multiple',
    extensions = list(Scroller=list()),
    options = list(
      searching=FALSE,
      lengthChange = TRUE
    )
  )
})

}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#------------------------关联规则-------------------三级类目的关联规则
rulesUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = '关联规则', status = 'primary', solidHeader = T, width = 12,
    fluidRow(class='toolbar',
         
             column(3, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-31, end = Sys.Date()-1)),
             column(3),
             column(3, sliderInput(ns('support'), "Support:", min=0.0001, max=0.2, value=0.001)),
             column(3, sliderInput(ns('confidence'), "Confidence:", min=0.02, max=0.8, value=0.1))
          )
    ),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    fluidRow(
      column(6,dataTableOutput(ns('tbl'))),
      column(6,plotlyOutput(ns('rules1')))
    ),
    fluidRow(
      plotOutput(ns('rules2'))
    )
  )
}
rules <- function(input, output, session) {
  
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  support <- reactive({
    req(input$support)
    input$support
  })
  confidence <- reactive({
    req(input$confidence)
    input$confidence
  })
  df <- reactive({
    sql <-
      paste0("
         select distinct a.mid,c.ctitle2
         from tb_porder a
         join tb_porder_line b on a.oid=b.oid
         join tb_product_catalogbase c on b.pid=c.pid
         where 
         1 = 1
         AND (
           (
             a.PAY_STATUS = 1
             AND date(a.payDates) >='",pay_date()[1],"' 
             AND date(a.payDates) <= '",pay_date()[2],"'
           )
           OR (
             a.HANDLE_STATUS NOT IN (5, 7)
             AND a.PAY_TYPE = 10002
             AND date(a.POST_DATE_STR) >='",pay_date()[1],"' 
             AND date(a.POST_DATE_STR) <='",pay_date()[2],"'	
           )
         )
      ")
    dbGetQuery0('ecommerce',sql)
  }) 
  # 数据处理
  df1 <- reactive({
    data1<-df()
    trans_data1<- as(split(data1[,"ctitle2"], data1[,"mid"]), "transactions")
    # a100=0.001
    #rules<-apriori(trans_data1, parameter = list(minlen=1,supp=0.001, conf=0.1))
    rules<-apriori(trans_data1, parameter=list(support=support(),confidence=confidence()))
    #inspectDT(rules)
    as.data.frame(inspect(rules))%>%
      mutate(
        `支持度`=round(support,6),
        `置信度`=round(confidence,6),
        `提升`=round(lift,6) )%>%
      select(
        -support,-confidence,-lift
             )
      
    #inspect(rules)
  })
  df2 <- reactive({
    data1<-df()
    trans_data1<- as(split(data1[,"ctitle2"], data1[,"mid"]), "transactions")
    # a100=0.001
    #rules<-apriori(trans_data1, parameter = list(minlen=1,supp=0.001, conf=0.1))
    rules<-apriori(trans_data1, parameter=list(support=support(),confidence=confidence()))
    rules
  })
  output$tbl <- renderDataTable({
    datatable(
      df1(),
      rownames = FALSE,
      selection = 'multiple',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        autoWidth = TRUE,
        scrollX=TRUE,
        searching=FALSE,
        lengthChange = FALSE
      ) 
      
    )
    # 数据要做转换
    # %>%
    #  formatPercentage(c(support,confidence), 2)
  })
  
 
  
  #renderPlotly  renderPlot
  output$rules1 <- renderPlotly({
    #plotly_arules(df2())
    plotly_arules(df2(), measure=c("support", "confidence"), shading="lift")
  })
  
  output$rules2 <- renderPlot({
     subrules<- head(sort(df2(), by="support"),100)
     plot(subrules, method="graph")
    # data(Groceries)
    # rules <- apriori(Groceries, parameter=list(support=0.005, confidence=0.5))
    # subrules2 <- sample(rules, 10)
    # plot(subrules2, method="graph")
  })
  
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 单品关联规则
itemRuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel('单品关联规则'),
    fluidRow(
      class='toolbar',
      column(3, dateRangeInput(ns('dt'), '成单日期', start = Sys.Date()-120, end=Sys.Date()-1)),
      column(2, textInput(ns('txt'), '三级类目pid(Excel直接复制粘贴或空格隔开)', value = '高血压')),
      column(1, downloadButton(ns('btn_export'), '导出规则', class = 'btn_nowrap')),
      column(3, sliderInput(ns('support'), "Support:", min=0.0001, max=0.2, value=0.001)),
      column(3, sliderInput(ns('confidence'), "Confidence:", min=0.01, max=0.8, value=0.1))
      #rules<-apriori(trans_data1, parameter = list(minlen=1,supp=0.001, conf=0.1))
    ),
    br(),
    br(),
    
    #规则汇总，summary说明，散点图
    fluidRow(
      column(12, DT::dataTableOutput(ns('tbl')))
    )
  )
}
itemRule <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  txt <- reactive({
    req(input$txt)
    stringr::str_replace_all(input$txt, " ", "','")
  })
  support <- reactive({
    req(input$support)
    input$support
  })
  confidence <- reactive({
    req(input$confidence)
    input$confidence
  })
  
  df <- reactive({
    dbGetQuery0("ecommerce", paste0("
                                    
                                    select a.oid,a.mid,b.proname,
                                    b.pid,c.ctitle0,c.ctitle1,c.ctitle2
                                    from (
                                    SELECT	
                                    CASE WHEN PAY_STATUS = 1 THEN	payDates
                                    WHEN  PAY_TYPE = 10002 THEN	POST_DATE_STR
                                    END dt,
                                    oid, mid
                                    FROM	tb_porder
                                    WHERE	1 = 1
                                    and SUM_PRICE<30000
                                    and 	HANDLE_STATUS NOT IN (5, 7)
                                    AND (
                                    (
                                    PAY_STATUS = 1
                                    AND date(payDates) >= '",dt()[1],"'
                                    AND date(payDates) <='",dt()[2],"'
                                    )
                                    OR (
                                    PAY_TYPE = 10002
                                    AND date(POST_DATE_STR) >= '",dt()[1],"'
                                    AND date(POST_DATE_STR) <= '",dt()[2],"'
                                    )
                                       )
                                    ) a
                                    join tb_porder_line b on a.oid=b.oid
                                    join tb_product_catalogbase c on b.pid=c.pid
                                    where 1=1
                                    and ctitle2 in ('",txt(),"')
                                    "))
  })
  

  # 数据处理 要去掉重复值
  df1 <- reactive({
    data0<-df()%>%select(mid,proname)
    #去重
    index<-duplicated(data0)
    data1<-data0[!index,]
    
    trans_data1<- as(split(data1[,"proname"], data1[,"mid"]), "transactions")
    # a100=0.001
    #rules<-apriori(trans_data1, parameter = list(minlen=1,supp=0.001, conf=0.1))
    rules<-apriori(trans_data1, parameter=list(support=support(),confidence=confidence()))
    #inspectDT(rules)
    as.data.frame(inspect(rules))%>%
      mutate(
        `支持度`=round(support,6),
        `置信度`=round(confidence,6),
        `提升`=round(lift,6) )%>%
      select(
        -support,-confidence,-lift
      )
    #inspect(rules)
  })
  # export
  output$btn_export <- downloadHandler(paste('bbf-itemrules-',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      tmp <- df1()[input$tbl_rows_all, , drop = FALSE]
      write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
    }
  })
  # df2 <- reactive({
  #   data1<-df()
  #   trans_data1<- as(split(data1[,"proname"], data1[,"mid"]), "transactions")
  #   # a100=0.001
  #   #rules<-apriori(trans_data1, parameter = list(minlen=1,supp=0.001, conf=0.1))
  #   rules<-apriori(trans_data1, parameter=list(support=support(),confidence=confidence()))
  #   rules
  # })
  output$tbl <- renderDataTable({
    datatable(
      df1(),
      rownames = FALSE,
      selection = 'multiple',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        autoWidth = TRUE,
        scrollX=TRUE,
        searching=FALSE,
        lengthChange = FALSE
      ) 
    )
    # 数据要做转换
    # %>%
    #  formatPercentage(c(support,confidence), 2)
  })
  
  }
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#------------------------会员等级分布-------------------
customerLevelUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = '会员等级分布', status = 'primary', solidHeader = T, width = 12,
        fluidRow(class='toolbar',
                 column(9,helpText('E 普通会员0~2000',
                                   'D 铜牌会员2001~10000',
                                   'C 银牌会员10001~50000', 
                                   'B 金牌会员50001~100000',
                                   'A 金牌会员>100000',
                                   '此表未将次数考虑到等级划分当中')
                 )
        ),
        br(),
        # 开始日期如何-一年
        # 频次阀值4个
        fluidRow(class='toolbar',
                 column(3, dateRangeInput(ns('pay_date'), '付款时间',  start = floor_date(Sys.Date()-months(12)-1, "month"), end = Sys.Date()-1)),
                 column(2, numericInput(ns("freq1"), '铜：购买次数>=',value = 2 , step = 1)),
                 column(2, numericInput(ns("freq2"), '银：购买次数>=',value = 4 , step = 1)),
                 column(2, numericInput(ns("freq3"), '金：购买次数>=',value = 6 , step = 1)),
                 column(2, numericInput(ns("freq4"), '钻：购买次数>=',value = 8 , step = 1))
                 
        ),
        # 金额阀值4个 step什么意思，能否不写  , step = 10000
        fluidRow(class='toolbar',
                 column(3, ''),
                 column(2, numericInput(ns("cumamounts1"), '铜：累计购买金额>=',value = 750 )),
                 column(2, numericInput(ns("cumamounts2"), '银：累计购买金额>=',value = 2000 )),
                 column(2, numericInput(ns("cumamounts3"), '金：累计购买金额>=',value = 5000 )),
                 column(2, numericInput(ns("cumamounts4"), '钻：累计购买金额>=',value = 10000 ))
                 
        ),
        br(),
        fluidRow(
          
          # column(6,plotlyOutput(ns('level')))
          column(6,dataTableOutput(ns('tbl'))) 
          #)
        )
    )
  )
}
customerLevel <- function(input, output, session) {
  
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  
  freq1 <- reactive({
    req(input$freq1)
    input$freq1
  }) 
  freq2 <- reactive({
    req(input$freq2)
    input$freq2
  }) 
  freq3 <- reactive({
    req(input$freq3)
    input$freq3
  }) 
  freq4 <- reactive({
    req(input$freq4)
    input$freq4
  })    
  
  cumamounts1 <- reactive({
    req(input$cumamounts1)
    input$cumamounts1
  })  
  cumamounts2 <- reactive({
    req(input$cumamounts2)
    input$cumamounts2
  })  
  cumamounts3 <- reactive({
    req(input$cumamounts3)
    input$cumamounts3
  })  
  cumamounts4 <- reactive({
    req(input$cumamounts4)
    input$cumamounts4
  })  
  #and date(dt) >= '",pay_date()[1],"'
  #and date(dt) <= '",pay_date()[2],"'
  
  
  df <- reactive({
    sql <-
      paste0("
             select 
             ifnull(account_level,'未知') account_level ,
             count(distinct x.mid) mids ,
             sum(x.amount) amount,
             sum(x.oids) oids
             from (
             select t.mid ,
             count(DISTINCT t.oid) as oids,
             sum(t.SUM_PRICE) as amount,
             CASE 
             WHEN (count(DISTINCT t.oid)>='",freq4(),"' ) and (sum(t.SUM_PRICE)>='",cumamounts4(),"' ) THEN 'A 钻石会员'
             WHEN (count(DISTINCT t.oid)>='",freq3(),"' ) and (sum(t.SUM_PRICE)>='",cumamounts3(),"' ) THEN 'B 金牌会员'
             WHEN (count(DISTINCT t.oid)>='",freq2(),"' ) and (sum(t.SUM_PRICE)>='",cumamounts2(),"' ) THEN 'C 银牌会员'
             WHEN (count(DISTINCT t.oid)>='",freq1(),"' ) and (sum(t.SUM_PRICE)>='",cumamounts1(),"' ) THEN 'D 铜牌会员' 
             else  'E 普通会员'
             END AS account_level
             from tb_porder t
             where 1=1
             and 	HANDLE_STATUS NOT IN (5, 7)
             AND (
             (
             PAY_STATUS = 1
             AND date(payDates) >= '",pay_date()[1],"'
             AND date(payDates) <=  '",pay_date()[2],"'
             )
             OR (
             PAY_TYPE = 10002
             AND date(POST_DATE_STR) >= '",pay_date()[1],"'
             AND date(POST_DATE_STR) <= '",pay_date()[2],"'
             )
             )
             and t.sum_price>0
             and not exists(select 'X' from bbf_shiny.tb_b_type_member c where t.mid = c.mid )
             group by t.mid
             #order by count(DISTINCT t.oid) desc
             ) x
             GROUP BY account_level
             ")
    dd <- dbGetQuery0('ecommerce',sql)
    
    df <- dd %>%mutate(
      mid_rat = dd[,2]/sum(dd[,2]),
      amount_rat = dd[,3]/sum(dd[,3]),
      per_price = round(dd[,3]/dd[,4],2)
    ) %>% rename(`会员等级`=account_level , `购买人数`=mids,`销售额`=amount, 
                 `订单数`=oids,`客单价`=per_price,`人数占比`=mid_rat,`销售额占比`=amount_rat) 
  }) 
  
  
  
  
  
  output$tbl <- renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'multiple',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        searching=FALSE,
        lengthChange = TRUE
      )
    )%>%  # datatable end
      formatPercentage(c('人数占比','销售额占比'), 2)
  })
  
  
}







#------------------------会员性别分布-------------------
customerSexUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = '会员性别分布', status = 'primary', solidHeader = T, width = 12,
        fluidRow(class='toolbar',
                 column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-1, end = Sys.Date()-1))
        ),
        fluidRow(
          column(6,dataTableOutput(ns('tbl'))) ,
          column(6,RECharts3Output(ns('sex')))
          #)
        )
    )
  )
}
customerSex <- function(input, output, session) {
  
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
             select  case when y.sex = 0 then '男' 
             when y.sex = 1 then '女'
             else '未知' end sex ,
             count(distinct x.mid) mids ,
             sum(x.amount) amount,
             sum(quantity) quantity,
             count(distinct x.oid) oids
             from (
             SELECT
             CASE
             WHEN PAY_STATUS = 1 THEN
             payDates
             WHEN PAY_TYPE = 10002 THEN
             POST_DATE_STR
             END dt,
             t.oid,
             t.mid,
             b.amount,
             b.quantity
             FROM
             tb_porder t
             JOIN tb_porder_line b ON t.oid = b.oid
             WHERE
             1 = 1
            and  HANDLE_STATUS NOT IN (5, 7)
             AND (
             (
             PAY_STATUS = 1
             AND date(payDates) >= '",pay_date()[1],"'
             AND date(payDates) <= '",pay_date()[2],"'
             )
             OR (
             date(POST_DATE_STR) >= '",pay_date()[1],"'
             AND date(POST_DATE_STR) <= '",pay_date()[2],"'
             AND PAY_TYPE = 10002
             )
             )
             ) x 
             left join (select sex , mid  from tb_member a
             where 1 = 1) y
             on x.mid = y.mid 
             group by
             case when y.sex = 0 then '男' 
             when y.sex = 1 then '女'
             else '未知' end 
             ")
    dd <- dbGetQuery0('ecommerce',sql)
    
    df <- dd %>%mutate(
      mid_rat = dd[,2]/sum(dd[,2]),
      amount_rat = dd[,3]/sum(dd[,3]),
      per_price = round(dd[,3]/dd[,5],2)
    ) %>% dplyr::rename(`性别`=sex , `购买人数`=mids,`销售额`=amount, 
                 `销售量`= quantity, `客单价`=per_price,`人数占比`=mid_rat,`销售额占比`=amount_rat) %>% select(-oids)
  }) 
  
  
  
  
  
  output$tbl <- renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'multiple',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        searching=FALSE,
        lengthChange = TRUE
      )
    )%>%  # datatable end
      formatPercentage(c('人数占比','销售额占比'), 2)
  })
  
  
  # output$sex <- renderPlotly({
  #   # g <- df3() %>%
  #   #   ggplot(aes(lbl, CUM_pay))+geom_bar(aes(fill=3), stat='identity')+labs(x='M客户累计金额区间分布')+theme(legend.position='none')
  #   # ggplotly(g)
  # 
  #   p <- plot_ly(
  #     df(), 
  #     values = df()$'人数占比', labels = df()$'性别',
  #     type = 'pie'
  #     #marker =  df()$'性别'
  #   )
  # })
  
  
  
  output$sex <- renderREcharts3({ 
    # dat1 = aggregate(weight ~ feed, data = chickwts, mean)
    # bar(dat1, feed, weight, label = round(weight, 0), title = 'test')
    # x<-df()
    # x[,1] <-as.factor(x[,1])
    # pie(x,`性别`,`购买人数`)
    pie(df(),'性别', '人数占比',title = '人数占比', height = 400)
    
    # bar(df, as.factor(df$'性别'), df$'人数占比')
    # REcharts3::pie(df() , df()$'性别' , df()$'人数占比', title = '人数占比', height = 400)
    # REcharts3::pie(df , df$'性别' , df$'人数占比', title = '人数占比', height = 400)
    #p02
    
  })
}


#------------------------会员年龄分布-------------------
customerAgeUI <- function(id) {
  ns <- NS(id)
  tagList(
    # box(title = '会员年龄分布', status = 'primary', solidHeader = T, width = 12,
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-1, end = Sys.Date()-1))
    ),
    fluidRow(
      column(6,dataTableOutput(ns('tbl'))) ,
      column(6,RECharts3Output(ns('age')))
    )
  )
}
customerAge <- function(input, output, session){
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
             select  case 
             when age <= 10 then '小于10岁'
             when age > 10 and age <= 19 then '10-19'
             when age >= 20 and age <= 29 then '20-29'
             when age >= 30 and age <= 39 then '30-39'
             when age >= 40 and age <= 49 then '40-49'
             when age >= 50 and age <= 59 then '50-59'
             when age >= 60 then '60岁以上'
             else '未知' end age ,
             count(distinct x.mid) mids ,
             sum(x.amount) amount ,
             sum(quantity) quantity,
             count(distinct x.oid) oids
             from (
             SELECT
             CASE
             WHEN PAY_STATUS = 1 THEN
             payDates
             WHEN PAY_TYPE = 10002 THEN
             POST_DATE_STR
             END dt,
             t.oid,
             t.mid,
             b.amount,
             b.quantity
             FROM
             tb_porder t
             JOIN tb_porder_line b ON t.oid = b.oid
             WHERE
             1 = 1
             AND HANDLE_STATUS NOT IN (5, 7)
             AND (
             (
             PAY_STATUS = 1
             AND date(payDates) >= '",pay_date()[1],"'
             AND date(payDates) <= '",pay_date()[2],"'
             )
             OR (
             date(POST_DATE_STR) >= '",pay_date()[1],"'
             AND date(POST_DATE_STR) <= '",pay_date()[2],"'
             AND PAY_TYPE = 10002
             )
             )
             ) x 
             left join (
             select ifnull(round(DATEDIFF(now() , date(birthday) )/365),'未知') age ,mid 
             from tb_member 
             where birthday <> ''
             ) y
             on x.mid = y.mid 
             group by case 
             when age <= 10 then '小于10岁'
             when age > 10 and age <= 19 then '10-19'
             when age >= 20 and age <= 29 then '20-29'
             when age >= 30 and age <= 39 then '30-39'
             when age >= 40 and age <= 49 then '40-49'
             when age >= 50 and age <= 59 then '50-59'
             when age >= 60 then '60岁以上'
             else '未知' end
             ")
    dd <- dbGetQuery0('ecommerce',sql)
    
    df <- dd %>%mutate(
      mid_rat = dd[,2]/sum(dd[,2]),
      amount_rat = dd[,3]/sum(dd[,3]),
      per_price = round(dd[,3]/dd[,5],2)
    ) %>% dplyr::rename(`年龄`=age , `购买人数`=mids,`销售额`=amount, 
                 `销售量`= quantity, `客单价`=per_price,`人数占比`=mid_rat,`销售额占比`=amount_rat) %>% select(-oids)
    
    
    
  }) 
  
  
  output$tbl <- renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'multiple',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        searching=FALSE,
        lengthChange = TRUE
      )
    )%>%  # datatable end
      formatPercentage(c('人数占比','销售额占比'), 2)
  })
  
  
  output$age <- renderREcharts3({ 
    # dat1 = aggregate(weight ~ feed, data = chickwts, mean)
    # pie(dat1, feed, weight, label = round(weight*10, 0), title = 'Pie Plot')
    # donut(dat1, feed, weight, title = 'Pie Plot')
    pie(df(),`年龄`,`销售额`, title = '销售额', height = 400)
    #p02
    
  })
  
}

#------------------------用户总表-------------------
customerTotalsituationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(title = '用户总表', status = 'primary', solidHeader = T, width = 12,
        fluidRow(class='toolbar',
                 column(6,helpText('1.总注册用户数：是截止到选择日期的所有注册用户数',br(),
                                   '2.累计购买客户数：是截止到选择日期所有购买客户数',br(),
                                   '3.近活跃用户数：最近4个月有访问的用户数（这里是指近4个月有在网站登录的，没有登录识别不到，此处小于实际值）')
                 ),
                 column(6,helpText( '3.复购率：最近三个月购买2次及以上的客户数除以最近3个月有购买的人数',br(),
                                    '4.已流失用户数：近一年内没有登录的用户（这里是指近一年有在网站登录的，没有登录识别不到，此处大于实际值）',br(),
                                    '5.潜在流失客户数:最近4个月有访问且无购买的注册用户',br(),
                                    '6.活跃用户数:最近4个月有登录访问的用户数')
                 )
        ),
        fluidRow(class='toolbar',
                 column(3, dateInput(ns('dt'), '日期', Sys.Date()-1))
        ),
        fluidRow(
          column(12,dataTableOutput(ns('tb1')) )
          #column(6, plotlyOutput(ns('trend1')) )
        )
    )
  )
}
customerTotalsituation <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  df <- reactive({
    
    sql <- paste0("
select x1.total_registers,x2.total_buy_customer,x3.oneyear_buy_custmers,x4.active_register,x41.active_register30,X5.lost_customer,x6.potential_lost,
x7.mids_threemonth,x8.mids_threemonth1
from (
                  (
                  select count(DISTINCT mid) as total_registers from tb_member
                  where date(post_date_str)<='",dt(),"'
                  ) x1,
                  (
                  select count(DISTINCT mid) as total_buy_customer from tb_porder
                  where 1=1
                  AND HANDLE_STATUS NOT IN (5, 7)
                  AND (
                  (
                  PAY_STATUS = 1
                  AND date(payDates) <= '",dt(),"'
                  )
                  OR (
                   PAY_TYPE = 10002
                  AND date(POST_DATE_STR) <= '",dt(),"'
                  )
                  )
                  ) x2,
                  (
                  select count(DISTINCT mid) as oneyear_buy_custmers from tb_porder
                  where 1=1
                  AND HANDLE_STATUS NOT IN (5, 7)
                  AND (
                  (
                  PAY_STATUS = 1
                  AND date(payDates) >= date_sub('",dt(),"', interval '365' day)
                  AND date(payDates) <='",dt(),"'
                  )
                  OR (
                    PAY_TYPE = 10002
                  AND date(POST_DATE_STR) >=date_sub('",dt(),"', interval '365' day)
                  AND date(POST_DATE_STR) <= '",dt(),"'
                  )
                  )
                  ) x3,
                  (
                  select  count(DISTINCT mid) as active_register from tb_member
                  where 1=1
                  and date(last_login_date_str)>= date_sub('",dt(),"', interval '120' day)
                  and date(last_login_date_str)<= '",dt(),"'
                  ) x4,
                  (select  count(DISTINCT mid) as active_register30 from tb_member
                  where 1=1
                  and date(last_login_date_str)>= date_sub('",dt(),"', interval '30' day)
                  and date(last_login_date_str)<= '",dt(),"'
                  ) x41,
                  (
                  select  count(DISTINCT mid) as lost_customer from tb_member
                  where 1=1
                  and date(last_login_date_str)<= date_sub('",dt(),"', interval '365' day)
                  ) x5,
                  (
                  select COUNT(DISTINCT mid) as potential_lost from tb_member a
                  where 1=1
                  and date(last_login_date_str)>= date_sub('",dt(),"', interval '120' day)
                  and date(last_login_date_str)<='",dt(),"'
                  and not EXISTS ( SELECT 1 from 
                  (
                  select DISTINCT mid  from tb_porder b
                  where 1=1
                  and  HANDLE_STATUS NOT IN (5, 7)
                  AND (
                  (
                  PAY_STATUS = 1
                  AND date(payDates) >=date_sub('",dt(),"', interval '120' day)
                  AND date(payDates) <='",dt(),"'
                  )
                  OR (
                  PAY_TYPE = 10002
                  AND date(POST_DATE_STR) >=date_sub('",dt(),"', interval '120' day)
                  AND date(POST_DATE_STR) <= '",dt(),"'
                  )
                  )
                  ) m
                  where a.mid=m.mid)
                  ) x6,
                  (
                  	select count(DISTINCT mid) as mids_threemonth from  (
                  	 select  mid,count(DISTINCT oid) as oids from tb_porder
                  	 WHERE	1 = 1
                      and  HANDLE_STATUS NOT IN (5, 7)
                  	 AND (
                  	 (
                  	 PAY_STATUS = 1
                  	 AND date(payDates) >= date_sub('",dt(),"', interval '90' day)
                  	 AND date(payDates) <='",dt(),"'	
                  	 )
                  	 OR (
                  	  PAY_TYPE = 10002
                  	 AND date(POST_DATE_STR) >=date_sub('",dt(),"', interval '90' day)	
                  	 AND date(POST_DATE_STR) <='",dt(),"'		
                  	 )
                  	 )
                  	 group by mid
                      ) x
                  	 where 1=1 
                  	 and oids>=2
                    ) x7,
                  (

                  	 select  count(DISTINCT mid) as mids_threemonth1 from tb_porder
                  	 WHERE	1 = 1
                     and HANDLE_STATUS NOT IN (5, 7)
                  	 AND (
                    	 (
                    	 PAY_STATUS = 1
                    	 AND date(payDates) >=date_sub('",dt(),"', interval '90' day)
                    	 AND date(payDates) <= '",dt(),"'		
                    	 )
                    	 OR (
                    	 
                    	   PAY_TYPE = 10002
                    	 AND date(POST_DATE_STR) >=date_sub('",dt(),"', interval '90' day)	
                    	 AND date(POST_DATE_STR) <='",dt(),"'		
                    	 )
                  	 )
                    ) x8
    )
                  ")
    x <- dbGetQuery0('ecommerce', sql)
    x1<-x%>%
      mutate(
       active_rate=round(active_register/total_registers,4),
       fugoulv=round(mids_threemonth/mids_threemonth1,4)
      )%>%
      select(`总注册用户数`=total_registers , 
             `累计购买客户数`=total_buy_customer,
             `近一年购买客户数`=oneyear_buy_custmers,
             `近三个月购买客户数`=mids_threemonth1,
             `近三个月购买两次及以上客户数`=mids_threemonth,
             `复购率`=fugoulv,
             `活跃用户数`=active_register ,
             `活跃度`= active_rate,
             `30天活跃注册用户数`=active_register30,
             `已流失用户数`=lost_customer,
             `潜在流失客户数`=potential_lost)
    x1
  })
  
  
  # output$trend1 <- renderPlotly({
  #   x1 <- df()
  #   plot_ly(x1, x = ~`小时`) %>%
  #     add_lines(y = ~`销售额`)%>% add_markers(y = ~`销售额`)
  #   # plot_ly(x1, x = ~x1[,1]) %>%
  #   #      add_lines(y = ~x1[,2]) %>% add_markers(y = ~x1[,2])
  # })
  output$tb1 <- DT::renderDataTable({
    datatable(
      df(), 
      rownames = FALSE,
      selection = 'multiple',
      extensions = list(Scroller=list()),
      options = list(
        searching=FALSE,
        lengthChange = TRUE,
        pageLength = 24,
        scrollY=200,
        scrollX=TRUE,
        scrollCollapse = TRUE
        
      )
    )
  })
  
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 流失监控
lostcustomerMonitorUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(title = '流失监控', status = 'primary', solidHeader = T, width = 12,
        fluidRow(class='toolbar',
          column(4, dateRangeInput(ns('dt1'), '注册时间',  start =floor_date(Sys.Date()-months(4)-1, "month"), 
                                   end = ceiling_date(Sys.Date()-months(4), "month") -1) ),
          column(4, dateRangeInput(ns('dt2'), '成单时间',  start =floor_date(Sys.Date()-months(4)-1, "month"),
                                   end = Sys.Date()-1)),
          column(2, downloadButton(ns('btn_export'), '导出天清单', class = 'btn_nowrap'))
        ),
        fluidRow(
          column(6,dataTableOutput(ns('tb1')) ),
          column(6, plotlyOutput(ns('trend1')) )
        ),
        br(),br(),
        fluidRow(
          column(6,dataTableOutput(ns('tb12')) ),
          column(6, plotlyOutput(ns('trend2')) )
        )
    )
  )
}
lostcustomerMonitor <- function(input, output, session) {
  dt1 <- reactive({
    req(input$dt1)
    input$dt1
  })
  dt2 <- reactive({
    req(input$dt2)
    input$dt2
  })
  df <- reactive({
    sql <- paste0("
                  select DATE_FORMAT(dt,'%Y-%m') as dt_month,count(DISTINCT mid) as mids,sum(sum_price) as sales,avg(sum_price) as persales,count(DISTINCT oid) as oids
                  FROM  (
                  select case
                  when PAY_STATUS = 1 then	payDates
                  when PAY_TYPE = 10002 then	POST_DATE_STR
                  end dt , 
                  oid, mid,SUM_PRICE
                  from tb_porder 
                  where 1 = 1
                  and HANDLE_STATUS not in (5, 7) 
                  and (PAY_STATUS = 1 or PAY_TYPE = 10002 )
                  ) x
                  where 1 = 1 
                  and date(dt) >='",dt2()[1],"'
                  and date(dt) <='",dt2()[2],"'
                  and mid in (
                  SELECT DISTINCT mid from tb_member 
                  WHERE 1=1
                  and date(post_date_str)>='",dt1()[1],"'
                  and date(post_date_str)<='",dt1()[2],"'
                  )
                  GROUP BY DATE_FORMAT(dt,'%Y-%m')
                  ")
    x <- dbGetQuery0('ecommerce', sql)
    x1<-x%>%
      rename(`月份`=dt_month , `销售额`=sales ,`购买客户数`=mids,`订单数量`=oids,`客单价`=persales)
    x1
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
  output$trend1 <- renderPlotly({
    g <- df() %>% 
      ggplot(aes(`月份`, `购买客户数`))+geom_bar(aes(fill=3), stat='identity')+labs(x='购买月份')+theme(legend.position='none')
    #ggplot()+geom_point(aes(dt, m, color=2))+geom_smooth()+labs(x='客单价趋势')+theme(legend.position='none')
    ggplotly(g)
  })
  
  df1 <- reactive({
    sql <- paste0("
select DATE_FORMAT(dt,'%Y-%m-%d') as dt_day,count(DISTINCT mid) as mids,sum(sum_price) as sales,avg(sum_price) as persales,
                  count(DISTINCT oid) as oids
                  FROM  (
                  select case
                  when PAY_STATUS = 1 then	payDates
                  when PAY_TYPE = 10002 then	POST_DATE_STR
                  end dt , 
                  oid, mid,SUM_PRICE
                  from tb_porder 
                  where 1 = 1
                  and HANDLE_STATUS not in (5, 7) 
                  and (PAY_STATUS = 1 or PAY_TYPE = 10002 )
                  ) x
                  where 1 = 1 
                  and date(dt) >='",dt2()[1],"'
                  and date(dt) <='",dt2()[2],"'
                  and mid in (
                  SELECT DISTINCT mid from tb_member 
                  WHERE 1=1
                  and date(post_date_str)>='",dt1()[1],"'
                  and date(post_date_str)<='",dt1()[2],"'
                  )
                  GROUP BY DATE_FORMAT(dt,'%Y-%m-%d')
                  ")
    x <- dbGetQuery0('ecommerce', sql)
    x1<-x%>%
      rename(`日期`=dt_day , `销售额`=sales ,`购买客户数`=mids,`订单数量`=oids,`客单价`=persales)
    x1
  })
  
  # export
  output$btn_export <- downloadHandler(paste('bbf-day_lost-',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      #tmp <- df1()[input$tbl_rows_all, , drop = FALSE]
      write.csv( df1(), file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
    }
    
  })
  
  output$tb12 <- DT::renderDataTable({
    datatable(
      df1(), 
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
  output$trend2 <- renderPlotly({
    g <- df1() %>% 
      ggplot(aes(`日期`, `购买客户数`))+geom_bar(aes(fill=3), stat='identity')+labs(x='购买日期')+theme(legend.position='none')
    #ggplot()+geom_point(aes(dt, m, color=2))+geom_smooth()+labs(x='客单价趋势')+theme(legend.position='none')
    ggplotly(g)
  })
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#------------------------会员购买间隔分布-------------------
customerBuyDiffUI <- function(id) {
  ns <- NS(id)
  tagList(
    # box(title = '会员年龄分布', status = 'primary', solidHeader = T, width = 12,
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-120, end = Sys.Date()-1))
    ),
    fluidRow(
      column(5,dataTableOutput(ns('tbl'))) ,
      column(7,RECharts3Output(ns('buy')))
    )
  )
}
customerBuyDiff <- function(input, output, session){
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  
  
  df <- reactive({
    #print(pay_date()[1])
    #print(format(pay_date()[1],'%Y%m%d') ) 
    
    sql <-
      paste0("
             select case 
             when difftime >= 0 and difftime <= 7 then 'A:0-7' 
             when difftime > 7 and difftime <= 14 then 'B:7-14'             
             when difftime > 14 and difftime <= 30 then 'C:14-30'             
             when difftime > 30 and difftime <= 60 then 'D:30-60'             
             when difftime > 60 and difftime <= 90 then 'E:60-90'             
             when difftime > 90 and difftime <= 120 then 'F:90-120'             
             when difftime > 120 and difftime <= 180 then 'G:120-180'             
             when difftime > 180 and difftime <= 270 then 'H:180-270'             
             when difftime > 270 and difftime <= 360 then 'I:270-360'             
             when difftime > 360  then 'J:360+' 
             end diff , count(distinct mid) mids , sum(sum_price) AMOUNT, count(distinct oid) oids from (
             with temp as (
             select mid, dt, sum_price , oid ,  row_number() over(partition by mid order by dt desc) rn 
             from (select mid,sum_price ,oid,
             case
             when PAY_STATUS = 1 then
             payDates
             when PAY_TYPE = 10002 then
             POST_DATE_STR
             end dt
             from tb_porder a
             WHERE 1 = 1
             and HANDLE_STATUS not in (5, 7)
             AND ((PAY_STATUS = 1 AND
             to_char(payDates, 'yyyymmdd') >= '", format(pay_date()[1],'%Y%m%d') ,"' AND
             to_char(payDates, 'yyyymmdd') <= '", format(pay_date()[2],'%Y%m%d'),"') OR
             ( PAY_TYPE = 10002 AND
             to_char(POST_DATE_STR, 'yyyymmdd') >= '",format(pay_date()[1],'%Y%m%d'),"' AND
             to_char(POST_DATE_STR, 'yyyymmdd') <= '",format(pay_date()[2],'%Y%m%d'),"')))
             ) select a.mid  , round(a.dt-b.dt) difftime , a.sum_price ,a.oid from temp a
             join temp b  on a.mid = b.mid and  a.rn = b.rn - 1 
             )
             group by case 
             when difftime >= 0 and difftime <= 7 then 'A:0-7' 
             when difftime > 7 and difftime <= 14 then 'B:7-14'             
             when difftime > 14 and difftime <= 30 then 'C:14-30'             
             when difftime > 30 and difftime <= 60 then 'D:30-60'             
             when difftime > 60 and difftime <= 90 then 'E:60-90'             
             when difftime > 90 and difftime <= 120 then 'F:90-120'             
             when difftime > 120 and difftime <= 180 then 'G:120-180'             
             when difftime > 180 and difftime <= 270 then 'H:180-270'             
             when difftime > 270 and difftime <= 360 then 'I:270-360'             
             when difftime > 360  then 'J:360+' 
             end
             ")
    
    
    
    dd <- dbGetQuery1(sql)
    
    df <- dd %>%mutate(
      mid_rat = dd[,2]/sum(dd[,2]),
      amount_rat = round(dd[,3]/sum(dd[,3]),4),
      per_price = round(dd[,3]/dd[,4],2)
    ) %>% dplyr::rename(`购买间隔`=DIFF  , `购买人数`=MIDS ,`销售额`=AMOUNT,  `客单价`=per_price,`人数占比`=mid_rat,`销售额占比`=amount_rat) %>% select(-OIDS)
    
    
    
  }) 
  
  
  output$tbl <- renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'multiple',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        searching=FALSE,
        lengthChange = TRUE
      )
    )%>%  # datatable end
      formatPercentage(c('人数占比','销售额占比'), 2)
  })
  
  
  output$buy <- renderREcharts3({
    
    dat <- df()[c('购买间隔','销售额','销售额占比')]
    
    dat$销售额 <- dat$销售额/10000
    
    dat2 = gather(dat, key, value, -购买间隔)
    
    p = his(dat2, '购买间隔', value, key, #label = percent(value, 0), 
            title = '单位:万', label.show = F, label.position = 'top',yAxis.max = 1000)
    p2 = p %>% addSecAxis(series = '销售额占比', type = 'line' ,yAxis.max = 1)
    p2
  })
  
}







#------------------------会员生命周期-------------------
customerLifeCycleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(2, sliderInput(ns("logday"),label = "未登陆天数", min = 1, max = 1000, value = 365  ))
    ),
    fluidRow(
      column(5,dataTableOutput(ns('tbl'))) ,
      column(7,RECharts3Output(ns('life')))
    )
  )
}
customerLifeCycle <- function(input, output, session){
  #  未登陆天数  获取
  logday <- reactive({
    req(input$logday)
    input$logday
  }) 
  
  
  df <- reactive({
    sql <-
      paste0("
             select  case  
             when difftime >= 0 and difftime <= 7 then 'A:0-7' 
             when difftime > 7 and difftime <= 14 then 'B:7-14'             
             when difftime > 14 and difftime <= 30 then 'C:14-30'             
             when difftime > 30 and difftime <= 60 then 'D:30-60'             
             when difftime > 60 and difftime <= 90 then 'E:60-90'             
             when difftime > 90 and difftime <= 120 then 'F:90-120'             
             when difftime > 120 and difftime <= 180 then 'G:120-180'             
             when difftime > 180 and difftime <= 270 then 'H:180-270'             
             when difftime > 270 and difftime <= 360 then 'I:270-360'             
             when difftime > 360  then 'J:360+' 
             end diff , count(mid) mids   
             from (
             select mid , round(last-first) difftime  from (
             select mid,
             min(dt) keep (dense_rank first order by dt desc) last , 
             min(dt) keep (dense_rank last order by dt desc) first
             from (
             with temp as (select mid
             from ana_member a
             where 1 = 1
             and sysdate - a.LAST_LOGIN_DATE_STR > ",logday(),")
             select a.mid,
             case
             when PAY_STATUS = 1 then
             payDates
             when PAY_TYPE = 10002 then
             POST_DATE_STR
             end dt
             from tb_porder a
             join temp b
             on a.mid = b.mid
             WHERE 1 = 1
            and HANDLE_STATUS not in (5, 7)
             AND (PAY_STATUS = 1 OR PAY_TYPE = 10002))
             group by mid 
             ) x
             where first < last )
             group by case  
             when difftime >= 0 and difftime <= 7 then 'A:0-7' 
             when difftime > 7 and difftime <= 14 then 'B:7-14'             
             when difftime > 14 and difftime <= 30 then 'C:14-30'             
             when difftime > 30 and difftime <= 60 then 'D:30-60'             
             when difftime > 60 and difftime <= 90 then 'E:60-90'             
             when difftime > 90 and difftime <= 120 then 'F:90-120'             
             when difftime > 120 and difftime <= 180 then 'G:120-180'             
             when difftime > 180 and difftime <= 270 then 'H:180-270'             
             when difftime > 270 and difftime <= 360 then 'I:270-360'             
             when difftime > 360  then 'J:360+' 
             end 
             ")
    
    
    
    dd <- dbGetQuery1(sql)
    
    df <- dd %>%mutate(
      mid_rat = round(dd[,2]/sum(dd[,2]),4)
    )%>% arrange(DIFF)  %>% dplyr::rename(`生命周期`=DIFF  , `购买人数`=MIDS , `人数占比`=mid_rat)
    
  }) 
  
  
  output$tbl <- renderDataTable({
    datatable(  
      #df() <- df()[order(df()$生命周期),],
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'multiple',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        searching=FALSE,
        lengthChange = TRUE
      )
    )%>%  # datatable end
      formatPercentage(c('人数占比'), 2)
  })
  
  
  output$life <- renderREcharts3({
    
    #dat1 = aggregate(weight ~ feed, data = chickwts, mean)
    donut(df(), 生命周期, 购买人数, title = '会员生命周期分布')
    
  })
  
}








#------------------------会员购买留存-------------------
customerBuyKeepUI <- function(id) {
  ns <- NS(id)
  tagList(
    # box(title = '会员年龄分布', status = 'primary', solidHeader = T, width = 12,
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-120, end = Sys.Date()-1))
    ),
    fluidRow(
      column(5,dataTableOutput(ns('tbl'))) 
      #column(7,RECharts3Output(ns('buy')))
    )
  )
}
customerBuyKeep <- function(input, output, session){
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  
  
  df <- reactive({
    #print(pay_date()[1])
    #print(format(pay_date()[1],'%Y%m%d') ) 
    
    sql <-
      paste0("
               
          select case 
             when difftime >= 0 and difftime <= 7 then 'A:0-7' 
             when difftime > 7 and difftime <= 14 then 'B:7-14'             
             when difftime > 14 and difftime <= 30 then 'C:14-30'             
             when difftime > 30 and difftime <= 60 then 'D:30-60'             
             when difftime > 60 and difftime <= 90 then 'E:60-90'             
             when difftime > 90 and difftime <= 120 then 'F:90-120'             
             when difftime > 120 and difftime <= 180 then 'G:120-180'             
             when difftime > 180 and difftime <= 270 then 'H:180-270'             
             when difftime > 270 and difftime <= 360 then 'I:270-360'             
             when difftime > 360  then 'J:360+' 
             end diff , count(distinct  mid) mids , sum(sum_price) AMOUNT, count(distinct oid) oids from (
             with temp as (
             select mid, dt, sum_price , oid ,  row_number() over(partition by mid order by dt desc) rn 
             from (select mid,sum_price ,oid,
             case
             when PAY_STATUS = 1 then
             payDates
             when PAY_TYPE = 10002 then
             POST_DATE_STR
             end dt
             from tb_porder a
             WHERE 1 = 1
             and HANDLE_STATUS not in (5, 7)
             AND ((PAY_STATUS = 1 AND
             to_char(payDates, 'yyyymmdd') >=  '", format(pay_date()[1],'%Y%m%d') ,"' AND
             to_char(payDates, 'yyyymmdd') <= '",format(pay_date()[2],'%Y%m%d'),"') OR
             ( PAY_TYPE = 10002 AND
             to_char(POST_DATE_STR, 'yyyymmdd') >= '", format(pay_date()[1],'%Y%m%d') ,"' AND
             to_char(POST_DATE_STR, 'yyyymmdd') <= '",format(pay_date()[2],'%Y%m%d'),"')))
             ) select a.mid  , round(a.dt-b.dt) difftime , a.sum_price ,a.oid 
             from temp a
             join temp b  on a.mid = b.mid and  b.rn > 1 
             where a.rn = 1 
             )
             group by case 
             when difftime >= 0 and difftime <= 7 then 'A:0-7' 
             when difftime > 7 and difftime <= 14 then 'B:7-14'             
             when difftime > 14 and difftime <= 30 then 'C:14-30'             
             when difftime > 30 and difftime <= 60 then 'D:30-60'             
             when difftime > 60 and difftime <= 90 then 'E:60-90'             
             when difftime > 90 and difftime <= 120 then 'F:90-120'             
             when difftime > 120 and difftime <= 180 then 'G:120-180'             
             when difftime > 180 and difftime <= 270 then 'H:180-270'             
             when difftime > 270 and difftime <= 360 then 'I:270-360'             
             when difftime > 360  then 'J:360+' 
             end
             ")
    
    
    
    dd <- dbGetQuery1(sql)
    
    df <- dd %>%mutate(
      mid_rat = dd[,2]/sum(dd[,2]),
      amount_rat = round(dd[,3]/sum(dd[,3]),4),
      per_price = round(dd[,3]/dd[,4],2)
    ) %>% dplyr::rename(`相对第一次购买间隔时间分布`=DIFF  , `购买人数`=MIDS ,`销售额`=AMOUNT,  `客单价`=per_price,`人数占比`=mid_rat,`销售额占比`=amount_rat) %>% select(-OIDS)
    
    
    
  }) 
  
  
  output$tbl <- renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'multiple',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        searching=FALSE,
        lengthChange = TRUE
      )
    )%>%  # datatable end
      formatPercentage(c('人数占比','销售额占比'), 2)
  })
  
  # 
  # output$buy <- renderREcharts3({
  #   
  #   dat <- df()[c('购买间隔','销售额','销售额占比')]
  #   
  #   dat$销售额 <- dat$销售额/10000
  #   
  #   dat2 = gather(dat, key, value, -购买间隔)
  #   
  #   p = his(dat2, '购买间隔', value, key, #label = percent(value, 0), 
  #           title = '单位:万', label.show = F, label.position = 'top',yAxis.max = 1000)
  #   p2 = p %>% addSecAxis(series = '销售额占比', type = 'line' ,yAxis.max = 1)
  #   p2
  # })
  # 
}






#------------------------新注册红包提醒-------------------
#默认的新用户注册红包是不变的。
newregisterHongbaoUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             box(
               title = '新注册红包提醒', status = 'primary', solidHeader = T, width = 12,
               column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
               #column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-31, end = Sys.Date()-1)) # 筛选剩余多少个小时的红包
             ),
             br(),
             br(),
             br(),
             br(),
             fluidRow(
               column(10,dataTableOutput(ns('tbl')))
             )
    )
  )
}
newregisterHongbao <- function(input, output, session) {
  
  #付款日期值获取
  #pay_date <- reactive({
  #  req(input$pay_date)
  #   input$pay_date
  #  }) 
  
  df <- reactive({
    sql <-
      paste0("
             select * from (
             select userid ,
             b.mobile ,
             b.post_date_str ,  
             timestampdiff(hour,sysdate(),valid_enddate_s) remain_h ,
             couponid ,
             a.amount ,
             a.couponcode
             from ecommerce.tb_coupon_record  a
             join tb_member b on a.userid = b.mid 
             where a.couponid in (5621,5622,5623,5625) 
             and ISUSE = 0  
             ) x
             where x.remain_h >= 0
            #and x.remain_h <= 24
             ")
    dbGetQuery0('ecommerce',sql)%>%
      select(
        `会员编号` = userid, 
        `电话` = mobile,
        `会员注册时间` = post_date_str,
        `新注册红包剩余时间` = remain_h,
        `红包代码` = couponid,
        `红包面额` = amount,
        `红包明细` = couponcode
      )
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
        order = list(list(3, 'desc')),
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