# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 月度KPI仪表盘
monthKpiUI <- function(id) {
  ns <- NS(id)
  month_list <- stringr::str_sub(as.character(seq(floor_date(Sys.Date()-365,unit = "year"), by='month', length.out = 24)), 1, 7)
    #stringr::str_sub(seq(as.Date("2016-01-01"), as.Date("2017-01-01"), by="1 month"), 1, 7)
  #substr(as.character(seq(floor_date(Sys.Date(),unit = "year"), by='month', length.out = 12)), 1, 7)
  #kpi <- dbGetQuery0('bbf_shiny', paste0("select * from bbf_kpi where dt='", month_list[month(Sys.Date())], "'"))
  
  tagList(
    box(
      title = '月度KPI仪表盘', status = 'primary', solidHeader = T, width = 12,
      fluidRow(class='toolbar',
               column(6,helpText('1.本表业绩数据均以发货口技计算，包括销售、订单转化、毛利等',br(),
                                 '2.日均咨询量:包括小能有效咨询量与有效呼入接通数',
                                 '3.从11月开始有数据')
               )
      ),
      fluidRow(class='toolbar',
               column(2,selectInput(ns("dt"), "月份", choices = month_list, selected = substr(Sys.Date(),1,7)  ))
               #column(4, dateRangeInput(ns('dt'), '支付日期', start = Sys.Date()-7 , end = Sys.Date()-1))
      )),
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
    br(),
    br(),
    br(),
    br(),
    fluidRow(
      column(12,DT::dataTableOutput(ns('monthkpi')))
    ),
    br(),
    br(),
    br(),
    br(),
    fluidRow(
      column(6,DT::dataTableOutput(ns('monthkpi1')))
    )
    
    
  )
}
monthKpi <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  #-- -- 目标值: 日平均UV  销售额  日均有效咨询量  成单转化率 
  #-- -- 访问：日平均UV    日均有效咨询量（+severals电话接通量）  日均注册会员数  日均新访客数 日均新用户注册转化率
  #-- -- 销售：累计销售额（成交） 订单数 客单价 客户数  客户购买金额 毛利额 毛利率
  #-- -- R计算：UV达成率，销售达成率，有效咨询量达成率，成单转化达成率。
  df <- reactive({
    sql<-paste0("
                select IFNULL(x1.target_uv,0) as target_uv,IFNULL(x1.target_sales,0) as target_sales,IFNULL(x1.target_consult,0) as target_consult,
                IFNULL(x1.target_payrat,0) as target_payrat,
                x2.UV_perday,x2.UV_sum,x2.consult_perday,x2.registers_perday,x2.newvisits_perday,x2.newregisters_rate,
                x3.sales_total,x3.count_oids,x3.sales_avg,x3.mids,x3.mid_cumprice,x3.gross_margin,x3.grossmargin_rate
                from (
                (
                select dt,uv as target_uv,sales as target_sales,consult as target_consult,payrat as target_payrat
                from bbf_shiny.month_bbf_kpi
                where 1=1
                and dt = '",dt(),"'
                ) x1,
                (
                select round(avg(uv)) as UV_perday,round(sum(uv)) as UV_sum,round(avg(consult)+avg(severals)) as consult_perday,
                round(avg(zhuces)) as registers_perday, round(avg(new_visits)) as newvisits_perday, round(avg(zhuces)/avg(new_visits),4) as newregisters_rate
                from bbf_shiny.bbf_yy_data_daily
                where 1=1
                and DATE_FORMAT(dt,'%Y-%m') = '",dt(),"'
                ) x2,
                (
SELECT
round(sum(sum_price)) AS sales_total,
                count(DISTINCT OID) AS count_oids,
                avg(sum_price) AS sales_avg,
                count(DISTINCT mid) as mids,
                round(sum(sum_price)/count(DISTINCT mid)) as mid_cumprice,
                sum(sum_price*0.07-couponamount-score_to_cost/100)  as gross_margin,
                sum(sum_price*0.07-couponamount-score_to_cost/100)/sum(sum_price) as grossmargin_rate
                FROM  tb_porder
                WHERE
                1 = 1
                and DATE_FORMAT(send_time_str,'%Y-%m')='",dt(),"'
                and HANDLE_STATUS not in (5 , 7 )
                ) x3
                )
                ")
    dbGetQuery0('ecommerce', sql)
    
  })
  df1 <- reactive({
    df()%>%
      mutate(
        uv_complete = round(UV_perday/target_uv,4),
        sales_complete = round(sales_total/target_sales,4),
        consult_complete = round(consult_perday/target_consult,4),
        payrat = round(count_oids/UV_sum,4),
        payrat_complete  = round(count_oids/UV_sum/target_payrat,4),
        grossmargin_rate = round(grossmargin_rate,4)
      )%>%
      select(
        `目标日均UV` = target_uv,
        `日均UV` = UV_perday,
        `UV完成率` = uv_complete,
        `目标销售额` = target_sales,
        `累计销售额` = sales_total,
        `累计销售完成率` = sales_complete, 
        `目标咨询量` = target_consult,
        `日均咨询量` = consult_perday,
        `日均咨询量完成率` = consult_complete,
        `目标成交转化率` = target_payrat,
        `平均成单转化率` = payrat,
        `成单转化率完成率` = payrat_complete,
        `毛利额` = gross_margin,
        `毛利率` = grossmargin_rate
        
      )
  })
  
  
  
  output$monthkpi <- DT::renderDataTable({
    datatable(
      df1(), 
      rownames = FALSE,
      selection = 'multiple',
      extensions = list(Scroller=list()),
      options = list(
        autoWidth = TRUE,
        scrollX=TRUE,
        searching=FALSE,
        lengthChange = FALSE
      ) 
    ) 
  })
  
  df2<- reactive({
    df()%>%
      mutate(
        uv_complete = round(UV_perday/target_uv,4),
        sales_complete = round(sales_total/target_sales,4),
        consult_complete = round(consult_perday/target_consult,4),
        payrat = round(count_oids/UV_sum,4),
        payrat_complete  = round(count_oids/UV_sum/target_payrat,4)
      )%>%
      select(
        `日均注册用户数` = registers_perday,
        `日均新访客数` = newvisits_perday,
        `新访客注册转化率` = newregisters_rate,
        `累计订单数` = count_oids,
        `累计购买客户数` = mids,
        `客户购买累计金额` = mid_cumprice
      )
  })
  output$monthkpi1 <- DT::renderDataTable({
    datatable(
      df2(), 
      rownames = FALSE,
      selection = 'multiple',
      extensions = list(Scroller=list()),
      options = list(
        autoWidth = TRUE,
        scrollX=TRUE,
        searching=FALSE,
        lengthChange = FALSE
      )
    )
  })
  
  
  
  
} 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 客服月度绩效 
kefumonthKpiUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      class='toolbar',
      column(6,helpText('  1.绩效考核用的金额是发货金额（没有扣除积分、红包和退款金额)',br(),
                        
                        '  2.这里的退款金额与定位是指：发货后的成功退款的发货金额与订单')
      ),
      column(6,helpText('',br())
      )
    ),
    fluidRow(class='',
     column(2, selectInput(ns('dt'), '月份', choices = substr(as.character(seq(floor_date(Sys.Date()-365,unit = "year"), by='month', length.out = 24)), 1, 7), 
                           selected = substr(Sys.Date(),1,7))),
     column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
    ), 
    br(),
    fluidRow(
      column(12, DT::dataTableOutput(ns('tbl')))
    )
  )
}
kefumonthKpi <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  df <- reactive({
    stuff <- dbReadTable0('bbf_shiny', 'bbf_stuff')
    sql <- paste0("
                  select adminaccount, 
                  sum(sum_price) as pay,
                  count(DISTINCT oid) as cnt
                  FROM tb_porder
                  where 1 = 1 
                  and DATE_FORMAT(send_time_str,'%Y-%m')= '",dt(),"'
                  and HANDLE_STATUS not in (5 , 7 )
                  GROUP BY adminaccount
                  ")
    df1 <- dbGetQuery0('ecommerce', sql)
    
    sql <- paste0("
                  select b.adminaccount,sum(a.amount) as amount,count(distinct a.oid) as tk_cnt 
                  from 
                  tb_porder_drawback a 
                  join tb_porder b 
                  on a.oid=b.oid and DATE_FORMAT(b.send_time_str,'%Y-%m')='",dt(),"'
                  where 1=1
                  and substring(a.dualdates,1,7)='",dt(),"'
                  and b.HANDLE_STATUS not in (5 , 7 )
                  and a.dualstatus=2 
                  group by b.adminaccount
                  ")
    df2 <- dbGetQuery0('ecommerce', sql)
    
    df1 %>%
      left_join(df2, by='adminaccount') %>%
      left_join(stuff, by=c('adminaccount'='name_py')) %>%
      select(`下单人账号`=adminaccount, `工号`=stuff_id, `下单人名字`=name, `部门`=dept, `小组`=team, `发货金额`=pay, `发货单数`=cnt, `退款金额`=amount, `退款单数`=tk_cnt)
  })
  #-couponamount-score_to_cost/200
  
  # output$btn_export <- downloadHandler(paste('bbf-data-jixiao-',Sys.Date(),'.csv',sep=''), content = function(file) {
  #   if(TRUE){
  #     write.csv(df(), file, fileEncoding='gbk',row.names=FALSE)
  #   } else {
  #     write.csv('无数据下载权限', file, fileEncoding='gbk', row.names = FALSE, col.names = FALSE)
  #   }
  # })
  output$btn_export <- downloadHandler(paste('bbf-data-jixiao-',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      tmp <- df()[input$tbl_rows_all, , drop = FALSE]
      write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names = FALSE, col.names = FALSE)
    }
  })
  
  
  output$tbl <- DT::renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      extensions = 'Scroller',
      options = list(
        autoWidth = TRUE,
        searching = FALSE,
        deferRender = TRUE,
        scrollY=200,
        scrollCollapse = TRUE,
        order = list(list(5, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 200,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    ) %>% formatRound(c('发货金额', '退款金额'), 2)
  })
  
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 发货与成单金额  发货金额-红包-积分
fahuoVSchendanUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      class='toolbar',
      column(6,helpText('  1.绩效考核用的金额是发货金额（没有扣除积分、红包与退款）），包括内部绩效考核与供应商货款结算',br(),
                        '  2成单金额（排除取消与关闭,没有扣除积分、红包与退款）',br(),
                        '')
      ),
      column(6,helpText('3.差异=成单金额-发货金额',br(),
                        '4.差异率=（成单金额-发货金额）/发货金额',br()
                        )
      )
    ),
    fluidRow(class='',
       column(2, selectInput(ns('dt1'), '开始月份', choices = substr(as.character(seq(floor_date(Sys.Date()-365,unit = "year"), by='month', length.out = 24)), 1, 7), 
                             selected = substr(Sys.Date()-months(3),1,7))),
       column(2, selectInput(ns('dt2'), '结束月份', choices = substr(as.character(seq(floor_date(Sys.Date()-365,unit = "year"), by='month', length.out = 24)), 1, 7), 
                             selected = substr(Sys.Date(),1,7))),
       column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
    ), 
    br(),
    fluidRow(
      column(12, DT::dataTableOutput(ns('tbl')))
    )
  )
}
fahuoVSchendan <- function(input, output, session) {
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
                  select x1.months,x1.sumpay_send,x2.sumpay from (
                  select DATE_FORMAT(send_time_str,'%Y-%m') as months,sum(sum_price) as sumpay_send from tb_porder 
                  where 1=1
                  and DATE_FORMAT(send_time_str,'%Y-%m')>='",dt1(),"'
                  and DATE_FORMAT(send_time_str,'%Y-%m')<='",dt2(),"'
                  and HANDLE_STATUS not in (5 , 7 )
                  GROUP BY DATE_FORMAT(send_time_str,'%Y-%m')
                  ) x1
                  join (
                  select DATE_FORMAT(dt,'%Y-%m') as month1,sum(sum_price) as sumpay from (
                  SELECT
                  CASE WHEN PAY_STATUS = 1 THEN	payDates
                  WHEN PAY_TYPE = 10002 THEN	POST_DATE_STR
                  END dt,
                  t.oid, t.mid,t.sum_price,t.couponamount,t.score_to_cost
                  FROM
                  tb_porder t
                  WHERE
                  1 = 1
                  and  HANDLE_STATUS NOT IN (5, 7) 
                  AND (
                  (
                  PAY_STATUS = 1
                  
                  AND DATE_FORMAT(payDates,'%Y-%m') >='",dt1(),"'
                  AND DATE_FORMAT(payDates,'%Y-%m') <='",dt2(),"'
                  )
                  OR (
                   PAY_TYPE = 10002
                  AND DATE_FORMAT(POST_DATE_STR,'%Y-%m') >='",dt1(),"'
                  AND DATE_FORMAT(POST_DATE_STR,'%Y-%m') <='",dt2(),"'
                  )
                  )
                  ) x
                  GROUP BY DATE_FORMAT(dt,'%Y-%m')
                  ) x2
                  on x1.months=x2.month1
                  ")
    df1 <- dbGetQuery0('ecommerce', sql)
    df1 %>%
      mutate(
        a1=round(sumpay-sumpay_send,2),
        r1=round((sumpay-sumpay_send)/sumpay,4)
      ) %>%
      select(`月份`=months, `发货金额`=sumpay_send, `成单金额`=sumpay,`差异`=a1, `差异率`=r1)
  })
  
  #-couponamount-score_to_cost/200
  
  output$btn_export <- downloadHandler(paste('bbf-fahuo-',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      tmp <- df()[input$tbl_rows_all, , drop = FALSE]
      write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names = FALSE, col.names = FALSE)
    }
  })
  
  # months sumpay_send sumpay
  
  output$tbl <- DT::renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      extensions = 'Scroller',
      options = list(
        autoWidth = TRUE,
        searching = FALSE,
        deferRender = TRUE,
        scrollY=200,
        scrollCollapse = TRUE,
        #order = list(list(5, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 200,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    ) %>% formatRound(c('发货金额', '成单金额'), 0)
  })
  
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 商家月度分账统计 

merchantmonthKpiUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      class='toolbar',
      column(6,helpText( '  1.基于财务分账统计报表，并作为核对依据',br(),
                         '  2.等待20秒',br(),
                         '  3.服务费=商品发货金额-（本月退款+跨月退款）*0.05')
      ),
      column(6,helpText('',br())
      )
    ),
    fluidRow(class='',
             column(2, selectInput(ns('dt'), '月份', choices = substr(as.character(seq(floor_date(Sys.Date()-365,unit = "year"), by='month', length.out = 24)), 1, 7), 
                                   selected = substr(Sys.Date(),1,7))),
             column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
    ), 
    br(),
    fluidRow(
      column(12, DT::dataTableOutput(ns('tbl')))
    )
  )
}
merchantmonthKpi <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  df <- reactive({
    sql <- paste0("
                  select t.SHOP_CODE ,
                  replace(replace(REPLACE(REPLACE(t.merchant_name,CHAR(10),''),CHAR(13),''),char(9),''),' ','')   as merchant_name ,
                  replace(replace(replace(REPLACE(REPLACE(t.allname,CHAR(10),''),CHAR(13),''),char(9),''),' ',''),'•','')   as allname ,
                  ifnull(t2.SUM_PRICE  ,0) as pay_facetoface,
ifnull(t1.SUM_PRICE,0) as pay_online,
ifnull(t1.ACTUAL_PAY,0) as ACTUAL_PAY  , 
                  (ifnull(t1.shopCouponAmount,0) +ifnull(t2.shopCouponAmount,0)) as shopCouponAmount ,  
                  (ifnull(t1.score_to_cost,0) +ifnull(t2.score_to_cost,0))/100 as score_to_cost, 
                  ifnull(t3.dAMOUNT,0) as lastdrawback,
ifnull(t4.amount,0) as thisdrawback, 
round(ifnull(t5.service_pay,0),1) as service_pay,
ifnull(t6.yuant_pay,0) as yuant_pay
                  from 
                  (
                	select  SHOP_CODE, max(merchant_name) as merchant_name,max(allname) as allname from (
                	select DISTINCT SHOP_CODE, merchant_name,allname  from tb_porder tbp
                			where 1=1
                			and SUM_PRICE>0
                			AND DATE_FORMAT(tbp.send_time_str,'%Y-%m')='",dt(),"' 
                	union
                	SELECT tbpd.SHOPCODE ,tbp.merchant_name,tbp.allname
                	FROM tb_porder_drawback tbpd
                		LEFT JOIN tb_porder tbp ON tbp.OID = tbpd.OID
                	WHERE 1 = 1
                	#z and tbpd.SHOPCODE = 100430
                		and DATE_FORMAT(tbp.send_time_str,'%Y-%m')= DATE_FORMAT(DATE_SUB(concat('",dt(),"','-01'),INTERVAL 1 MONTH),'%Y-%m')
                		and DATE_FORMAT(tbpd.dualdates,'%Y-%m')='",dt(),"' 
                		AND tbpd.DUALSTATUS = 2
                	group by tbpd.SHOPCODE 
                	) y
                	#where SHOP_CODE=100782
                	GROUP BY SHOP_CODE
                  ) t 
                  left join 
                  (
                  -- 在线支付
                  SELECT SHOP_CODE, sum(tbp.ACTUAL_PAY) ACTUAL_PAY , sum( tbp.shopCouponAmount)shopCouponAmount ,
                  sum( tbp.SUM_PRICE) SUM_PRICE,sum( tbp.score_to_cost) score_to_cost
                  FROM tb_porder tbp
                  WHERE 1 = 1
                  # and tbp.SHOP_CODE = 100430
                  and tbp.pay_status = 1
                  AND DATE_FORMAT(tbp.send_time_str,'%Y-%m')='",dt(),"'
                  AND  tbp.HANDLE_STATUS not in (5 , 7 ) 
                  GROUP BY SHOP_CODE
                  )  t1
                  on t.SHOP_CODE = t1.SHOP_CODE
                  left join (
                  -- 货到付款 
                  SELECT SHOP_CODE ,  sum(tbp.ACTUAL_PAY) ACTUAL_PAY , sum( tbp.shopCouponAmount) shopCouponAmount,
                  sum( tbp.SUM_PRICE) SUM_PRICE,sum( tbp.score_to_cost) score_to_cost
                  FROM tb_porder tbp
                  WHERE 1 = 1
                  #  and tbp.SHOP_CODE = 100430
                  and tbp.pay_status = 0
                  AND DATE_FORMAT(tbp.send_time_str,'%Y-%m')='",dt(),"'
                  AND  tbp.HANDLE_STATUS not in (5 , 7 ) 
                  GROUP BY SHOP_CODE
                  ) t2
                  on t.SHOP_CODE =t2.shop_code 
                  left join (
                  -- 退款金额：本月发货且退款成功的（不包括退款时间在本月的就可以对上）
                  select b.SHOP_CODE,sum(a.amount) as amount,count(distinct a.oid) as tk_cnt 
                  from 
                  tb_porder_drawback a 
                  join tb_porder b 
                  on a.oid=b.oid and DATE_FORMAT(b.send_time_str,'%Y-%m')='",dt(),"'
                  where 1=1
                  #and DATE_FORMAT(a.dualdates,'%Y-%m')='",dt(),"' 
                  and a.dualstatus=2 
                  #and b.SHOP_CODE = 100430
                  AND  b.HANDLE_STATUS not in (5 , 7 ) 
                  group by b.SHOP_CODE
                  )t4
                  on t.SHOP_CODE =t4.shop_code
                  left join (
                  -- 跨月退款,不包括订单取消和关闭的就可以对上
                  SELECT tbpd.SHOPCODE ,
                  sum(tbpd.AMOUNT) AS dAMOUNT 	 
                  FROM tb_porder_drawback tbpd
                  LEFT JOIN tb_porder tbp ON tbp.OID = tbpd.OID
                  WHERE 1 = 1
                  #z and tbpd.SHOPCODE = 100430
                  and DATE_FORMAT(tbp.send_time_str,'%Y-%m')=DATE_FORMAT(DATE_SUB(concat('",dt(),"','-01'),INTERVAL 1 MONTH),'%Y-%m')
                  and DATE_FORMAT(tbpd.dualdates,'%Y-%m')='",dt(),"'
                  AND tbpd.DUALSTATUS = 2
                  group by tbpd.SHOPCODE 
                  ) t3
                  on t.SHOP_CODE =t3.SHOPCODE 
                  left join (

                  -- 服务费 11月的服务费 
                  SELECT shop_code,sum(AMOUNT*yongjinlv) as service_pay from (
                  select   x.shop_code,x.oid,a.pid,a.AMOUNT ,
                  case when b.oid is not null  then f.rate  else  a.BCBILV  end yongjinlv  
                  from 
                  ( select shop_code,oid from tb_porder t
                  where 1=1
                  AND DATE_FORMAT(t.send_time_str,'%Y-%m')='",dt(),"'
                  and 	t.HANDLE_STATUS NOT IN (5, 7) 
                  #and shop_code ='71000'
                  ) x
                  join tb_porder_line a 
                  on x.oid=a.oid
                  left join  tb_porder_transfer b 
                  on a.oid = b.oid  and b.check_status = 1
                  left join tb_product_info c on a.pid = c.pid
                  left join tb_drug_base d  on c.DID = d.DID
                  left join TB_CATALOG_BASE e on d.CID = e.ID
                  left join TB_MERCHANT_CATALOG_RATE f  on b.new_shopcode = f.shop_code and f.bcid = substr(e.ALLPIDS,1,1) 
              ) xx
              GROUP BY shop_code
                  ) t5
                  on t.SHOP_CODE =t5.SHOP_CODE 
                  left join (
                  -- 圆通货到付款发货金额
                  select SHOP_CODE,sum(SUM_PRICE) as yuant_pay from tb_porder tbp
                  where 1 = 1 
                  and tbp.pay_type = 10002 
                  and tbp.deliveryname  LIKE '%圆通货到付款%'
                  and DATE_FORMAT(tbp.send_time_str,'%Y-%m')='",dt(),"'
                  group by SHOP_CODE
                  ) t6
                  on t.SHOP_CODE =t6.SHOP_CODE 
                  ")
    dbGetQuery0('ecommerce', sql)%>%
      mutate(
        fahuo_total=pay_facetoface+pay_online,
        service_pay1=service_pay-(lastdrawback+thisdrawback)*0.05,
        should_paymerchants=ACTUAL_PAY+shopCouponAmount+score_to_cost-service_pay1-lastdrawback-thisdrawback
      )%>%
    select(
    `商家编码`=SHOP_CODE, 
    `商家名`=merchant_name, 
    `商家公司名`=allname,
    `圆通货到付款金额`=yuant_pay, 
    `货到付款发货金额`=pay_facetoface, 
    `在线付款发货金额`=pay_online, 
    `平台发货总额`=fahuo_total, 
    `到账金额`=ACTUAL_PAY,
    `红包金额`=shopCouponAmount, 
    `积分金额`=score_to_cost,
    `跨月退款金额`=lastdrawback,
    `本月退款金额`=thisdrawback,
    `服务费-未扣退款`=service_pay,
    `服务费`=service_pay1,
    `应付商家款`=should_paymerchants
    )
  })
  # '",dt(),"' 上一个月怎么写  实际付款金额=到账金额
  output$btn_export <- downloadHandler(paste('bbf-merchant-fztj-',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      tmp <- df()[input$tbl_rows_all, , drop = FALSE]
      write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names = FALSE, col.names = FALSE)
    }
  })
  
  
  output$tbl <- DT::renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      extensions = 'Scroller',
      options = list(
        autoWidth = TRUE,
        searching = TRUE,
        deferRender = TRUE,
        scrollY=200,
        scrollX=TRUE,
        scrollCollapse = TRUE,
        #order = list(list(5, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 200,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    )
    #%>% formatRound(c('发货金额', '退款金额'), 2)
  })
  
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

