# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 商家销售分析
merchantUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(class='',
             column(2, dateRangeInput(ns('dt'), NULL, language='zh_CN', start = Sys.Date()-months(1)-1, end = Sys.Date()-1)),
             #column(1, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap')),
             column(2,offset = 2, selectInput(ns('shop'), NULL, choices = c('同康柳影店', '北京好药师大药房')))
    ),
    br(),
    fluidRow(
      column(4, DT::dataTableOutput(ns('tbl'))),
      column(8, DT::dataTableOutput(ns('tbl2')))
      #column(4, plotOutput(ns('p3'), height = "300px"), plotOutput(ns('p4'), height = "300px"))
    )
  )
}
merchant <- function(input, output, session){
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  shop <- reactive({
    req(input$shop)
    input$shop
  })
  
  df0 <- reactive({
    dbGetQuery0('ecommerce', paste0("SELECT
	                                  merchant_name,
                                    count(distinct mid) mids ,
                                    sum(sum_price) sum_price
                                    FROM
                                    tb_porder
                                    WHERE 1 = 1 
                                    AND HANDLE_STATUS NOT IN (5, 7)
                                    AND (
                                    (
                                    PAY_STATUS = 1
                                    AND date(payDates) BETWEEN '",dt()[1],"' AND '",dt()[2],"'
                                    )
                                    OR (
                                    PAY_TYPE = 10002
                                    AND date(POST_DATE_STR) BETWEEN '",dt()[1],"' AND '",dt()[2],"'
                                    )
                                    )
                                    AND actual_pay < 10000
                                    group by merchant_name 
                                    having count(distinct mid) > 10 
                                    "))
  })
  
  df <- reactive({
    df0() %>% 
      #filter(mids>10, !is.na(merchant_name)) %>% 
      mutate(
        pct=df0()[,3]/sum(df0()[,3]),
        pay =sum_price/10000,
        kd = sum_price/mids
      ) %>% 
      select(`商家`=merchant_name, `买家`=mids, `销售/万`=pay, `客单`=kd, `占比`=pct)
  })
  
  observe({
    if (!is.null(input$tbl_rows_selected)) {
      merchant <- df()$`商家`[input$tbl_rows_selected]
      updateSelectInput(session,'shop', choices = merchant)
    }
  })
  
  df2 <- reactive({
    s <- ifelse(is.null(input$tbl_rows_selected), input$tbl_rows_all, input$tbl_rows_selected)
    merchant <- paste(df()$`商家`[input$tbl_rows_selected], collapse="','")
    dbGetQuery0('ecommerce', paste0("select b.merchant_name,a.pid,a.name,a.shop_price,a.quantity, a.amount from tb_porder_line a inner join tb_porder b on a.oid=b.oid and b.merchant_name in ('",merchant,"') and b.post_date_str>='",dt()[1],"' and b.post_date_str<='",dt()[2],"'")) %>%
      group_by(merchant_name, pid) %>%
      summarise(
        name = max(name),
        shop_price = mean(shop_price),
        quantity = sum(quantity, na.rm=TRUE),
        amount = sum(amount, na.rm = TRUE)
      )
  })
  
  # df3 <- reactive({
  #   s <- ifelse(is.null(input$tbl_rows_selected), input$tbl_rows_all, input$tbl_rows_selected)
  #   merchant <- df()$`商家`[input$tbl_rows_selected]
  #   df0() %>% 
  #     filter(merchant_name %in% merchant)
  # })
  
  output$tbl <- DT::renderDataTable({
    datatable(
      df()   ,
      escape = FALSE,
      rownames = FALSE,
      #selection = list(mode = 'multiple', selected = c(1, 2)),
      selection = list(selected = c(1, 2)),
      extensions = list(Scroller=list()),
      options = list(
        autoWidth = TRUE,
        searching = TRUE,
        deferRender = TRUE,
        scrollY=200,
        scrollX=TRUE,
        scrollCollapse = TRUE,
        order = list(list(2, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 100,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-200)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-200;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    ) %>% formatPercentage(c('占比'), 2) %>% formatRound(c('客单', '销售/万'), 2)
  })
  
  output$tbl2 <- DT::renderDataTable({
    datatable(
      df2() %>% ungroup() %>% filter(merchant_name == shop()) %>% mutate(pct = amount/sum(amount, na.rm=TRUE)) %>% select(`商品` = name, `价格` = shop_price, `数量` = quantity, `销售` = amount, `占比` = pct),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      extensions = list(Scroller=list()),
      options = list(
        autoWidth = TRUE,
        searching = FALSE,
        deferRender = TRUE,
        scrollY=200,
        scrollX=TRUE,
        scrollCollapse = TRUE,
        order = list(list(3, 'desc')),
        #columnDefs = list(list(width = '50px', targets = c(0))),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 100,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('#DataTables_Table_0_wrapper > div.dataTables_scroll > div.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-200) +'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = ($(window).height()-200);$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    ) %>% formatPercentage(c('占比'), 2) %>% formatRound(c('数量', '销售', '价格'), 0)
  })
  
  # output$p3 <- renderPlot({
  #   showtext.auto()
  #   #df2() %>% ggplot(aes(lbl))+geom_bar()+facet_wrap(~merchant_name)+theme(axis.text.x = element_text(angle = 90, hjust = 1), text=element_text(family = 'wqy'))+labs(title='客单价分布', x = '客单价', y = '人数')
  #   p <- df3() %>%
  #     # group_by(merchant_name, mid) %>% 
  #     # summarise(
  #     #   actual_pay = sum(actual_pay, na.rm=TRUE)
  #     # ) %>% 
  #     #mutate(lbl = cut(sum_price, c(seq(from=0, to=1000, by=50), max(sum_price)))) %>% 
  #     #filter(sum_price<1000) %>%
  #     ggplot()+
  #     geom_density(aes(sum_price, color=merchant_name, fill=merchant_name, alpha=0.5), show_guide=FALSE) +
  #     #stat_density(aes(x=sum_price, colour=merchant_name), geom="line", position="identity") +
  #     #geom_vline(data=df3() %>% group_by(merchant_name) %>% summarise( m=sum(sum_price, na.rm=TRUE)/n()), aes(xintercept = m, colour=merchant_name), linetype = "longdash") +
  #     theme(axis.text.x = element_text(angle = 90, hjust = 1), text=element_text(family = 'wqy-microhei')) +
  #     labs(x = '客单价', y = '占比')
  #   direct.label(p, list('top.points', cex=0.8))
  # })
  
  # output$p4 <- renderPlot({
  #   showtext.auto()
  #   p <- df2() %>% 
  #     filter(shop_price<200) %>% 
  #     ggplot()+
  #     geom_density(aes(shop_price, color=merchant_name, fill=merchant_name, alpha=0.5), show_guide=FALSE) +
  #     stat_density(aes(x=shop_price, colour=merchant_name), geom="line", position="identity") +
  #     geom_vline(data=df2() %>% group_by(merchant_name) %>% summarise(m=sum(shop_price, na.rm=TRUE)/n()), aes(xintercept = m, colour=merchant_name), linetype = "longdash") +
  #     theme(axis.text.x = element_text(angle = 90, hjust = 1), text=element_text(family = 'wqy-microhei')) +
  #     labs(x = '商品价格', y = '占比')
  #   direct.label(p, list('top.points', cex=0.8))
  # })
}
 









#------------------------非重点商家高动销商品监测-------------------
commonMerchantUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '日期',  start = Sys.Date()-2, end = Sys.Date()-1)),
             column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
    ),
    fluidRow(
      column(12,dataTableOutput(ns('tbl'))) 
      #column(7,plotlyOutput(ns('chnl')))
    )
  )
}
commonMerchant <- function(input, output, session){
  
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  
  # 
  # timest >= '", format(pay_date()[1],'%Y%m%d') ,"'
  #  timest <=  '",format(pay_date()[2],'%Y%m%d') ,"'
  
  df <- reactive({
    sql <-
      paste0("  
             select t1.merchant_name,
             t1.shop_code,
             t1.pid,
             t1.name,
             t1.proname,
             t1.pzwh,
             CASE
             WHEN t1.ischufang = 0 THEN
             '非药物'
             WHEN t1.ischufang = 1 THEN
             '处方药'
             WHEN t1.ischufang = 2 THEN
             '非处方药'
             END AS ischufang,
             t1.ctitle0,
             t1.ctitle1,
             t1.ctitle2,
             t1.specification,
             t1.company,
             t1.shop_price,
             case
             when t1.verify = 0 then
             '待审核'
             when t1.verify = 1 then
             '审核通过'
             when t1.verify = 2 then
             '审核未通过'
             when t1.verify = 3 then
             '废弃'
             else
             '未知'
             end verify,
             case
             when t1.ALIVE = 0 then
             '否'
             when t1.ALIVE = 1 then
             '是'
             when t1.ALIVE = 2 then
             '违规商品(强制下架,商城下的)'
             when t1.ALIVE = 3 then
             '在售更新商品'
             when t1.ALIVE = 4 then
             '再次上架'
             when t1.ALIVE = 5 then
             '新发布商品'
             end ALIVE,
             'http://www.800pharm.com/shop/product-' || t1.shop_code || '-' ||
             t1.pid || '.html' url,
             nvl(t3.oids, 0) oids,
             nvl(t3.quantity, 0) quantity,
             nvl(t3.amount, 0) amount,
             nvl(t4.pids, 0) pids,
             nvl(t5.min_price || '--' || t5.max_price, 0) rang_price,
             nvl(t6.type_a_cnts, 0) type_a_cnts
             from tb_product t1
             join (select merchant_code ，pact_start, pact_end
             from tb_merchant a
             where 1 = 1
             and pact_start is not null
             and ((is_lock = 1 and pact_start is not null and
             months_between(sysdate, pact_end) <= 6) or is_open = -1 or
             (privilege not in (2, 3) and is_lock = 0 and is_open <> -1))) t2
             on t1.shop_code = t2.merchant_code
             --销售
             join (select pid,
             count(distinct oid) oids,
             sum(amount) amount,
             sum（quantity) quantity
             from (select case
             when a.pay_status = 1 then
             a.paydates
             when a.pay_type = 10002 then
             a.post_date_str
             end dt,
             pid,
             a.oid,
             a.shop_code， b.quantity,
             b.amount
             from tb_porder a
             join tb_porder_line b
             on a.oid = b.oid
             where 1 = 1
             and a.handle_status not in (5, 7)
             and (a.pay_status = 1 or a.pay_type = 10002))
             where to_char(dt, 'yyyymmdd') >= '", format(pay_date()[1],'%Y%m%d') ,"'
             and  to_char(dt, 'yyyymmdd') <= '", format(pay_date()[2],'%Y%m%d') ,"'
             group by pid
             having count(distinct oid) >= 10
      ) t3
             on t1.pid = t3.pid
             --  此商品现有在售商品数
             left join (select a.pzwh, a.specification, count(distinct pid) pids
             from tb_product a
             where 1 = 1
             and a.alive not in (0, 2) -- 在售
             and a.ischufang in (1, 2) -- 药品 
             and a.verify = 1
             group by a.pzwh, a.specification) t4
             on t1.pzwh = t4.pzwh
             and t1.specification = t4.specification
             --价格区间
             left join (select a.pzwh,
             a.specification,
             min(a.shop_price) min_price,
             max(a.shop_price) max_price
             from tb_product a
             where a.ischufang in (1, 2)
             group by a.pzwh, a.specification) t5
             on t1.pzwh = t5.pzwh
             and t1.specification = t5.specification
             left join (
             -- 此商品在售的金牌商家数
             select a.pzwh,
             a.specification,
             count(distinct case
             when b.privilege = 3 then
             shop_code
             else
             null
             end) type_a_cnts -- 金牌商家数
             from tb_product a
             join tb_merchant b
             on a.shop_code = b.merchant_code
             and b.privilege = 3
             where 1 = 1
             and a.alive not in (0, 2) -- 在售
             and a.ischufang in (1, 2) -- 药品
             and a.verify = 1
             group by a.pzwh, a.specification) t6
             on t1.pzwh = t6.pzwh
             and t1.specification = t6.specification
             where 1 = 1
             -- and t1.alive not in (0, 2)
             -- and t1.ischufang in (1, 2)
             and t1.verify = 1 
             ")
    
    
    
    dd <- dbGetQuery1(sql)
    
    df <- dd  %>% 
      mutate(rang_price=as.character(RANG_PRICE)) %>% 
      dplyr::rename(`商家名称` = MERCHANT_NAME,
                    `商家代码` = SHOP_CODE,
                    `商品编号` = PID,
                    `商品名称` = NAME,
                    `通用名称` = PRONAME,
                    `批准文号` = PZWH,
                    `商品类别` = ISCHUFANG,
                    `一级分类` = CTITLE0,
                    `二级分类` = CTITLE1,
                    `三级分类` = CTITLE2,
                    `规格` = SPECIFICATION,
                    `生产厂商` = COMPANY,
                    `商城价` = SHOP_PRICE,
                    `审核状态` = VERIFY,
                    `销售状态` = ALIVE,
                    `产品链接` = URL,
                    `销售次数` = OIDS,
                    `销售数量` = QUANTITY,
                    `销售金额` = AMOUNT,
                    `此商品现有在售商品数` = PIDS,
                    `价格区间` = RANG_PRICE,
                    `此商品现有在售的金牌商家数` = TYPE_A_CNTS
      ) %>% arrange(desc(`销售金额`))
    
  }) 
  
  
  output$tbl <- renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      #extensions = list(FixedColumns = list(leftColumns = 1 ), Scroller=list()),
      extensions = c('FixedColumns', 'Scroller'),
      options = list(
        autoWidth = TRUE,
        searching=FALSE,
        lengthChange = TRUE,
        #pageLength = 30,
        scrollX=TRUE
      )
    ) 
  })
  
  
  # export
  output$btn_export <- downloadHandler(paste('bbf-data-commonMerchant',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      write.csv(df(), file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
    }
    
  })
  
  # output$chnl <- renderPlotly({ 
  # })
  
}
















#------------------------重点商家高动销商品监测-------------------
keyMerchantUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '日期',  start = Sys.Date()-30, end = Sys.Date()-1)),
             column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
    ),
    fluidRow(
      column(12,dataTableOutput(ns('tbl')))  
    )
  )
}
keyMerchant <- function(input, output, session){
  #  未登陆天数  获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  
  # 
  # timest >= '", format(pay_date()[1],'%Y%m%d') ,"'
  #  timest <=  '",format(pay_date()[2],'%Y%m%d') ,"'
  
  df <- reactive({
    sql <-
      paste0("  
             select a.merchant_name , 
             a.merchant_code shop_code,
             b.pid ,
             b.name ,
             b.proname,
             b.pzwh  ,
             CASE
             WHEN b.ischufang = 0 THEN
             '非药物'
             WHEN b.ischufang = 1 THEN
             '处方药'
             WHEN b.ischufang = 2 THEN
             '非处方药'
             END AS ischufang ,
             b.ctitle0 ,
             b.ctitle1 ,
             b.ctitle2 ,
             b.specification ,
             b.company,
             b.shop_price ,
             case when b.verify = 0 then '待审核'
             when b.verify = 1 then '审核通过'   
             when b.verify = 2 then '审核未通过'
             when b.verify = 3 then '废弃' else '未知'
             end verify,
             case 
             when b.ALIVE     = 0 then  '否'
             when b.ALIVE = 1 then '是'
             when b.ALIVE = 2 then '违规商品(强制下架,商城下的)'
             when b.ALIVE = 3 then '在售更新商品'
             when b.ALIVE = 4 then '再次上架'
             when b.ALIVE = 5 then '新发布商品'
             end ALIVE ,
             'http://www.800pharm.com/shop/product-' || b.shop_code || '-' ||       b.pid || '.html' url ,
             nvl(t3.oids,0) oids,
             nvl(t3.quantity,0) quantity,
             nvl(t3.amount ,0)    amount         
             from (select merchant_code ,merchant_name
             from tb_merchant 
             where 1 = 1
             and privilege = 3) a
             join tb_product b on a.merchant_code = b.shop_code and b.verify = 1 and b.alive not in (0 ,2 )
             left join ( 
             select   pid, count(distinct oid) oids , sum(amount) amount , sum（quantity) quantity 
             from (select case
             when a.pay_status = 1 then
             a.paydates
             when a.pay_type = 10002 then
             a.post_date_str
             end dt,
             pid,
             a.oid,
             a.shop_code，
             b.quantity ,
             b.amount 
             from tb_porder a
             join tb_porder_line b
             on a.oid = b.oid
             where 1 = 1
             and a.handle_status not in (5, 7)
             and (a.pay_status = 1 or a.pay_type = 10002)
             )
             where to_char(dt, 'yyyymmdd') >= '", format(pay_date()[1],'%Y%m%d') ,"'
             and  to_char(dt, 'yyyymmdd') <= '", format(pay_date()[2],'%Y%m%d') ,"'
             group by  pid 
      )  t3  on b.pid = t3.pid 
             where 1 = 1     
             ")
    
    
    
    dd <- dbGetQuery1(sql)
    
    df <- dd  %>% 
      dplyr::rename(`商家名称` = MERCHANT_NAME,
                    `商家代码` = SHOP_CODE,
                    `商品编号` = PID,
                    `商品名称` = NAME,
                    `通用名称` = PRONAME,
                    `批准文号` = PZWH,
                    `商品类别` = ISCHUFANG,
                    `一级分类` = CTITLE0,
                    `二级分类` = CTITLE1,
                    `三级分类` = CTITLE2,
                    `规格` = SPECIFICATION,
                    `生产厂商` = COMPANY,
                    `商城价` = SHOP_PRICE,
                    `审核状态` = VERIFY,
                    `销售状态` = ALIVE,
                    `产品链接` = URL,
                    `销售次数` = OIDS,
                    `销售数量` = QUANTITY,
                    `销售金额` = AMOUNT
      ) %>% arrange(desc(`销售金额`))
    
  }) 
  
  
  output$tbl <- renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      #extensions = list(FixedColumns = list(leftColumns = 1 ), Scroller=list()),
      extensions = c('FixedColumns', 'Scroller'),
      options = list(
        autoWidth = TRUE,
        searching=FALSE,
        lengthChange = TRUE,
        #pageLength = 30,
        scrollX=TRUE
      )
    ) 
  })
  
  
  # export
  output$btn_export <- downloadHandler(paste('bbf-data-commonMerchant',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      write.csv(df(), file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
    }
    
  })
  
  # output$chnl <- renderPlotly({ 
  # })
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#------------------------圆通货到付款-------------------
#查询开通圆通的商家数量；圆通货到付款的销售趋势
yuantongHdfkUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    fluidRow(
      box(
      title = '开通圆通活动付款商家分布', status = 'primary', solidHeader = T, width = 12,
      br(),
      br(),
      column(6,helpText(h3('说明：'),br(),
                        '1.查询开通圆通的商家数量,其中1表示正常开通圆通货到付款的商家',br(),
                        '2.数据排除了商品状态锁定、禁用以及物流方式全关闭的行为',br(), 
                        '3.是否开通货到付款商家：1-开通，0-没有开通',br()
                        ) ),
      column(4, dataTableOutput(ns('tbl2')) )
      )
    ),
    br(),
    br(),
    br(),
    fluidRow(
      box(
        title = '圆通货到付款销售趋势', status = 'primary', solidHeader = T, width = 12,
             column(4, dateRangeInput(ns('dt'), '成单时间', start = Sys.Date()-30, end = Sys.Date()-1)),
             column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
      )      
    ) ,
    br(),
    br(),
    fluidRow(
      column(8, plotlyOutput(ns('trend1')) ),
      column(4,dataTableOutput(ns('tbl')) )
    )
  )
}
yuantongHdfk <- function(input, output, session) {
  df1 <- reactive({
    dbGetQuery0('ecommerce', paste0("
                                    SELECT  cashOnDelivery, count(DISTINCT MERCHANT_CODE) as cnts
                                    from tb_merchant
                                    where 1=1
                                    and is_lock=0
                                    and is_open=1
                                    and  merchant_code in (
                                    select shopcode from tb_shipping_template
                                    GROUP BY shopcode 
                                    HAVING sum(disabled)>0
                                    )
                                    GROUP BY cashOnDelivery
                                    ")
      )%>%                        
      # mutate(name=substring(name,1,10)) %>%
      select(
        `是否开通圆通货到付款`=cashOnDelivery,
        `商家数量` = cnts
      )
  })
  output$tbl2 <- DT::renderDataTable({
    datatable(
      df1(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'multiple',
      extensions = list(Scroller=list()),
      options = list(
        searching=FALSE,
        lengthChange = FALSE
    )
    )
  })
  
  
  
  
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  df <- reactive({
    dbGetQuery0('ecommerce', paste0("
                                    select dt,	sum(sum_price) as amount,count(DISTINCT oid) as oids from (
                                    SELECT 	
                                    CASE WHEN PAY_STATUS = 1 THEN	date_format(payDates,'%Y%m%d')
                                    WHEN  PAY_TYPE = 10002 THEN	date_format(POST_DATE_STR,'%Y%m%d')
                                    END dt,
                                    oid,
                                    sum_price
                                    from tb_porder
                                    WHERE 1 = 1
                                    and  pay_type = 10002 
                                    and deliveryname  LIKE '%圆通货到付款%'
                                    AND  HANDLE_STATUS not in (5 , 7 )
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
                                    group by dt
                                    "))%>%                        
      # mutate(name=substring(name,1,10)) %>%
      select(
        `成单日期`=dt,
        `成单金额`=amount,
        `订单数` = oids
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
  # 这里画图，时间趋势图。
  output$trend1 <- renderPlotly({
    x1 <- df()
    plot_ly(x1, x = ~`成单日期`) %>%
      add_lines(y = ~`成单金额`)%>% add_markers(y = ~`成单金额`)
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
        #scrollY=200,
        scrollX=TRUE,
        scrollCollapse = TRUE,
        #order = list(list(11, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        #pageLength = 200,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-200)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-200;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    )
  })
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~