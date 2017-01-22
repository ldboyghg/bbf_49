# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 热卖商品
topProductUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('dt'), '付款时间', start = Sys.Date()-1, end = Sys.Date()-1)),
             column(2, downloadButton(ns('btn_export'), '导出商品汇总表', class = 'btn_nowrap'))
#             column(2, downloadButton(ns('btn_export_detail'), '导出所选商品的订单明细', class = 'btn_nowrap'))
    ) ,
    fluidRow(
      DT::dataTableOutput(ns('tbl'))
    )
  )
}
topProduct <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  df <- reactive({
    dbGetQuery0('ecommerce', paste0("
      SELECT  	a.oid ,a.pid,	a.name,	a.proname, a.proPzwh,b.merchant_name,
				 c.ctitle0,	c.ctitle1,	c.ctitle2,
        a.quantity,	a.amount ,	a.guige,	a.bcbilv,	a.commission,
        b.mid as account
        FROM
        tb_porder_line a
        INNER JOIN tb_porder b 
        ON a.oid = b.oid
        INNER JOIN tb_product_catalogbase c ON c.pid = a.pid
        WHERE 1=1
        and b.HANDLE_STATUS NOT IN (5, 7)
        AND (
          (
            b.PAY_STATUS = 1
            AND date(b.payDates) >= '",dt()[1],"'
            AND date(b.payDates) <= '",dt()[2],"'
          )
          OR (
            b.PAY_TYPE = 10002
            AND date(b.POST_DATE_STR) >= '",dt()[1],"'
            AND date(b.POST_DATE_STR) <= '",dt()[2],"'
            )
        )
  "))
  })
  #and date(dt)>='",dt()[1],"' and date(dt)<='",dt()[2],"'
  df2 <- reactive({
    x <- df() %>% 
      group_by(pid) %>% 
      mutate(name=substring(name,1,10)) %>% 
      summarise(
        name=max(name),
        proname = max(proname),
        proPzwh=max(proPzwh),
        merchant_name=max(merchant_name),
        pcnt = n_distinct(pid),
        oids = n_distinct(oid),
        ctitle0 = max(ctitle0),
        ctitle1 = max(ctitle1),
        ctitle2 = max(ctitle2),
        quantity = sum(quantity),
        amount = sum(amount),
        guige = max(guige),
        bcbilv = max(bcbilv),
        commission = sum(commission),
        account = n_distinct(account)
      ) %>% 
      arrange(desc(amount)) %>% 
      ungroup() %>% 
      # mutate(name=ifelse(nchar(proname)==0, name, paste(name, proname, sep=' / '))) %>% 
      select(
        `pid`=pid,
        `商品名称`=name,
        `通用名称` = proname,
        `销售量` = quantity,
        `销售额` = amount,
        `批准文号`=proPzwh,
        `商家名称` = merchant_name,
        `商品个数` = pcnt,
        `一级分类` = ctitle0,
        `二级分类` = ctitle1,
        `三级分类` = ctitle2,
        `购买人数(排重)` = account,
        `购买订单数`=oids,
        `规格` = guige,
        `分成比率` = bcbilv,
        `分成总额` = commission
      )
    # x %>% add_row(
    #     `商品名称/通用名称` = '合计',
    #     `商品个数` = sum(x$`商品个数`, na.rm=TRUE),
    #     `一级分类` = '',
    #     `二级分类` = '',
    #     `三级分类` = '',
    #     `购买人数(排重)` = sum(x$`购买人数(排重)`, na.rm=TRUE),
    #     `销售量` = sum(x$`销售量`, na.rm=TRUE),
    #     `销售额` = sum(x$`销售额`, na.rm=TRUE),
    #     `规格` = '',
    #     `分成比率` = '',
    #     `分成总额` = ''
    # )
  })
  
  # export
  output$btn_export <- downloadHandler(paste('bbf-data-callcenter-',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      tmp <- df2()[input$tbl_rows_all, , drop = FALSE]
      write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
    }
    
  })

  
  # export detail
  # output$btn_export_detail <- downloadHandler(paste('bbf-data-order-',Sys.Date(),'.csv',sep=''), content = function(file) {
  #   if(TRUE){
  #     r = ifelse(is.null(input$tbl_rows_selected), 1, input$tbl_rows_selected)
  #     p <- df2()[r, , drop = FALSE]$`商品名称/通用名称`
  #     p <- strsplit(p, ' / ') %>% unlist()
  #     tmp <- df() %>% filter(name %in% p) %>% select(`下单姓名`=xd_name, `下单电话`=xd_mobile, `收货姓名`=sh_name, `收货电话`=sh_mobile, `省份`=province_name,`城市`=city_name,`地址`=address, `下单时间`=post_date_str, `商品`=name, `订单金额`=amount)
  #     write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
  #   } else {
  #     write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
  #   }
  #   
  # })
  output$tbl <- DT::renderDataTable({
    datatable(
      df2(),
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
        order = list(list(11, 'desc')),
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
# 类目复购
cateRebuyUI <- function(id){
  ns <- NS(id)
  # cate_ <- dbGetQuery0('ecommerce', "select distinct ctitle1 from tb_product_catalogbase")
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('dt'), '付款时间', start = Sys.Date()-30, end = Sys.Date()-1)),
             column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
    ),
    fluidRow(
      DT::dataTableOutput(ns('tbl'))
    )
  )
}
cateRebuy <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  cate <- reactive({
    req(input$cate)
    input$cate
  })
  df <- reactive({
    sql <- paste0("
            SELECT
              t.mid, 
            	c.ctitle1,
            	c.ctitle2,
            CASE WHEN PAY_STATUS = 1 THEN	date(payDates)
            			 WHEN  PAY_TYPE = 10002 THEN	date(POST_DATE_STR)
            	END dt
            FROM
            	tb_porder t
            JOIN tb_porder_line b ON t.oid = b.oid
            JOIN tb_product_catalogbase c ON c.pid = b.pid
            WHERE	1 = 1
            and 	HANDLE_STATUS NOT IN (5, 7)
            AND (
            		(
            			PAY_STATUS = 1
            			AND date(payDates) >= '", dt()[1], "'
            			AND date(payDates) <= '", dt()[2], "'
            		)
            		OR (
            			PAY_TYPE = 10002
            			AND date(POST_DATE_STR) >= '", dt()[1], "'
            			AND date(POST_DATE_STR) <= '", dt()[2], "'
            		)
            )
                  ")
    x <- dbGetQuery0('ecommerce', sql) %>% 
      unique() %>% 
      mutate(dt = as.Date(dt)) %>% 
      # 按三级类目+会员ID分组汇总
      group_by(ctitle2, mid) %>% 
      summarise(
        r = as.numeric(Sys.Date() - max(dt)),
        f = n()
      ) %>% 
      filter(!is.na(ctitle2)) %>% 
      mutate(
        f = ifelse(f==1, '1次', '2次及以上')
      ) %>% 
      reshape2::acast(ctitle2~f)
    rn <- rownames(x)
    x <- x %>% as.data.frame() %>% mutate(`重复购买率`= `2次及以上`/(`1次`+`2次及以上`))
    rownames(x) <- rn
    x
  })
  
  # export
  output$btn_export <- downloadHandler(paste('bbf-day_lost-',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      #tmp <- df()[input$tbl_rows_all, , drop = FALSE]
      write.csv( df(), file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
    }
    
  })
  
  output$tbl <- DT::renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = TRUE,
      selection = 'none',
      extensions = list(Scroller=list()),
      options = list(
        autoWidth = TRUE,
        searching=TRUE,
        deferRender=TRUE,
        scrollY=200,
        scrollX=TRUE,
        scrollCollapse = TRUE,
        order = list(list(1, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 200,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-220)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-220;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    ) %>% formatPercentage(c('重复购买率'), 2)
  })
  
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 商品复购
productRebuyUI <- function(id){
  ns <- NS(id)
  cate_ <- dbGetQuery0('ecommerce', "select distinct ctitle1 from tb_product_catalogbase where ctitle1 is not null")
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('dt'), '付款时间', start = Sys.Date()-months(3)-1, end = Sys.Date()-1)),
             column(2, selectInput(ns('cate'), '类目', choices = cate_$ctitle1, selected = '男科用药')),
             column(2, numericInput(ns('f2'), '复购人数大于X人', value = 5))
    ),
    fluidRow(
      DT::dataTableOutput(ns('tbl'))
    )
  )
}
productRebuy <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  cate <- reactive({
    req(input$cate)
    input$cate
  })
  f2 <- reactive({
    req(input$f2)
    input$f2
  })
  
  df <- reactive({
    sql <- paste0("select b.mid, a.name, a.pid,b.actual_pay as pay, substring(b.post_date_str, 1, 10) as dt from tb_porder_line a inner join tb_porder b on a.oid = b.oid inner join tb_product_catalogbase c on c.pid=a.pid where date(b.post_date_str)>='", dt()[1], "' and date(b.post_date_str)<='",dt()[2],"' and b.handle_status in (1,4,6,99) and c.ctitle1='",cate(),"'")
    x <- dbGetQuery0('ecommerce', sql) %>% 
      unique() %>% 
      mutate(dt = as.Date(dt)) %>% 
      # 按商品名称+会员ID分组汇总计算
      group_by(name, mid) %>% 
      summarise(
        r = as.numeric(Sys.Date() - max(dt)),
        f = n(),
        pay = sum(pay, na.rm=TRUE)
      ) %>% 
      filter(!is.na(name)) %>% 
      group_by(name) %>%
      summarise(
        `购买1次人数` = sum(f==1),
        `购买2次及以上人数` = sum(f>1),
        `重复购买金额` = sum(ifelse(f>1, pay, 0), na.rm=TRUE),
        `重复购买率` = `购买2次及以上人数`/(`购买1次人数`+`购买2次及以上人数`),
        `平均回购周期(天)` = mean(r, na.rm=TRUE),
        `回购周期中位数(天)` = median(r, na.rm=TRUE)
      ) %>%
      rename(`产品`=name) %>% 
      filter(`购买2次及以上人数` >= f2())
    x %>% add_row(
      `产品` = '合计',
      `购买1次人数` = sum(x$`购买1次人数`),
      `购买2次及以上人数` = sum(x$`购买2次及以上人数`),
      `重复购买金额` = sum(x$`重复购买金额`),
      `重复购买率` = sum(x$`购买2次及以上人数`)/(sum(x$`购买1次人数`)+sum(x$`购买2次及以上人数`)),
      `平均回购周期(天)` = mean(x$`平均回购周期(天)`),
      `回购周期中位数(天)` = mean(x$`回购周期中位数(天)`)
    )
  })
  
  output$tbl <- DT::renderDataTable({
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
        scrollY=200,
        scrollCollapse = TRUE,
        order = list(list(1, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 200,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-220)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-220;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    ) %>% formatPercentage(c('重复购买率'), 2) %>% formatRound(c('重复购买金额', '平均回购周期(天)', '回购周期中位数(天)'), 2)
  })
  
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#------------------------商家销售排名-------------------
merchantRankUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-1, end = Sys.Date()-1)),
             column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
    ),
    fluidRow(
      dataTableOutput(ns('tbl'))
    )
  )
}
merchantRank <- function(input, output, session) {
  
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  
  
  
  
  df <- reactive({
    # sql <-
    #   paste0("
    #          select x.* , y.jy_Pids from (
    #          select shop_code ,merchant_name,sum(sum_price) amount,sum(quantity) quantity ,count(distinct oid) oids ,count(distinct proname ) dx_pids  from (
    #          select 
    #          a.SHOP_CODE , a.merchant_name ,
    #          case when a.PAY_STATUS = 1 then a.payDates when  a.PAY_TYPE = 10002 then a.POST_DATE_STR end dt,
    #          a.sum_price, b.quantity ,a.oid , b.proname 
    #          FROM
    #          tb_porder a
    #          join tb_porder_Line b on a.oid = b.oid 
    #          WHERE
    #          1 = 1
    #          and a.HANDLE_STATUS not in (5, 7 ) 
    #          and ( a.PAY_STATUS = 1 or a.PAY_TYPE = 10002 )  
    #          and not EXISTS (select 'X' from bbf_shiny.tb_b_type_member d where a.mid = d.mid )  -- 排除掉  B类客户
    #          ) x
    #          where 1 = 1 
    #          and date(dt) >= '",pay_date()[1],"'
    #          and date(dt) <= '",pay_date()[2],"'
    #          group by shop_code ,merchant_name  )x 
    #          left join (
    #          select a.SHOP_CODE , count(distinct c.proname ) jy_Pids from tb_product_info a
    #          join tb_product_sku_info b on a.DID = b.DID
    #          join tb_product_spec s on b.sku_spec = s.id
    #          join tb_product_base_info c on s.base_info_id = c.id
    #          where 1 = 1 
    #          and alive not in (0 ,2 )
    #          and date(POST_DATE) <=  '",pay_date()[2],"'
    #          group by a.SHOP_CODE 
    #          ) y
    #          on x.shop_code = y.shop_code 
    #         ")
    
    
    sql <- paste0("  select x.* , y.jy_Pids from (
             select shop_code ,merchant_name,sum(sum_price) amount,sum(quantity) quantity ,count(distinct oid) oids ,count(distinct proname ) dx_pids  from (
                  select 
                  a.SHOP_CODE , a.merchant_name ,
                  case when a.PAY_STATUS = 1 then a.payDates when  a.PAY_TYPE = 10002 then a.POST_DATE_STR end dt,
                  a.sum_price, b.quantity ,a.oid , b.proname 
                  FROM
                  tb_porder a
                  join tb_porder_Line b on a.oid = b.oid 
                  WHERE
                  1 = 1
                  and ( 
                  ( pay_status = 1 
                  and date(payDates) >= '",pay_date()[1],"'
                  and date(payDates) <= '",pay_date()[2],"') 
                  or ( pay_type = 10002 
                  and date(post_date_str) >= '",pay_date()[1],"'
                  and date(post_date_str)  <= '",pay_date()[2],"'   )
                  ) 
                  and not EXISTS (select 'X' from bbf_shiny.tb_b_type_member d where a.mid = d.mid )  -- 排除掉  B类客户
    ) x
                  where 1 = 1 
                  group by shop_code ,merchant_name  )x 
                  left join (
                  select a.SHOP_CODE , count(distinct c.proname ) jy_Pids from tb_product_info a
                  join tb_product_sku_info b on a.DID = b.DID
                  join tb_product_spec s on b.sku_spec = s.id
                  join tb_product_base_info c on s.base_info_id = c.id
                  where 1 = 1 
                  and alive not in (0 ,2 )
                  and date(POST_DATE) <= '",pay_date()[2],"'
                  group by a.SHOP_CODE 
                  ) y
                  on x.shop_code = y.shop_code ")
    
    
    dd <- dbGetQuery0('ecommerce',sql)
    
    df <- dd %>%mutate(
      amount=round(amount),
      pid_dx_rat =  ifelse( dd[,7] == 0  , 0 ,  dd[,6]/dd[,7]  )
      ## 商家销售排名：商家ID/商家名称/销售额/销售量/成单数量/经营产品数/动销产品数/产品动销率
    ) %>% rename(`商家ID`=shop_code , `商家名称`=merchant_name,`销售额`=amount,`销售量`=quantity , `成单数量`=oids ,`经营产品数`= jy_Pids ,`动销产品数`= dx_pids,`产品动销率`=pid_dx_rat)
  }) 
  
  
  
  # export
  output$btn_export <- downloadHandler(paste('bbf-data-merchantRank-',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      #tmp <- df2()[input$tbl_rows_all, , drop = FALSE]
      write.csv(df(), file, fileEncoding='gbk',row.names=FALSE)
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
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        #autoWidth = TRUE,
        columnDefs = list(list(width = '200px', targets = c(0))),
        searching=FALSE,
        deferRender=FALSE,
        scrollY=200,
        scrollX=TRUE,
        scrollCollapse = TRUE,
        order = list(list(2, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 500,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    )%>%  # datatable end
      formatPercentage(c('产品动销率'), 2)
  })
  
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 商品通用名排名 
pronameProductUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('dt'), '付款时间', start = Sys.Date()-1, end = Sys.Date()-1)),
             column(2, downloadButton(ns('btn_export'), '导出商品通用名汇总表', class = 'btn_nowrap'))
    ),
    fluidRow(
      DT::dataTableOutput(ns('tbl'))
    )
  )
}
pronameProduct <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  df <- reactive({
    dbGetQuery0('ecommerce', paste0("
                                    SELECT
                                    a.proname,	c.ctitle0,	c.ctitle1,	c.ctitle2,
                                    count(distinct a.NAME) as name_counts,count(distinct a.pid) as pid_counts,COUNT(DISTINCT b.merchant_name) as merchant_counts,
                                    sum(a.quantity) as quantity,	round(sum(a.amount)) as amounts,	sum(a.commission) as commission,
                                    count(distinct b.mid) as mid_counts
                                    FROM
                                    tb_porder_line a
                                    INNER JOIN tb_porder b 
                                    ON a.oid = b.oid
                                    INNER JOIN tb_product_catalogbase c ON c.pid = a.pid
                                    WHERE 1=1
                                    and b.HANDLE_STATUS NOT IN (5, 7)
                                    AND (
                                    (
                                    b.PAY_STATUS = 1
                                    AND date(b.payDates) >= '",dt()[1],"'
                                    AND date(b.payDates) <= '",dt()[2],"'
                                    )
                                    OR (
                                      b.PAY_TYPE = 10002
                                    AND date(b.POST_DATE_STR) >= '",dt()[1],"'
                                    AND date(b.POST_DATE_STR) <= '",dt()[2],"'
                                    )
                                    )
                                    group by a.proname
                                    order by amounts desc
                                    "))
  })
  
  df2 <- reactive({
    x <- df() %>% 
      select(
        `通用名称` = proname,
        `一级分类` = ctitle0,
        `二级分类` = ctitle1,
        `三级分类` = ctitle2,
        `商品名数` = name_counts,        
        `pid数` = pid_counts,  
        `商家数` = merchant_counts, 
        `购买人数` = mid_counts,
        `销售额` = amounts,
        `销售量` = quantity,
        `分成总额` = commission
      )
  })
  
  # export
  output$btn_export <- downloadHandler(paste('bbf-proname-',Sys.Date(),'.csv',sep=''),content = function(file) {
    if(TRUE){
      tmp <- df2()[input$tbl_rows_all, , drop = FALSE]
      write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
    }
    
  })
  
  
  output$tbl <- DT::renderDataTable({
    datatable(
      df2(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'multiple',
      #extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      extensions = list(Scroller=list()),
      options = list(
        autoWidth = TRUE,
        columnDefs = list(list(width = '30px', targets = c(1))),
        searching=TRUE,
        deferRender=TRUE,
        scrollY=200,
        scrollX=TRUE,
        scrollCollapse = TRUE,
        order = list(list(8, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 50,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-200)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-200;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    )
  })
  }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 品类优化
proudctOptimizeUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, dateRangeInput(ns('pay_date'), '查询期付款时间',  start = Sys.Date()-1, end = Sys.Date()-1)),
      column(3, dateRangeInput(ns('pay_date1'), '对比期付款时间',  start = Sys.Date()-2, end = Sys.Date()-2)),
      #column(3, numericInput(ns("kpi1"), 'UV',value = 0 , step = 10000)),
      column(2, sliderInput(ns("sharerat"),label = "占有率", min = 0.0001, max = 0.5, value = c(0.0001, 0.2)  )),
      column(2, sliderInput(ns("riserat"),label = "增长率",min = -0.9999, max = 5, value = c(-0.001, 2) ))
      
    ),
    
    fluidRow(
      column(4,dataTableOutput(ns('tbl'))),
      column(8,plotlyOutput(ns('plot')))
    )
    
  )
}
proudctOptimize <- function(input, output, session) {
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  })
  #对比期付款日期值获取
  pay_date1 <- reactive({
    req(input$pay_date1)
    input$pay_date1
  })
  #增长率
  riserat <- reactive({
    req(input$riserat)
    input$riserat
  })
  
  #占有率
  sharerat <- reactive({
    req(input$sharerat)
    input$sharerat
  })
  
  #and date(dt) >= '",pay_date()[1],"'
  #and date(dt) <= '",pay_date()[2],"'
  # AND date(POST_DATE_STR) >= '",pay_date()[1]-30,"'
  # AND date(POST_DATE_STR) <= '",pay_date()[2]-30,"'
  
  
  
  df <- reactive({
    
    
    sql <-
      paste0("  select ctitle2, amount1 , amount2 from (
             select  x.ctitle2 , ifnull(x.amount ,0) amount1, ifnull(y.amount ,0) amount2 from (
             SELECT 
             c.ctitle2 ,
             count(distinct t.oid) oids ,
             count(distinct t.mid) mids ,
             sum( b.amount ) amount ,
             sum( b.quantity )quantity 
             FROM
             ecommerce.tb_porder t
             JOIN ecommerce.tb_porder_line b ON t.oid = b.oid
             join bbf_shiny.my_product c on b.pid = c.pid 
             WHERE
             1 = 1
             and HANDLE_STATUS NOT IN (5, 7)
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
             group by c.ctitle2 ) x
             
             left join 
             (
             SELECT 
             c.ctitle2 ,
             count(distinct t.oid) oids ,
             count(distinct t.mid) mids ,
             sum( b.amount ) amount ,
             sum( b.quantity )quantity 
             FROM
             ecommerce.tb_porder t
             JOIN ecommerce.tb_porder_line b ON t.oid = b.oid
             join bbf_shiny.my_product c on b.pid = c.pid 
             WHERE
             1 = 1
             and HANDLE_STATUS NOT IN (5, 7)
             AND (
             (
             PAY_STATUS = 1
             AND date(payDates) >=  '",pay_date1()[1],"'
             AND date(payDates) <=  '",pay_date1()[2],"'
             )
             OR (
              PAY_TYPE = 10002
             AND date(POST_DATE_STR) >=  '",pay_date1()[1],"'
             AND date(POST_DATE_STR) <= '",pay_date1()[2],"'
             )
             )
             group by  c.ctitle2 ) y
             on  x.ctitle2 = y.ctitle2
      ) z  
             order by amount1  desc 
             limit 1,100
             ")
    
    dd <- dbGetQuery0('ecommerce',sql)
    
    df <- dd %>% 
      mutate(
        sum_amount = round(sum(amount1)),
        amount_rat=ifelse(sum_amount==0 ,0 ,round(amount1/sum_amount,4)),
        rise_rat=ifelse(amount2==0,0,round((amount1-amount2)/amount2,4))
      ) %>% select(-sum_amount,-amount2) 
    
    
  })
  
  
  
  output$tbl <- renderDataTable({ 
    
    datatable(
      df()%>%rename(`三级类目`=ctitle2,`销售额`=amount1,`销售额占比`=amount_rat,`增长率`=rise_rat),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        #autoWidth = TRUE,
        columnDefs = list(list(width = '200px', targets = c(0))),
        searching=FALSE,
        deferRender=FALSE,
        scrollY=200,
        scrollX=TRUE,
        scrollCollapse = TRUE,
        order = list(list(2, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 500,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    )
  })
  
  
  output$plot <- renderPlotly({
    #ePoints(df(), ~amount_rat, ~rise_rat, series = ~ctitle2)
    #
    # highchart() %>%
    #   hc_title(text = "Scatter chart with size and color") %>%
    #   hc_add_series_scatter(df()$amount_rat, df()$rise_rat)
    
    
    df1 <- subset(df() ,df()$amount_rat<= quantile(df()$amount_rat,  probs = c(95)/100) &  df()$rise_rat<=quantile(df()$rise_rat,  probs = c(95)/100))
    
    
    
    df1 <- subset(df1 , df1$rise_rat >= riserat()[1] &  df1$rise_rat<= riserat()[2] )
    
    df1 <- subset(df1 , df1$amount_rat >= sharerat()[1] &  df1$amount_rat<= sharerat()[2] )
    
    df1 <- df1 %>% rename(`占有率`=amount_rat,`增长率`=rise_rat)
    
    ggplot(df1, aes(x = 占有率, y = 增长率)) +
      # 散点图函数
      geom_point() +
      # 文本函数：aes参数中：y将原有纵轴值向上偏移，label设置绑定文本
      # 将y轴偏移的目的是为了让文本展示在样本点上方而不是中间
      #geom_text(aes( label = ctitle2   ))+
      geom_vline(size = 1, colour = "green", xintercept = mean(df1$占有率)) +
      geom_hline(size = 1, colour = "blue", yintercept = mean(df1$增长率)) +
      geom_text(aes( label = paste(ctitle2,'\n',"占有率:",占有率,",","增长率:",增长率)   ))
  })
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 经营商家监测
merchantOperateUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('dt'), '商家更新时间', start = Sys.Date()-months(6), end = Sys.Date()-1))
             #column(2, downloadButton(ns('btn_export'), '导出商家表', class = 'btn_nowrap'))
    ),
    fluidRow(
      box(column(12,dygraphOutput(ns('trend1')))),
      box(column(12,dygraphOutput(ns('trend2'))))
    ),
    fluidRow(
      box(
      title = '正常合作商家', status = 'primary', solidHeader = T, width = 12,
      fluidRow(
        class='toolbar',
        column(6,helpText('  1.正常合作商家是截止到昨天的商家状态，排除了商家状态为禁用、锁定或者物流全部关闭的商家',br(),
                          '  2.商家级别：2=B 3=A')
        ),
        column(6,helpText('',br())
        )
      ),

      br(),
      fluidRow(DT::dataTableOutput(ns('tbl')))
      )
    ),
    fluidRow(
      box(
        title = '禁用、锁定或者货到付款关闭商家', status = 'primary', solidHeader = T, width = 12,
        fluidRow(
          class='toolbar',
          column(6,helpText('  1.截止到昨天，商家状态为禁用、锁定或者物流全部关闭的商家',br(),
                            '  2.是否锁定:1表示锁定。',br(),
                            '  3.禁用：0，-1，-2均表示禁用',br(),
                            '  4.物流是否正常：0表示全部关闭')
          ),
          column(6,helpText('',br())
          )
        ),
        br(),
        fluidRow(DT::dataTableOutput(ns('tbl2')))
    )
    )
  )
}
merchantOperate <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  df1 <- reactive({
    sql<-paste0("
                select date(updatetime) as days,count(DISTINCT merchant_code) as merchants_lock
                from tb_merchant 
                where 1=1
                and is_lock=1
                and is_open<>1
                and date(updatetime)>='",dt()[1],"' 
                and date(updatetime)<='",dt()[2],"'
                GROUP BY date(updatetime) 
                ORDER BY date(updatetime) desc
                ")
    dbGetQuery0('ecommerce', sql)%>%
      mutate(dt1 = as.Date(days))
  })
  # 做曲线图:锁定或者禁用商家数
  output$trend1 <- renderDygraph({
    x <- df1() %>% select(dt1,merchants_lock)
    x <- as.xts(x[, 2], x[, 1])
    dygraph(x,"锁定或者禁用商家数趋势图")%>%
      dyRangeSelector(height = 20, strokeColor = "") 
  })
  
  
  df2 <- reactive({
    sql<-paste0("
                select dt,count(DISTINCT shopcode) as shops from (
                SELECT shopcode,date(modifydt) as dt,sum(disabled) as shippings
                from tb_shipping_template
                where 1=1
                GROUP BY shopcode
                ) x
                where shippings=0
                and date(dt)>='",dt()[1],"' 
                and date(dt)<='",dt()[2],"'
                GROUP BY dt
                ORDER BY date(dt) desc
                ")
    dbGetQuery0('ecommerce', sql)%>%
      mutate(dt1 = as.Date(dt))
  })
  # 做曲线图:货到付款全关闭商家数
  output$trend2 <- renderDygraph({
    x <- df2() %>% select(dt1,shops)
    x <- as.xts(x[, 2], x[, 1])
    dygraph(x,"货到付款全关闭商家数趋势图")%>%
      dyRangeSelector(height = 20, strokeColor = "") 
  })
  
  # 正常合作商家数
  df3 <- reactive({
    sql<-paste0("
    select merchant_code,MERCHANT_NAME, ALLNAME,pact_start,pact_end ,date(updatetime) as updatetime ,privilege 
    from tb_merchant a
    where 1=1
    and is_lock=0
    and is_open=1
    and  merchant_code in (
      select shopcode from tb_shipping_template
      GROUP BY shopcode 
      HAVING sum(disabled)>0
    )
    ORDER BY PRIVILEGE desc
    ")
    dbGetQuery0('ecommerce', sql)
  })
    output$tbl <- DT::renderDataTable({
    datatable(
    df3()%>%rename(`商家编号`=merchant_code,`商家名`=MERCHANT_NAME,`公司`=ALLNAME,`合同开始`=pact_start,`合同结束`=pact_end,
                   `更新时间`=updatetime,`商家级别`=privilege),
    escape = FALSE,
    rownames = FALSE,
    selection = 'multiple',
    #extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
    extensions = list(Scroller=list()),
    options = list(
    autoWidth = TRUE,
    #columnDefs = list(list(width = '50px', targets = c(1))),
    searching=TRUE,
    deferRender=TRUE,
    scrollY=22,
    scrollX=TRUE,
    scrollCollapse = TRUE,
    #order = list(list(11, 'desc')),
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
    pageLength = 22,
    lengthChange = FALSE,
    initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-200)+'px !important');}"),
    fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-200;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
    )
    )
    })
    # 正常合作商家数 一个汇总值
    
    # 禁用、锁定或者货到付款关闭商家数
    df4 <- reactive({
    sql<-paste0("
    SELECT a.merchant_code,b.MERCHANT_NAME, b.ALLNAME,a.is_lock, a.is_open,a.shippings,a.modifydt,date(a.updatetime) as updatetime ,b.privilege,
b.pact_start,b.pact_end 
                from(
                select merchant_code,is_lock, is_open,updatetime,
                b.modifydt,b.shippings
                from tb_merchant a
                left join (
                select modifydt, shopcode,shippings
                from (
                SELECT shopcode,modifydt ,sum(disabled) as shippings
                from tb_shipping_template
                where 1=1
                GROUP BY shopcode
                ) x
                where shippings=0
                ) b
                on a.merchant_code=b.shopcode
                where 1=1
                and is_lock=1
                and is_open<>1
                UNION
                (
                select shopcode as merchant_code,is_lock, is_open,updatetime,
                modifydt,shippings
                from (
                SELECT shopcode,modifydt ,sum(disabled) as shippings
                from tb_shipping_template
                where 1=1
                GROUP BY shopcode
                ) x
                LEFT JOIN (
                select merchant_code,is_lock, is_open,updatetime
                from tb_merchant a
                where 1=1
                and is_lock=1
                and is_open<>1
                ) y
                on x.shopcode=y.merchant_code
                where x.shippings=0
                ORDER BY x.modifydt desc
                )
                ) a
                JOIN tb_merchant b
                on a.merchant_code=b.merchant_code
                order by a.updatetime desc
    ")
  dbGetQuery0('ecommerce', sql)
  })
  output$tbl2 <- DT::renderDataTable({
    datatable(
      df4()%>%rename(`商家编号`=merchant_code,`商家名`=MERCHANT_NAME,`公司`=ALLNAME,`是否锁定`=is_lock,`禁用`=is_open,`物流是否正常`=shippings,`关闭物流方式时间`=modifydt,
                     `更新时间`=updatetime,`商家级别`=privilege,`合同开始`=pact_start,`合同结束`=pact_end),
      escape = FALSE,
      rownames = FALSE,
      selection = 'multiple',
      #extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      extensions = list(Scroller=list()),
      options = list(
        autoWidth = TRUE,
       # columnDefs = list(list(width = '50px', targets = c(1))),
        searching=TRUE,
        deferRender=TRUE,
        scrollY=22,
        scrollX=TRUE,
        scrollCollapse = TRUE,
        #order = list(list(11, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 22,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-200)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-200;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    )
  })
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 经营商品监测
# 增加帮助、中文字体
productMonitorUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(class='toolbar',
             column(6,helpText('1.此表为截止到昨日的只有一个商家经营的批准文号',br(),
                               '2.成单时间仅表示对应商品在选择的时间段成单金额，默认为近30日')
             ),
             column(6,helpText(''))
    ),
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('dt'), '成单时间', start = Sys.Date()-30, end = Sys.Date()-1)),
             column(2, downloadButton(ns('btn_export'), '导出商品汇总表', class = 'btn_nowrap'))
    ),
    fluidRow(
      DT::dataTableOutput(ns('tbl'))
    )
  )
}
productMonitor <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  df <- reactive({
    dbGetQuery0('ecommerce', paste0(" 
                                 select b.pid,b.proname,b.name,replace(replace(b.PZWH,'（','') ,'）','') PZWH,b.merchant_name,
case when c.is_lock=0 then '否' when c.is_lock=1 then '是' end is_lock,
                                    case when c.is_open=1 then '否' when c.is_open<>1 then '是' end is_open,
                                    c.privilege,c.disabled,d.amount,d.commission,
                                    date(b.lastupdatetimestamp) as  lastupdatetimestamp
                                    from (
                                    select pid,proname,name,PZWH,merchant_name,shop_code,
                                    date(lastupdatetimestamp) as  lastupdatetimestamp
                                    from 	bbf_shiny.my_product 
                                    where 1=1
                                    and verify = 1  -- 审核通过
                                    and alive not in (0,2)
                                    GROUP BY PZWH
                                    having count(DISTINCT pid)=1
                                    )b
                                    left join (
                                    select d.merchant_code,d.is_lock, d.is_open,d.privilege,f.disabled
                                    from tb_merchant d
                                    left join  (
                                    select shopcode,
                                    case when disabled>0 then '否'
                                    when disabled=0 then '是'
                                    end disabled
                                    from (SELECT shopcode,sum(disabled) as disabled from tb_shipping_template GROUP BY shopcode) x
                                    ) f
                                    on d.merchant_code=f.shopcode
                                      where d.is_open = 1       -- 剔除禁售
                                    ) c
                                    on b.shop_code=c.merchant_code
                                    left join (
                                    SELECT b.pid, sum(b.amount) as amount, sum(b.commission) as commission
                                    FROM
                                    tb_porder t
                                    JOIN tb_porder_line b ON t.oid = b.oid
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
                                    group by b.pid
                                    ) d
                                    ON b.pid=d.pid
                                    ")) %>% 
      select(
              
             `批准文号` = PZWH,
              `商品pid`=pid,
              `商家名` = merchant_name,
              `是否锁定` = is_lock,
              `是否禁用` = is_open,
              `最后更新时间`=lastupdatetimestamp,
              `商家等级` = privilege,
              `快递方式是否全关闭` = disabled,
              `近30天成单金额` = amount,
              `商品名` = name,
               `商品通用名`=proname,
              `分成总` = commission
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
        order = list(list(8, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 21,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-200)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-200;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    )
  })
  }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

