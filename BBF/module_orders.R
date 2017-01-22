# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 转单报表
transferReportUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(class='toolbar',
             column(1, selectInput(ns('dt_type'), '日期类型', choices = c('支付'='post_date', '转单'='create_date', '承接'='accept_date', '审核'='check_date'))),
             column(2, dateRangeInput(ns('dt'), '日期', language='zh_CN', start = Sys.Date()-months(1), end = Sys.Date())),
             column(1, numericInput(ns('create_hour'), '转单>=X天', value = -1)),
             column(1, numericInput(ns('check_hour'), '审核>=X天', value = -1)),
             column(1, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
    ),
    #sbr(),br(),
    fluidRow(
      DT::dataTableOutput(ns('tbl'))
    )
  )
}
transferReport <- function(input, output, session) {
  dt_type <- reactive({
    req(input$dt_type)
    input$dt_type
  })
  
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  df <- reactive({
    # 计算转单时间改下单时间为支付时间，避免商家使用客户支付晚了作为借口
    #20160804 添加interval_day1,interval_day2两个字段
    sql <- paste0("
                  SELECT
                  cast(a.orderno as char) as orderno,
                  case when length(paydates)>5 then paydates else b.post_date_str end as post_date,
                  a.create_date,
                  case when length(paydates)>5 then DATEDIFF( a.create_date , paydates) 
                  else DATEDIFF(a.create_date , b.post_date_str)
                  end as interval_day1,
                  a.accept_date,
                  a.check_date,
                  DATEDIFF(a.check_date , a.accept_date)  interval_day2,
                  a.status,
                  a.old_merchant,
                  a.new_merchant,
                  a.fail_reason,
                  a.check_status,
                  a.check_user,
                  b.payname,
                  b.actual_pay,
                  b.name,
                  b.mobile,
                  b.account
                  FROM
                  tb_porder_transfer a
                  INNER JOIN tb_porder b ON a.orderno = b.orderno
                  ")
    tmp <- dbGetQuery0('ecommerce', sql)
    tmp <- tmp %>%
      # 日期跟NULL做difftime操作会产生NULL
      mutate(
        post_date = as.POSIXct(post_date),
        payname = ifelse(payname %in% c('货到付款'), '是', '否'),
        `转单用时` = as.integer(ifelse(is.na(create_date) | is.na(post_date), -1, as.numeric(difftime(create_date, post_date, units = 'day')))),
        #`转单用时` = as.numeric(interval_day1),
        `审核用时` = as.integer(ifelse(is.na(check_date) | is.na(accept_date), -1, as.numeric(difftime(check_date, accept_date, units = 'day')))),
        #`审核用时` = as.numeric(interval_day2),
        post_date = as.Date(post_date, '%Y-%m-%d'),
        create_date = as.Date(create_date, '%Y-%m-%d'),
        accept_date = as.Date(accept_date, '%Y-%m-%d'),
        check_date = as.Date(check_date, '%Y-%m-%d'),
        `收货人信息` = paste(name, mobile, sep='/'),
        status = c('审核中', '未承接', '转单成功','转单失败')[status+1]  # 0平台审核中,1未承接，2转单成功,3转单失败
      ) %>%
      group_by(orderno) %>% 
      arrange(orderno, create_date) %>% 
      mutate(`第几次转单`=seq_along(orderno)) %>% 
      # 以下过滤会导致部分记录丢失：平台取消转单的记录是没有承接和审核时间的
      filter_(
        paste0('`转单用时` >= ', input$create_hour),
        paste0('`审核用时` >= ', input$check_hour),
        paste0(dt_type(), ' >= "', dt()[1], '"'),
        paste0(dt_type(), ' <= "', dt()[2], '"')
      ) %>% 
      select(订单编号=orderno, 到付=payname,`收货人信息`, `第几次转单`, `审核人`=check_user, `转单用时(天)`=`转单用时`, `审核用时(天)`=`审核用时`,支付=post_date, 转单=create_date, 承接=accept_date, 审核=check_date, 原商家=old_merchant, 承接商家=new_merchant, 订单金额=actual_pay, 审核状态=status, 原因=fail_reason)
  })
  
  output$btn_export <- downloadHandler(paste('bbf-data-transfer-',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      tmp <- df()[input$tbl_rows_all, , drop = FALSE] %>% ungroup() %>% mutate(`订单编号`=paste0("'", as.character(`订单编号`)))
      write.csv(tmp, file, fileEncoding='gbk', row.names=FALSE)
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
      #filter = 'top',
      #extensions = c('FixedColumns', 'Scroller'),
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        autoWidth = TRUE,
        columnDefs = list(
          list(width = '120px', targets = c(0)),
          list(targets = c(2,4,5,6,7,8,9,12), searchable = FALSE)
        ),
        searching=TRUE,
        deferRender=TRUE,
        scrollY=200,
        scrollX='100%',
        scrollCollapse = TRUE,
        order = list(list(0, 'asc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 200,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )  # options end
    ) %>%  # datatable end
      formatRound(c('转单用时(天)', '审核用时(天)'), 0)
  })
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 咨询记录
customerUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      class='toolbar',
      column(2, dateRangeInput(ns('dt'), '更新时间', start = Sys.Date()-days(15), end=Sys.Date())),
      column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
    ),
    fluidRow(
      column(12, DT::dataTableOutput(ns('tbl')))
    )
  )
}
customer <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  
  
  ##update by gonghg 20160615
  # 会员中心反馈导出数据错位， 因为以下字段存在换行等特殊字符
  # replace(replace(replace(a.customerNote, char(13), ''),    char(10)  ,'') , char(9) ,'') customerNote,
  # replace(replace(replace(a.note, char(13), ''),    char(10)  ,'') , char(9) ,'') note,
  # replace(replace(replace(a.solution, char(13), ''),    char(10)  ,'') , char(9) ,'') solution,
  df <- reactive({
    dbGetQuery0('ecommerce', paste0("select a.id, b.id as record_id, cast(a.orderno as char) as orderno, d.actual_pay, a.createtime, a.merchantname, a.servicetype,
                                    
                                    replace(replace(replace(a.customerNote, char(13), ''),    char(10)  ,'') , char(9) ,'') customerNote,
                                    replace(replace(replace(a.note, char(13), ''),    char(10)  ,'') , char(9) ,'') note,
                                    replace(replace(replace(a.solution, char(13), ''),    char(10)  ,'') , char(9) ,'') solution,
                                    a.updatetime,b.status,b.kefu from tb_customer a left join tb_shouhou_record b on a.id = b.customerid and substring(a.updatetime,1,10)=substring(b.createtime,1,10) left join tb_porder d on a.orderno=d.orderno where substring(a.updatetime,1,10)>='", dt()[1], "' and substring(a.updatetime,1,10)<='", dt()[2], "'")) %>%
      arrange(id, desc(record_id)) %>%
      group_by(id) %>%
      top_n(1, record_id) %>%
      mutate(
        createtime = as.Date(createtime),
        updatetime = as.Date(updatetime),
        #status = c('已完成','处理中','未处理')[as.integer(status)+1],
        servicetype = c('顾客自身原因','发错货/发漏货','物流配送','商品信息错误','药品质量','其他原因','超时发货','商家客服服务','系统原因','八百方客服服务','商家客服服务(擅自改物流)','商家客服服务(虚假发货)','查账(已到账)','撤销投诉','查账(未到账)')[as.integer(servicetype)],
        customerNote = stringr::str_replace_all(customerNote, pattern = '\n', '<br />'),
        note = stringr::str_replace_all(note, pattern = '\n', '<br />'),
        solution = stringr::str_replace_all(solution, pattern = '\n', '<br />')
      ) %>%
      select(
        `序号` = id,
        `商家名称` = merchantname,
        `订单编号` = orderno,
        `订单金额` = actual_pay,
        `咨询日期` = createtime,
        `更新日期` = updatetime,
        `处理状态` = status,
        `处理客服` = kefu,
        `售后问题分类` = servicetype,
        `投诉内容` = customerNote,
        `客服备注` = note,
        `处理流程` = solution
      )
  })
  
  # export
  output$btn_export <- downloadHandler(paste('bbf-data-customer-shouhou-',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      tmp <- df()[input$tbl_rows_all, , drop = FALSE] %>%
        mutate(
          `订单编号` = paste0(`订单编号`, '-'),
          `投诉内容` = stringr::str_replace_all(`投诉内容`, pattern = '<br />', '\n'),
          `客服备注` = stringr::str_replace_all(`客服备注`, pattern = '<br />', '\n'),
          `处理流程` = stringr::str_replace_all(`处理流程`, pattern = '<br />', '\n')
        )
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
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        autoWidth = TRUE,
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




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 店铺退款分析
shopDrawbackUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      class='toolbar',
      column(2, dateRangeInput(ns('dt'), '付款时间', start = Sys.Date()-8, end = Sys.Date()-1)),
      column(1, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
    ),
    fluidRow(
      column(8, DT::dataTableOutput(ns('tbl'))),
      column(4, DT::dataTableOutput(ns('tbl2')))
    )
  )
}
shopDrawback <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  df0 <- reactive({
    dbGetQuery0('ecommerce', paste0("select * from (select a.merchant_name,a.orderno,b.reason,case when a.pay_status = 1 then date_format(a.paydates, '%Y-%m-%d') when a.pay_type = 10002 and a.handle_status not in (5) then date_format(a.post_date_str, '%Y-%m-%d') end dt, a.actual_pay, IFNULL(b.amount,0) as amount, case when b.amount is null then 0  else 1 end cnt_db from tb_porder a left join (select * from tb_porder_drawback d where d.dualstatus not in (3, 4)) b on a.oid=b.oid where a.pay_status = 1 or (a.pay_type = 10002 and a.handle_status not in (5))) c where c.dt>='",dt()[1],"' and c.dt<='",dt()[2],"'"))
  })
  df <- reactive({
    df0() %>% 
      group_by(merchant_name) %>% 
      summarise(
        actual_pay = sum(actual_pay, na.rm=TRUE),
        amount = sum(amount, na.rm=TRUE),
        pct = amount / actual_pay,
        cnt = n_distinct(orderno),
        cnt_db = sum(cnt_db, na.rm=TRUE),
        pct_db = cnt_db / cnt
      ) %>% 
      arrange(desc(cnt)) %>% 
      select(`商家`=merchant_name, `成交金额`=actual_pay, `退款金额`=amount, `金额退款率`=pct, `成交订单数`=cnt, `退款订单数`=cnt_db, `订单数退款率` = pct_db)
  })
  df2 <- reactive({
    merchant <- df()$`商家`[ifelse(is.null(input$tbl_rows_selected), 1, input$tbl_rows_selected)]
    df0() %>% 
      filter(merchant_name==merchant, amount>0) %>% 
      group_by(reason) %>% 
      summarise(cnt = n()) %>% 
      select(`退款原因`=reason, `订单数`=cnt)
  })
  output$tbl <- DT::renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = list(mode = 'single', selected = c(1)),
      extensions = list(Scroller=list()),
      options = list(
        autoWidth = TRUE,
        searching=TRUE,
        deferRender=TRUE,
        scrollY=200,
        scrollCollapse = TRUE,
        order = list(list(4, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 200,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-220)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-220;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    ) %>% formatPercentage(c('金额退款率', '订单数退款率'), 2) %>% formatRound(c('成交金额', '退款金额'), 2)
  })
  
  output$tbl2 <- DT::renderDataTable({
    datatable(
      df2(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      extensions = list(Scroller=list()),
      options = list(
        autoWidth = TRUE,
        searching = TRUE,
        deferRender = TRUE,
        scrollY=200,
        scrollCollapse = TRUE,
        order = list(list(1, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 200,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-220)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-220;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    )
  })
  
  output$btn_export <- downloadHandler(paste('bbf-data-shop-drawback-',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      tmp <- df()[input$tbl_rows_all, , drop = FALSE] %>% mutate(`金额退款率`=paste0(round(`金额退款率`*100,2), '%'), `订单数退款率`=paste0(round(`订单数退款率`*100,2), '%'))
      write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names = FALSE, col.names = FALSE)
    }
  })
  
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 店铺退款分析
shopDrawbackUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      class='toolbar',
      column(2, dateRangeInput(ns('dt'), '付款时间', start = Sys.Date()-8, end = Sys.Date()-1)),
      column(1, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
    ),
    fluidRow(
      column(8, DT::dataTableOutput(ns('tbl'))),
      column(4, DT::dataTableOutput(ns('tbl2')))
    )
  )
}
shopDrawback <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  df0 <- reactive({
    dbGetQuery0('ecommerce', paste0("select * from (select a.merchant_name,a.orderno,b.reason,case when a.pay_status = 1 then date_format(a.paydates, '%Y-%m-%d') when a.pay_type = 10002 and a.handle_status not in (5) then date_format(a.post_date_str, '%Y-%m-%d') end dt, a.actual_pay, IFNULL(b.amount,0) as amount, case when b.amount is null then 0  else 1 end cnt_db from tb_porder a left join (select * from tb_porder_drawback d where d.dualstatus not in (3, 4)) b on a.oid=b.oid where a.pay_status = 1 or (a.pay_type = 10002 and a.handle_status not in (5))) c where c.dt>='",dt()[1],"' and c.dt<='",dt()[2],"'"))
  })
  df <- reactive({
    df0() %>% 
      group_by(merchant_name) %>% 
      summarise(
        actual_pay = sum(actual_pay, na.rm=TRUE),
        amount = sum(amount, na.rm=TRUE),
        pct = amount / actual_pay,
        cnt = n_distinct(orderno),
        cnt_db = sum(cnt_db, na.rm=TRUE),
        pct_db = cnt_db / cnt
      ) %>% 
      arrange(desc(cnt)) %>% 
      select(`商家`=merchant_name, `成交金额`=actual_pay, `退款金额`=amount, `金额退款率`=pct, `成交订单数`=cnt, `退款订单数`=cnt_db, `订单数退款率` = pct_db)
  })
  df2 <- reactive({
    merchant <- df()$`商家`[ifelse(is.null(input$tbl_rows_selected), 1, input$tbl_rows_selected)]
    df0() %>% 
      filter(merchant_name==merchant, amount>0) %>% 
      group_by(reason) %>% 
      summarise(cnt = n()) %>% 
      select(`退款原因`=reason, `订单数`=cnt)
  })
  output$tbl <- DT::renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = list(mode = 'single', selected = c(1)),
      extensions = list(Scroller=list()),
      options = list(
        autoWidth = TRUE,
        searching=TRUE,
        deferRender=TRUE,
        scrollY=200,
        scrollCollapse = TRUE,
        order = list(list(4, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 200,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-220)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-220;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    ) %>% formatPercentage(c('金额退款率', '订单数退款率'), 2) %>% formatRound(c('成交金额', '退款金额'), 2)
  })
  
  output$tbl2 <- DT::renderDataTable({
    datatable(
      df2(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      extensions = list(Scroller=list()),
      options = list(
        autoWidth = TRUE,
        searching = TRUE,
        deferRender = TRUE,
        scrollY=200,
        scrollCollapse = TRUE,
        order = list(list(1, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 200,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-220)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-220;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    )
  })
  
  output$btn_export <- downloadHandler(paste('bbf-data-shop-drawback-',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      tmp <- df()[input$tbl_rows_all, , drop = FALSE] %>% mutate(`金额退款率`=paste0(round(`金额退款率`*100,2), '%'), `订单数退款率`=paste0(round(`订单数退款率`*100,2), '%'))
      write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names = FALSE, col.names = FALSE)
    }
  })
  
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 商家缺货率
merchantPenaltyUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      class='toolbar',
      column(3, dateRangeInput(ns('dt'), '日期', start = Sys.Date()-months(3), end=Sys.Date())),
      column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
    ),
    fluidRow(
      column(3, textInput(ns('txt'), '商家名(Excel直接复制粘贴)', value = '安利亚药房 安徽国胜大药房')),
      column(9, DT::dataTableOutput(ns('tbl')))
    )
  )
}
merchantPenalty <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  txt <- reactive({
    req(input$txt)
    stringr::str_replace_all(input$txt, " ", "','")
  })
  
  ##改成左连接, update by gonghg 20161110    
  df <- reactive({
    dbGetQuery0("ecommerce", paste0("SELECT a.merchant_name, c.post_date, c.out_stock, CASE WHEN b.bcid = 1 THEN b.rate END AS rate_1,  avg( CASE WHEN b.bcid = 25 THEN b.rate END ) AS rate_2,  max(d.amount) as amount FROM tb_merchant a left JOIN tb_merchant_catalog_rate b ON a.merchant_code = b.shop_code left JOIN (select * from tb_merchant_rating cc where substring(cc.post_date, 1, 7) = '",substr(Sys.Date(),1,7),"') c ON c.shop_code = a.merchant_code left JOIN (SELECT dd.SHOPCODE, sum(dd.amount) as AMOUNT FROM tb_porder_drawback dd WHERE SUBSTRING(dd.DUALDATES,1,7)='2016-04' AND dd.reason = '卖家缺货' GROUP BY SHOPCODE) d ON d.shopcode = a.merchant_code  WHERE a.merchant_name IN ('",txt(),"') GROUP BY a.merchant_name")) %>% 
      mutate(post_date=as.Date(post_date)) %>% 
      select(
        `商家` = merchant_name,
        `日期` = post_date,
        `缺货率` = out_stock,
        `药品` = rate_1,
        `非药` = rate_2,
        `金额` = amount
      )
  })
  # export
  output$btn_export <- downloadHandler(paste('bbf-data-merchant_penalty-',Sys.Date(),'.csv',sep=''), content = function(file) {
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
      extensions = list(Scroller=list()),
      options = list(
        autoWidth = TRUE,
        searching=TRUE,
        deferRender=TRUE,
        scrollY=200,
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
}

 

#------------------------两天未发货跟踪-------------------
sendProDelayTwoDaysUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('post_date'), '下单时间',  start = Sys.Date()-days(15), end = Sys.Date()-1))
    ),
    fluidRow(
      dataTableOutput(ns('tbl'))
    )
  )
}
sendProDelayTwoDays <- function(input, output, session) {
  #下单时间获取
  post_date <- reactive({
    req(input$post_date)
    input$post_date
  }) 
  
  
  
  df <- reactive({
    sql <-
      paste0(" select orderno , POST_DATE_STR ,case 
 when isBack = 0 then '前台'
   when isBack = 1 then '后台'
   when isBack = 2 then '在线订单'
   when isBack = 3 then '移动订单'
   when isBack = 4 then '店讯通订单'
   when isBack = 4 then '增值订单'
   end isback,a.merchant_name ,a.shop_code,b.name ,b.proName,b.AMOUNT,b.QUANTITY,
b.SHOP_PRICE,a.adminAccount , b.proPzwh  ,
case when a.PAY_STATUS= 1 or a.PAY_TYPE = 10002  then '已付款' else '未付款' end pay_status ,
a.DEL_TYPE_COST ,
a.shopCouponAmount,
a.couponAmount ,
a.score_to_cost,
ifnull(a.CUT_AMOUNT,0) CUT_AMOUNT
from tb_porder a
   left join tb_porder_line b on a.oid = b.oid 
where 1 = 1 
and a.handle_status  in ( 0, 1 )
and a.shop_code not in (801)
and date(a.POST_DATE_STR) between '",post_date()[1],"' and  '",post_date()[2],"'
AND DATEDIFF(now(),post_date_str) > 2")
    dd <- dbGetQuery0('ecommerce',sql)
    
    df <- dd %>% rename(`订单编号`=orderno ,
                        `下单时间`=POST_DATE_STR,
                        `订单来源`=isback,
                        `商家名称`=merchant_name ,
                        `商家编码`=shop_code ,
                        `商品金额`= AMOUNT,
                        `商品数量`=QUANTITY,
                        `商家单价`=SHOP_PRICE,
                        `后台下单人`=adminAccount,
                        `商品名称`=name,
                        `通用名`=proName,
                        `批准文号`=proPzwh ,
                        `支付状态`=pay_status,
                        `运费`=DEL_TYPE_COST,
                        `优惠券`=shopCouponAmount,
                        `红包`=couponAmount,
                        `积分抵扣`=score_to_cost,
                        `满就减金额`=CUT_AMOUNT
                        )
  }) 
  
  
  
  
  output$tbl <- renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'single',
       options = list(
        autoWidth = TRUE,
        columnDefs = list(list(width = '40px', targets = c(5,6,7))),
        searching=TRUE,
        deferRender=TRUE,
        scrollY=200,
        scrollX=TRUE,
        scrollCollapse = TRUE,
        order = list(list(0, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 200,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
       )
    ) 
  })
  
}







#------------------------未付款跟踪-------------------
nonPaymentUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('post_date'), '下单时间',  start = Sys.Date()-days(15), end = Sys.Date()-1))
    ),
    fluidRow(
      dataTableOutput(ns('tbl'))
    )
  )
}
nonPayment <- function(input, output, session) {
  #下单时间获取
  post_date <- reactive({
    req(input$post_date)
    input$post_date
  }) 
  
  
  
  df <- reactive({
    sql <-
      paste0(" select orderno , POST_DATE_STR ,case 
 when isBack = 0 then '前台'
             when isBack = 1 then '后台'
             when isBack = 2 then '在线订单'
             when isBack = 3 then '移动订单'
             when isBack = 4 then '店讯通订单'
             when isBack = 4 then '增值订单'
             end isback,a.merchant_name ,a.shop_code,b.name ,b.proName,b.AMOUNT,b.QUANTITY,
             b.SHOP_PRICE,a.adminAccount , b.proPzwh  ,
             case when PAYname != '货到付款' then '在线支付' else '货到付款' end pay_type ,
             CASE
             WHEN HANDLE_STATUS = 0 THEN
             '待处理'
             WHEN HANDLE_STATUS = 1 THEN
             '正在配货'
             WHEN HANDLE_STATUS = 4 THEN
             '交易成功'
             WHEN HANDLE_STATUS = 5 THEN
             '交易取消'
             WHEN HANDLE_STATUS = 6 THEN
             '已收货'
             WHEN HANDLE_STATUS = 7 THEN
             '交易关闭'
             WHEN HANDLE_STATUS = 97 THEN
             '已退货'
             WHEN HANDLE_STATUS = 99 THEN
             '已发货）'
             END HANDLE_STATUS,
             a.DEL_TYPE_COST ,
             a.shopCouponAmount,
             a.couponAmount ,
             a.score_to_cost,
             ifnull(a.CUT_AMOUNT,0) CUT_AMOUNT
             from tb_porder a
             left join tb_porder_line b on a.oid = b.oid 
             where 1 = 1 
             and ( ( a.PAY_STATUS != 1  and PAYname != '货到付款' )  -- 在线支付
             or (a.PAY_TYPE = 10002 and  a.handle_status  in ( 5, 7 )) 
             )
             and a.shop_code not in (801)
             and date(a.POST_DATE_STR) between '",post_date()[1],"' and  '",post_date()[2],"'
             #and date(a.POST_DATE_STR)  between '2017-01-01' and '2017-01-10' 
             ")
    dd <- dbGetQuery0('ecommerce',sql)
    
    df <- dd %>% rename(`订单编号`=orderno ,
                        `下单时间`=POST_DATE_STR,
                        `订单来源`=isback,
                        `订单状态`=HANDLE_STATUS,
                        `商家名称`=merchant_name ,
                        `商家编码`=shop_code ,
                        `商品金额`= AMOUNT,
                        `商品数量`=QUANTITY,
                        `商家单价`=SHOP_PRICE,
                        `后台下单人`=adminAccount,
                        `商品名称`=name,
                        `通用名`=proName,
                        `批准文号`=proPzwh ,
                        `支付类型`=pay_type,
                        `运费`=DEL_TYPE_COST,
                        `优惠券`=shopCouponAmount,
                        `红包`=couponAmount,
                        `积分抵扣`=score_to_cost,
                        `满就减金额`=CUT_AMOUNT
    )
  }) 
  
  
  
  
  output$tbl <- renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'single',
      options = list(
        autoWidth = TRUE,
        columnDefs = list(list(width = '40px', targets = c(5,6,7))),
        searching=TRUE,
        deferRender=TRUE,
        scrollY=200,
        scrollX=TRUE,
        scrollCollapse = TRUE,
        order = list(list(1, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 200,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    ) 
  })
  
}