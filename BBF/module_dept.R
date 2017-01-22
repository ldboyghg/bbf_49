# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 中心日报
centreDailyReportUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(class='toolbar',
             # column(2, selectInput(ns('dt'), NULL, choices = substr(as.character(seq(ymd('2016-02-01'), by='month', length.out = 12)-days(1)), 1, 7), selected = substr(Sys.Date(),1,7))),
             column(2, selectInput(ns('dt'), NULL, choices =  substr(as.character(seq(Sys.Date()-days(180) , by='month', length.out = 12)), 1, 7), selected = substr(Sys.Date(),1,7))),
             
             column(2, selectInput(ns('dept'), NULL, choices = c('会员中心', '客户发展部', '客户增值部')))
    ),
    #br(),br(),
    fluidRow(
      DT::dataTableOutput(ns('centre_daily_report'))
    )
  )
}
centreDailyReport <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  dept <- reactive({
    req(input$dept)
    input$dept
  })
  df <- reactive({
    stuff <- dbGetQuery0('bbf_shiny', 'select * from bbf_stuff')
    cc <- dbGetQuery0('bbf_shiny', 'SELECT * FROM bbf_cc_call_report_dept')
    xn <- dbGetQuery0('bbf_shiny', 'SELECT * FROM bbf_xn_daily')
    # 未付款payname为NULL（包括发展和增值客服下单+自主下单）
    # 货到付款的pay_status要做一个转换才方便处理
    # 货到付款可立即取消订单，因此handle_status可能为5
    isback_ <- c(1, 5)
    dept_ <- c('客户发展部', '客户增值部', '健康管理部', '客户服务部')
    if (dept() == '客户发展部') {
      isback_ = 1
      dept_ <- c('客户发展部')
    } else if (dept() == '客户增值部') {
      isback_ = 5
      dept_ <- c('客户增值部')
    }
    # 付款状态pay_stauts跟处理状态handle_status要同时考虑，付款也会有退款或者订单取消行为，货到付款可立即取消
    sql <- paste0("SELECT orderno, substring(post_date_str, 1, 10) as dt, actual_pay as pay, isback, adminaccount, payname, pay_status,handle_status FROM tb_porder WHERE substring(post_date_str,1,7)='", dt(), "' and substring(post_date_str,1,10)<'", Sys.Date(), "'")
    dd <- dbGetQuery0('ecommerce', sql) %>%
      # 这个handle_status排除了支付订单中的退款和取消交易（货到付款可立即取消）
      # 如果需要增加下单金额，这个filter需要放到后面dd1和dd2去过滤
      filter(handle_status %in% c(0, 1, 4, 6, 99)) %>% 
      mutate(
        dt = as.character(dt),
        payname=ifelse(is.na(payname), '未付款', ifelse(payname %in% c('货到付款'), '货到付款', '在线支付')),  # NA跟任意变量进行比较都是NA, 故不能直接`==`而用%in%
        pay_status = ifelse(payname=='货到付款', 1, pay_status)   # 到付pay_status=0，转换为1代表默认会付款，方便后面筛选
      ) %>% 
      filter(payname != '未付款') %>% 
      # 未审核发布导致isback更新错误，需要使用名字来统计
      mutate(
        isback = ifelse(adminaccount %in% stuff$name_py[stuff$dept=='客户发展部'], 1, ifelse(adminaccount %in% stuff$name_py[stuff$dept=='客户增值部'], 5, isback))
      )
    
    # 平台：全部订单(支付：在线＋到付)
    dd1 <- dd %>% 
      mutate(
        payname = ifelse(payname=='货到付款', '平台_货到付款', '平台_在线支付')
      ) %>% 
      group_by(dt, payname) %>%
      summarise(
        pay=sum(pay),
        cnt=n_distinct(orderno)
      ) %>% 
      rename(金额=pay, 单数=cnt) %>% 
      gather(variable, value, -(dt:payname)) %>%
      unite(temp, payname, variable) %>%
      spread(temp, value) %>% 
      mutate(
        `平台_支付总金额` = sum(`平台_在线支付_金额`, `平台_货到付款_金额`, na.rm=TRUE),
        `平台_支付总单数` = sum(`平台_在线支付_单数`, `平台_货到付款_单数`, na.rm=TRUE)
      ) %>%
      select(`平台_在线支付_单数`, `平台_货到付款_单数`, `平台_支付总单数`, `平台_在线支付_金额`, `平台_货到付款_金额`, `平台_支付总金额`)
    
    # 客服：isback==1/5 订单（在线+到付）
    # 在线＋到付
    dd2 <- dd %>% 
      filter(isback %in% isback_) %>% 
      mutate(
        payname = ifelse(payname=='货到付款', '客服_货到付款', '客服_在线支付')
      ) %>% 
      group_by(dt, payname) %>%
      summarise(
        pay=sum(pay),
        cnt=n_distinct(orderno)
      ) %>% 
      rename(金额=pay, 单数=cnt) %>% 
      gather(variable, value, -(dt:payname)) %>%
      unite(temp, payname, variable) %>%
      spread(temp, value) %>% 
      mutate(
        `客服_支付总金额` = sum(`客服_在线支付_金额`, `客服_货到付款_金额`, na.rm=TRUE),
        `客服_支付总单数` = sum(`客服_在线支付_单数`, `客服_货到付款_单数`, na.rm=TRUE)
      ) %>% 
      select(`客服_在线支付_单数`, `客服_货到付款_单数`, `客服_支付总单数`, `客服_在线支付_金额`, `客服_货到付款_金额`, `客服_支付总金额`)
    # cc
    # 通过dept筛选部门
    cc <- cc %>%
      rename(stuff_id=`工号`) %>%
      left_join(stuff, by='stuff_id') %>%
      rename(`工号`=stuff_id) %>% 
      filter(dept %in% dept_)
    cc <- cc %>%
      mutate(
        dt = as.character(dt),
        # 分钟
        `呼入通话平均时长` = hour(hms(`呼入通话平均时长`))*60+minute(hms(`呼入通话平均时长`))+second(hms(`呼入通话平均时长`))/60,
        # 小时
        `呼入通话总时长` = hour(hms(`呼入通话总时长`))+minute(hms(`呼入通话总时长`))/60+second(hms(`呼入通话总时长`))/3600,
        `呼入总数` = as.numeric(`呼入总数`),
        `呼入接通数` = as.numeric(`呼入接通数`),
        # 分钟
        `外呼通话平均时长` = hour(hms(`外呼通话平均时长`))*60+minute(hms(`外呼通话平均时长`))+second(hms(`外呼通话平均时长`))/60,
        # 小时
        `外呼通话总时长` = hour(hms(`外呼通话总时长`))+minute(hms(`外呼通话总时长`))/60+second(hms(`外呼通话总时长`))/3600,
        `外呼总数` = as.numeric(`外呼总数`),
        `外呼成功数` = as.numeric(`外呼成功数`)
      ) %>% 
      group_by(`dt`) %>%
      summarise(
        `呼入总数` = sum(`呼入总数`, na.rm=TRUE),
        `呼入接通数` = sum(`呼入接通数`, na.rm=TRUE),
        `呼入接通率` = `呼入接通数`/`呼入总数`,
        `呼入通话平均时长` = sum(`呼入通话总时长`, na.rm=TRUE)/sum(`呼入接通数`, na.rm=TRUE)*60,
        `呼入通话总时长` = sum(`呼入通话总时长`, na.rm=TRUE),
        `外呼总数` = sum(`外呼总数`, na.rm=TRUE),
        `外呼成功数` = sum(`外呼成功数`, na.rm=TRUE),
        `外呼接通率` = `外呼成功数`/`外呼总数`,
        `外呼通话平均时长` = sum(`外呼通话总时长`, na.rm=TRUE)/sum(`外呼成功数`, na.rm=TRUE)*60,
        `外呼通话总时长` = sum(`外呼通话总时长`, na.rm=TRUE),
        `总通时(H)` = sum(`呼入通话总时长`, `外呼通话总时长`, na.rm=TRUE)
      ) %>% 
      select(dt, `呼入`=`呼入总数`, `呼入接通数`, `呼入接通率`, `呼入平均通时(M)`=`呼入通话平均时长`, `呼入通时(H)`=`呼入通话总时长`, `外呼`=`外呼总数`, `外呼接通`=`外呼成功数`, `外呼接通率`, `外呼平均通时(M)`=`外呼通话平均时长`, `外呼通时(H)`=`外呼通话总时长`, `总通时(H)`)
    # xn
    # 通过dept筛选部门
    xn <- xn %>%
      rename(name=`用户_商户`) %>%
      left_join(stuff, by='name') %>%
      rename(`用户_商户`=name) %>% 
      filter(dept %in% dept_)
    xn <- xn %>% 
      mutate(
        dt = as.character(dt),
        `满意度` = as.numeric(sub('%', '', `满意度`))/100
      ) %>% 
      group_by(dt) %>% 
      summarise(
        `有效咨询` = sum(`有效咨询`),
        `无效咨询` = sum(`无效咨询`),
        `咨询总量` = sum(`咨询总量`),
        `首次响应时间` = mean(`首次响应时间`),
        `平均响应时间` = mean(`平均响应时间`),
        `满意度` = mean(`满意度`),
        `有效咨询%` = sum(`有效咨询`)/sum(`咨询总量`),
        `无效咨询%` = sum(`无效咨询`)/sum(`咨询总量`)
      ) %>% 
      select(dt, `有效咨询`=`有效咨询`, `有效咨询%`, `无效咨询`=`无效咨询`, `无效咨询%`, `咨询总量`=`咨询总量`, `首次响应时间`, `平均响应时间`, `满意度`)
    # 三表合并
    df <- dd1 %>%
      left_join(dd2, by='dt') %>%
      left_join(cc, by='dt') %>%
      left_join(xn, by='dt') %>%
      rename(`日期`=dt) %>% 
      mutate(
        `客服_支付总金额_占比` = `客服_支付总金额`/`平台_支付总金额`
      )
    df[is.na(df)] <- 0
    # 月度汇总
    df %>% 
      add_row(
        `日期`                 = '合计',
        `呼入`                 = sum(df$`呼入`),
        `呼入接通数`           = sum(df$`呼入接通数`),
        `呼入接通率`           = sum(df$`呼入接通数`)/sum(df$`呼入`),
        `呼入平均通时(M)`      = sum(df$`呼入通时(H)`)/sum(df$`呼入接通数`)*60,
        `呼入通时(H)`          = sum(df$`呼入通时(H)`),
        `外呼`                 = sum(df$`外呼`),
        `外呼接通`             = sum(df$`外呼接通`),
        `外呼接通率`           = sum(df$`外呼接通`)/sum(df$`外呼`),
        `外呼平均通时(M)`      = sum(df$`外呼通时(H)`)/sum(df$`外呼接通`)*60,
        `外呼通时(H)`          = sum(df$`外呼通时(H)`),
        `总通时(H)`            = sum(df$`总通时(H)`),
        `有效咨询`             = sum(df$`有效咨询`),
        `无效咨询`             = sum(df$`无效咨询`),
        `咨询总量`             = sum(df$`咨询总量`),
        `有效咨询%`            = sum(df$`有效咨询`)/sum(df$`咨询总量`),
        `无效咨询%`            = sum(df$`无效咨询`)/sum(df$`咨询总量`),
        `首次响应时间`         = as.integer(mean(df$`首次响应时间`)),
        `平均响应时间`         = as.integer(mean(df$`平均响应时间`)),
        `满意度`               = mean(df$`满意度`),
        `平台_在线支付_单数`   = sum(df$`平台_在线支付_单数`),
        `平台_货到付款_单数`   = sum(df$`平台_货到付款_单数`),
        `平台_支付总单数`      = sum(df$`平台_支付总单数`),
        `平台_在线支付_金额`   = sum(df$`平台_在线支付_金额`),
        `平台_货到付款_金额`   = sum(df$`平台_货到付款_金额`),
        `平台_支付总金额`      = sum(df$`平台_支付总金额`),
        `客服_在线支付_单数`   = sum(df$`客服_在线支付_单数`),
        `客服_货到付款_单数`   = sum(df$`客服_货到付款_单数`),
        `客服_支付总单数`      = sum(df$`客服_支付总单数`),
        `客服_在线支付_金额`   = sum(df$`客服_在线支付_金额`),
        `客服_货到付款_金额`   = sum(df$`客服_货到付款_金额`),
        `客服_支付总金额`      = sum(df$`客服_支付总金额`),
        `客服_支付总金额_占比` = sum(df$`客服_支付总金额`)/sum(df$`平台_支付总金额`)
      ) %>% select(
        `日期`,
        `平台_在线支付_单数`,
        `平台_货到付款_单数`,
        `平台_支付总单数`,
        `平台_在线支付_金额`,
        `平台_货到付款_金额`,
        `平台_支付总金额`,
        `客服_在线支付_单数`,
        `客服_货到付款_单数`,
        `客服_支付总单数`,
        `客服_在线支付_金额`,
        `客服_货到付款_金额`,
        `客服_支付总金额`,
        `客服_支付总金额_占比`,
        `呼入`,
        `呼入接通数`,
        `呼入接通率`,
        `呼入平均通时(M)`,
        `呼入通时(H)`,
        `外呼`,
        `外呼接通`,
        `外呼接通率`,
        `外呼平均通时(M)`,
        `外呼通时(H)`,
        `总通时(H)`,
        `有效咨询`,
        `无效咨询`,
        `咨询总量`,
        `有效咨询%`,
        `无效咨询%`,
        `首次响应时间`,
        `平均响应时间`,
        `满意度`
      )
  })
  output$centre_daily_report <- DT::renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      #extensions = c('FixedColumns', 'Scroller'),
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        autoWidth = TRUE,
        columnDefs = list(list(width = '60px', targets = c(0))),
        searching=TRUE,
        deferRender=TRUE,
        scrollY=200,
        scrollX=TRUE,
        scrollCollapse = TRUE,
        order = list(list(0, 'asc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 100,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )  # options end
    ) %>%  # datatable end
      formatRound(c('外呼平均通时(M)', '外呼通时(H)', '呼入平均通时(M)', '呼入通时(H)', '总通时(H)', '平台_在线支付_金额', '平台_货到付款_金额', '平台_支付总金额', '客服_在线支付_金额', '客服_货到付款_金额', '客服_支付总金额'), 2) %>% 
      formatRound(c('首次响应时间', '平均响应时间'), 0) %>% 
      formatPercentage(c('呼入接通率','外呼接通率', '有效咨询%', '无效咨询%', '客服_支付总金额_占比', '满意度'), 2)
  })
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 部门日报
deptDailyReportUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(2, selectInput(ns('dept'), '部门', choices = c('客户发展部', '客户增值部'))),
             column(2, selectInput(ns('team'), '分组', choices = c('全部', '发展1', '发展2', '发展3','发展4'))),
             column(2, dateInput(ns('dt'), '日期', value = Sys.Date()-1, max = Sys.Date()-1), min='2016-03-01'),
             column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'), actionButton(ns('btn_format_dt'), '精简', class = 'btn_nowrap', icon = icon('th')))
    ),
    fluidRow(
      column(12, DT::dataTableOutput(ns('tbl')))
    )
  )
}
deptDailyReport <- function(input, output, session) {
  dept <- reactive({
    req(input$dept)
    input$dept
  })
  team <- reactive({
    req(input$team)
    if(input$team=='全部') {
      c('发展1', '发展2', '发展3','发展4', '增值1')
    } else{
      input$team
    }
  })
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  observeEvent(input$dept, {
    if (dept()=='客户发展部') {
      updateSelectInput(session, 'team', choices = c('全部', '发展1', '发展2', '发展3','发展4'))
    }
    else if(dept()=='客户增值部') {
      updateSelectInput(session, 'team', choices = c('全部', '增值1'))
    }
  })
  
  df <- reactive({
    stuff <- dbGetQuery0('bbf_shiny', 'select * from bbf_stuff')
    stuff <- stuff %>% filter(team %in% team())
    
    sql <- paste0("SELECT orderno, substring(post_date_str, 1, 10) as dt, actual_pay as pay, isback, payname, pay_status,mid,adminaccount,handle_status FROM tb_porder WHERE isback in (1, 5) AND substring(post_date_str,1,10)>='",dt(),"'")
    dd <- dbGetQuery0('ecommerce', sql) %>%
      mutate(
        dt = as.character(dt),
        payname = ifelse(payname %in% c('货到付款'), '货到付款', '在线支付'),
        pay_status = ifelse(payname=='货到付款', 1, pay_status)
      )
    # isback==1 发展订单（支付+下单）
    # isback==5 增值订单（支付+下单）
    # 在线＋到付
    dd1 <- dd %>% 
      filter(handle_status %in% c(0, 1, 4, 6, 99), pay_status==1, !is.na(payname)) %>% 
      group_by(dt, adminaccount, payname) %>%
      summarise(
        pay=sum(pay),
        cnt=n_distinct(orderno)
      ) %>% 
      rename(金额=pay, 单数=cnt) %>% 
      gather(variable, value, -(dt:payname)) %>%
      unite(temp, payname, variable) %>%
      spread(temp, value) %>% 
      mutate(
        `总成交_金额` = sum(`在线支付_金额`, `货到付款_金额`, na.rm=TRUE),
        `总成交_单数` = sum(`在线支付_单数`, `货到付款_单数`, na.rm=TRUE)
      ) %>% 
      select(`在线支付_单数`, `货到付款_单数`, `总成交_单数`, `在线支付_金额`, `货到付款_金额`, `总成交_金额`)
    # 下单总额
    dd2 <- dd %>% 
      group_by(dt, adminaccount) %>%
      summarise(
        pay=sum(pay),
        cnt=n_distinct(orderno)
      ) %>% 
      rename(`下单金额`=pay, `下单数`=cnt)
    
    dd3 <- dd2 %>%
      left_join(dd1, by=c('dt'='dt', 'adminaccount'='adminaccount'))
    #fix na issue
    dd3[is.na(dd3)] <- 0
    dd3 <- dd3 %>% 
      mutate(
        `未付款金额` = `下单金额`-`总成交_金额`,
        `未付款单量` = `下单数`-`总成交_单数`,
        `订单确认%` = `总成交_金额`/`下单金额`
      )
    
    ##update by 20160629 按黄文宇需求调整报表字段
    dd3 <- dd3  %>%
      select(dt,adminaccount,`下单金额`,`在线支付_金额`,`货到付款_金额`,`总成交_金额`,`未付款金额`,`下单数`,`在线支付_单数`,`货到付款_单数`,`总成交_单数`,`未付款单量`,`订单确认%`
      )
    
    
    
    # 呼叫中心
    cc <- dbGetQuery0('bbf_shiny', paste0("SELECT * FROM bbf_cc_call_report_dept WHERE dt='", dt(), "'"))
    # 以stuff表筛选部门后作为合并表的基础，这样可防止漏掉记录
    #cc <- cc %>% rename(stuff_id=`工号`) %>% left_join(stuff, by='stuff_id') %>% rename(`工号`=stuff_id)
    cc <- cc %>% rename(name=`座席`, stuff_id=`工号`)
    cc <- stuff %>% left_join(cc, by=c('name'='name', 'stuff_id'='stuff_id'))
    cc <- cc %>%
      filter(dept == dept()) %>%
      mutate(
        dt = ifelse(is.na(dt), as.character(dt[!is.na(dt)][1]), as.character(dt)),
        `呼入通话平均时长` = hour(hms(`呼入通话平均时长`))*60+minute(hms(`呼入通话平均时长`))+second(hms(`呼入通话平均时长`))/60,
        `呼入通话总时长` = hour(hms(`呼入通话总时长`))+minute(hms(`呼入通话总时长`))/60+second(hms(`呼入通话总时长`))/3600,
        `呼入总数` = as.numeric(`呼入总数`),
        `呼入接通数` = as.numeric(`呼入接通数`),
        `外呼通话平均时长` = hour(hms(`外呼通话平均时长`))*60+minute(hms(`外呼通话平均时长`))+second(hms(`外呼通话平均时长`))/60,
        `外呼通话总时长` = hour(hms(`外呼通话总时长`))+minute(hms(`外呼通话总时长`))/60+second(hms(`外呼通话总时长`))/3600,
        `外呼总数` = as.numeric(`外呼总数`),
        `外呼成功数` = as.numeric(`外呼成功数`)
      ) %>% 
      group_by(`dt`, `name`) %>%
      summarise(
        `stuff_id`         = max(stuff_id),
        `name_py`          = max(name_py),
        `呼入总数`         = sum(`呼入总数`, na.rm=TRUE),
        `呼入接通数`       = sum(`呼入接通数`, na.rm=TRUE),
        `呼入接通率`       = `呼入接通数`/`呼入总数`,
        `呼入通话平均时长` = mean(`呼入通话平均时长`, na.rm=TRUE),
        `呼入通话总时长`   = sum(`呼入通话总时长`, na.rm=TRUE),
        `外呼总数`         = sum(`外呼总数`, na.rm=TRUE),
        `外呼成功数`       = sum(`外呼成功数`, na.rm=TRUE),
        `外呼接通率`       = `外呼成功数`/`外呼总数`,
        `外呼通话平均时长` = mean(`外呼通话平均时长`, na.rm=TRUE),
        `外呼通话总时长`   = sum(`外呼通话总时长`, na.rm=TRUE),
        `总通时(H)`        = `呼入通话总时长`+`外呼通话总时长`
      ) %>% 
      select(dt, name, `name_py`, `工号`=stuff_id, `呼入`=`呼入总数`, `呼入接通数`, `呼入接通率`, `呼入平均通时(M)`=`呼入通话平均时长`, `呼入通时(H)`=`呼入通话总时长`, `外呼`=`外呼总数`, `外呼接通数`=`外呼成功数`, `外呼接通率`, `外呼平均通时(M)`=`外呼通话平均时长`, `外呼通时(H)`=`外呼通话总时长`, `总通时(H)`)
    
    
    
    # 小能
    xn <- dbGetQuery0('bbf_shiny', paste0("SELECT * FROM bbf_xn_daily where dt='",dt(),"'"))
    # xn <- xn %>% rename(name=`用户_商户`) %>% left_join(stuff, by='name') %>% rename(`用户_商户`=name)
    xn <- xn %>% rename(name=`用户_商户`)
    xn <- stuff %>% left_join(xn, by='name')
    xn <- xn %>% 
      filter(dept == dept()) %>% 
      group_by(name) %>% 
      mutate(
        dt = ifelse(is.na(dt), as.character(dt[!is.na(dt)][1]), as.character(dt)),
        `有效咨询%` = `有效咨询`/`咨询总量`,
        `无效咨询%` = `无效咨询`/`咨询总量`
      ) %>% 
      select(dt, name, `有效咨询`=`有效咨询`, `有效咨询%`, `无效咨询`=`无效咨询`, `无效咨询%`, `咨询总量`=`咨询总量`, `首次响应时间`, `平均响应时间`, `满意度`)
    
    df <- cc %>% 
      left_join(dd3, by=c('dt'='dt', 'name_py'='adminaccount')) %>%
      left_join(xn, by=c('dt'='dt', 'name'='name')) %>% 
      rename(`日期`=dt, `座席`=name) %>%
      select(-name_py)
    
    # fixed na issue
    df[is.na(df)] <- 0
    df <- df %>%
      mutate(
        `满意度` = as.numeric(sub('%', '', `满意度`))/100,
        `转化率` = ifelse(`呼入接通数`+`外呼接通数`+`咨询总量`==0, 0, `总成交_单数`/(`呼入接通数`+`外呼接通数`+`咨询总量`))
      )
    df[is.na(df)] <- 0
    df %>% 
      add_row(
        `日期`            = '合计',
        `座席`            = '',
        `工号`            = 8888,
        `呼入`            = sum(df$`呼入`),
        `呼入接通数`      = sum(df$`呼入接通数`),
        `呼入接通率`      = sum(df$`呼入接通数`)/sum(df$`呼入`),
        `呼入平均通时(M)` = mean(df$`呼入平均通时(M)`),
        `呼入通时(H)`     = sum(df$`呼入通时(H)`),
        `外呼`            = sum(df$`外呼`),
        `外呼接通数`      = sum(df$`外呼接通数`),
        `外呼接通率`      = sum(df$`外呼接通数`)/sum(df$`外呼`),
        `外呼平均通时(M)` = mean(df$`外呼平均通时(M)`),
        `外呼通时(H)`     = sum(df$`外呼通时(H)`),
        `总通时(H)`       = sum(df$`总通时(H)`),
        `下单金额`        = sum(df$`下单金额`),
        
        `在线支付_金额`   = sum(df$`在线支付_金额`),
        `货到付款_金额`   = sum(df$`货到付款_金额`),
        `总成交_金额`     = sum(df$`总成交_金额`),
        `未付款金额`      = sum(df$`未付款金额`),
        
        ##update by 20160629 按黄文宇需求调整报表字段
        `下单数`          = sum(df$`下单数`),
        `在线支付_单数`   = sum(df$`在线支付_单数`),
        `货到付款_单数`   = sum(df$`货到付款_单数`),
        `总成交_单数`     = sum(df$`总成交_单数`),
        
        `未付款单量`      = sum(df$`未付款单量`),
        `订单确认%`       = sum(df$`总成交_金额`)/sum(df$`下单金额`),
        `有效咨询`        = sum(df$`有效咨询`),
        `有效咨询%`       = sum(df$`有效咨询`)/sum(df$`咨询总量`),
        `无效咨询`        = sum(df$`无效咨询`),
        `无效咨询%`       = sum(df$`无效咨询`)/sum(df$`咨询总量`),
        `咨询总量`        = sum(df$`咨询总量`),
        `首次响应时间`    = as.integer(mean(df$`首次响应时间`)),
        `平均响应时间`    = as.integer(mean(df$`平均响应时间`)),
        `满意度`          = mean(df$`满意度`, na.rm=TRUE),
        `转化率`          = ifelse(sum(df$`呼入接通数`)+sum(df$`外呼接通数`)+sum(df$`咨询总量`)==0, 0, sum(df$`总成交_单数`)/(sum(df$`呼入接通数`)+sum(df$`外呼接通数`)+sum(df$`咨询总量`)))
      )
    
  })
  # 精简列
  observeEvent(input$btn_format_dt, {
    #js$colVis(0, 1, 2, 4, 9, 18, 19, 20, 21, 22, 29, 30, 31, 33)
    #因为黄文宇调整字段，所有精简列也需要调整字段顺序
    js$colVis(0, 1, 2, 4, 9, 22, 15, 16, 17, 18, 29, 30, 31, 33)
  })
  output$tbl <- DT::renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      #extensions = c('FixedColumns', 'Scroller'),
      extensions = list(FixedColumns = list(leftColumns = 2), Scroller=list()),
      # save instance of this table to global window, so then we can use it for colVis
      callback = JS("window.xtbl = table;"),
      options = list(
        autoWidth = TRUE,
        columnDefs = list(list(width = '60px', targets = c(0, 1))),
        searching=TRUE,
        search = list(regex = TRUE, caseInsensitive = FALSE),
        deferRender=TRUE,
        scrollY=200,
        scrollX=TRUE,
        scrollCollapse = TRUE,
        order = list(list(2, 'asc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 100,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )  # options end
    ) %>%  # datatable end
      formatRound(c('外呼平均通时(M)', '外呼通时(H)', '呼入平均通时(M)', '呼入通时(H)', '总通时(H)', '总成交_金额', '未付款金额'), 2) %>% 
      formatPercentage(c('呼入接通率','外呼接通率', '满意度', '有效咨询%', '无效咨询%', '订单确认%', '转化率'), 2)
  })
  
  output$btn_export <- downloadHandler(paste('bbf-data-dept-',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      tmp <- df()[input$tbl_rows_all, , drop = FALSE]
      write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names = FALSE, col.names = FALSE)
    }
    
  })
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~