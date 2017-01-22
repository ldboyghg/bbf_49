
# 界面框架
library(shiny)
library(shinydashboard)
# javascript与R交互
library(shinyjs)
# 浏览器localstorage保存登录信息
library(shinyStore)
# 透视表
library(plyr)
library(dplyr)
library(tidyr)
#字符串处理
library(stringr)
# 日期处理
library(lubridate)
library(xts)
library(graphics)
# 做图
library(ggplot2)
library(plotly)
library(d3funnel)
library(forecast)
library(dygraphs)
library(showtext)
library(directlabels)
library(treemap)
library(REcharts3)
library(recharts)
#library(REmap)
library(highcharter)
library(maptools)
library(wordcloud2)
#library(d3treeR)
library(viridis)
library(arulesViz)
#模型
library(arules)
# 数据表格
library(DT)
library(rhandsontable)
# 加密
library(PKI)
# 数据库读写
library(DBI)
#访问oracle
library(rJava)
library(RJDBC)
# csv读取
library(readr)


#options(shiny.error=recover)
source('global.R', encoding='utf-8')

# colVis is used to hide the col that don't need
js_code <- '
shinyjs.colVis = function(col) {xtbl.columns()[0].filter(function(x) { return col.indexOf(x) < 0 }).forEach(function(i){xtbl.column(i).visible(0)})}
'

# prepare data
stuff <- dbGetQuery0('bbf_shiny', paste0("SELECT * FROM bbf_stuff"))

# localstorage加密key
pub_key <- PKI.load.key(file="./data/bbf.key.pub")
priv_key <- PKI.load.key(file="./data/bbf.key")

# 定义一个登录面板，后面多个判断逻辑重复使用
ui_login <- dashboardBody(
  wellPanel(
    id='loginpanel',
    textInput("username", icon('user', class='fa-fw'), placeholder='username', value = ''),
    passwordInput("password", icon('lock', class='fa-fw'), value = ''),
    tags$br(),
    actionButton("btn_login",label="登 录", icon=icon('sign-in')),
    actionButton("btn_reset",label="重 置", icon=icon('undo'), style='margin-left:20px;'),
    style = "width:500px; margin-top: 100px; margin-bottom: 400px; margin-left: auto; margin-right: auto;"
  )
)

# ui
ui = dashboardPage(
  header = dashboardHeader(
    title = img(src='./image/logo.png', width='150px;'),
    titleWidth = 180,
    dropdownMenu(type = 'messages', badgeStatus = 'success'),
    dropdownMenu(type='tasks', badgeStatus = 'warning')
  ),
  sidebar = dashboardSidebar(
    # 引入shinyjs
    useShinyjs(),
    extendShinyjs(text = js_code),
    # 初始化localStorage
    initStore("store", "shinyStore-bbf", priv_key),
    # ~~~~~~~~~~~~~~~~~~~~~~~~
    width = 180,
    uiOutput("userpanel"),
    sidebarMenu(id="tabs",
                sidebarMenuOutput('sidebarmenu')
    )
  ),
  body = uiOutput('page'),
  title = '八百方BI报表系统',
  skin = 'yellow'
)
# server
server = function(input, output, session) {
  # put module here to avoid restart shiny server when modified module
  #source('module_zwy.R', encoding='utf-8', local = TRUE)
  source('module.R', encoding='utf-8', local = TRUE)
  #source('module2.R', encoding='utf-8', local = TRUE)
  #销售主题
  source('module_sales.R', encoding='utf-8', local = TRUE)
  
  ##测试
  source('learn_by_ghg.R', encoding='utf-8', local = TRUE)
  
  #商品主题
  source('module_product.R', encoding='utf-8', local = TRUE)
  
  #用户分析
  source('module_customer.R', encoding='utf-8', local = TRUE)
  
  #KPI
  source('module_kpi.R', encoding='utf-8', local = TRUE)
  
  #流量
  source('module_traffic.R', encoding='utf-8', local = TRUE)
  
  #商家
  source('module_merchant.R', encoding='utf-8', local = TRUE)
  
  #订单
  source('module_orders.R', encoding='utf-8', local = TRUE)
  
  #部门
  source('module_dept.R', encoding='utf-8', local = TRUE)
  
  # 登录验证逻辑
  observeEvent(input$btn_login, {
    username <- as.character(isolate(input$username))
    password <- as.character(isolate(input$password))
    user <- dbGetQuery0('bbf_shiny', paste0("SELECT * FROM bbf_user WHERE status=1 AND username='",username,"' AND password='",digest::digest(paste0(username, password),'md5'),"'"))
    if (nrow(user) == 1){
      key <- pub_key
      # key <- NULL
      updateStore(session, "username", username, encrypt=key)
      #session5天效期
      expired = as.numeric(Sys.time())+5*24*60*60
      updateStore(session, "expired", expired, encrypt=key)
    } else {
      info('登录失败,请检查用户名和密码.')
      reset('password')
    }
    
  })
  # 重置按钮
  observeEvent(input$btn_reset, {
    reset('username')
    reset('password')
  })
  # 检测是否填写了登录信息
  observe({
    toggleState("btn_login", !is.null(input$username) && input$username != "" && !is.null(input$password) && input$password != "")
  })
  # 退出登录时清空session
  observeEvent(input$al_logout, {
    key <- pub_key
    # key <- NULL
    updateStore(session, "username", NULL, encrypt=key)
    # 退出登录则减去1天
    expired = as.numeric(Sys.time())-24*60*60
    updateStore(session, "expired", expired, encrypt=key)
  })
  # menu click
  onclick('msg1', {
    info('done')
  })
  
  # 登录验证后刷新UI
  observe({
    # 如果localstorage中的expired为空，表示第一次访问
    # 如果曾经访问过则不为空（不管session是否已过期）
    if (is.null(input$store$expired)){
      output$sidebarmenu <- renderMenu({
        sidebarMenu(
          menuItem('请先登录', tabName = 'tn_null', icon = icon('dashboard')),
          .list = list()
        )
      })
      output$page <- renderUI({
        ui_login
      })
    } else{
      # 如果localstorage中的expired存在且未过期则渲染菜单
      if (as.numeric(Sys.time()) < input$store$expired){
        # 验证usename：localstorage中的username跟数据库比对
        sql <- paste0("select * from bbf_user where username='", input$store$username, "' and status=1")
        uu <- dbGetQuery0('bbf_shiny', sql)
        if (nrow(uu)==1) {
          # 权限控制在这里实现
          sql <- paste0("SELECT * FROM bbf_menu WHERE status=1 and privilege like '%", input$store$username, "%' order by orders")
          menu <- dbGetQuery0('bbf_shiny', sql)
          if(nrow(menu)==0) {
            menu0 <- TRUE
            menu <- data.frame(menu1='权限受限', menu2='权限受限', status=1, orders=1, privilege='')
          } else {
            menu0 <- FALSE
          }
          msg <- apply(menu %>% arrange(orders) %>% group_by(menu1) %>% summarise(status=min(status)), 1, function(row1){
            msg1 <- apply(menu %>% filter(menu1==row1[['menu1']], status==1), 1, function(row2){
              menuSubItem(row2[['menu2']], tabName = paste0('tn_', row2[['menu2']]))
            })
            menuItem(
              text = row1[['menu1']],
              tabName =paste0('tn_', row1[['menu1']]),
              icon = icon('th'),
              .list=msg1
            )
          })
          output$sidebarmenu <- renderMenu({
            sidebarMenu(
              menuItem(
                text = '仪表盘',
                tabName = ifelse(menu0, 'null', 'tn_dashboard'),
                icon = icon('dashboard')
              ),
              .list = msg
            )
          })
          
          output$page <- renderUI({
            dashboardBody(
              includeCSS('./www/style.css'),
              tabItems(
                #             menu2             module       moduleID
                #           ↓↓↓↓↓↓↓↓↓         ↓↓↓↓↓↓↓↓↓↓↓  ↓↓↓↓↓↓↓↓↓↓↓↓↓
                tabItem('tn_dashboard',       dashboardUI('mid_dashboard')),
                tabItem('tn_账号管理',        userTableUI('mid_user_table')),
                tabItem('tn_系统菜单',        menuTableUI('mid_menu_table')),
               
                
                
                
                #订单分析
                #tabItem('tn_销售概况',        saleSumUI('mid_sale_summary')),
                #tabItem('tn_退款原因',        drawbackReasonReportUI('mid_drawback_reason_report')),
                tabItem('tn_转单报表',        transferReportUI('mid_transfer_report')),
                tabItem('tn_咨询记录',        customerUI('mid_customer')),
                tabItem('tn_店铺退款',        shopDrawbackUI('mid_shop_drawback')),
                tabItem('tn_商家缺货率',      merchantPenaltyUI('mid_merchant_penalty')),
                tabItem('tn_两天未发货跟踪',      sendProDelayTwoDaysUI('mid_sendProDelayTwoDays')),
                tabItem('tn_未付款跟踪',      nonPaymentUI('mid_nonPayment')), 
                
                
                
                
                
                #部门报表
                tabItem('tn_八百方运营数据报表',        bbfYyDataUI('mid_bbfYyData')),
                tabItem('tn_中心日报',        centreDailyReportUI('mid_centre_daily_report')),
                tabItem('tn_部门日报',        deptDailyReportUI('mid_dept_daily_report')),
                
                
                
                #销售主题
                tabItem('tn_活动分析',        marketingEvaluationUI('mid_marketing_evaluation')),
                tabItem('tn_销售品种数趋势',        saleskuTrendsUI('mid_sale_sku_Trends')),
                tabItem('tn_商品活动评估',       productMarketingvalUI('mid_product_marketing_val')),
                tabItem('tn_日小时销售',        salesHourUI('sales_hour')),
                tabItem('tn_货到付款订单趋势',       payFacetofaceUI('mid_pay_face_to_face')),
                tabItem('tn_地域销售概况',        salesMapUI('mid_salesMap')),
                tabItem('tn_每日支付方式',        salesDayTypeUI('mid_salesDayType')),
                tabItem('tn_红包分析',     hongbaoUseUI('mid_hongbao_Use')),
                tabItem('tn_电话查销售',     salesbaseOnmobileUI('mid_sales_base_Onmobile')),
                
                
                
                #商品主题
                tabItem('tn_热卖商品',        topProductUI('mid_top_product')),
                tabItem('tn_类目复购',        cateRebuyUI('mid_cate_rebuy')),
                tabItem('tn_商品复购',        productRebuyUI('mid_product_rebuy')),
                tabItem('tn_商家销售排名',        merchantRankUI('mid_merchantRank')),
                tabItem('tn_商品通用名排名',        pronameProductUI('mid_proname_product')),
                tabItem('tn_品类优化',        proudctOptimizeUI('mid_proudctOptimize')),
                tabItem('tn_经营商家监测',     merchantOperateUI('mid_merchant_operate')),
                tabItem('tn_经营商品监测',     productMonitorUI('mid_product_Monitor')),
                
                #用户分析
                tabItem('tn_客单价区间',        customerArpuUI('mid_customerArpu')),
                tabItem('tn_客单价',          kdUI('mid_kd')),
                ##tabItem('tn_RFM分布',         rfmUI('mid_rfm'))
                tabItem('tn_RFM',             rfmTableUI('mid_rfm_table')),
                tabItem('tn_注册购买转化',    registerBuyUI('mid_register_Buy')),
                tabItem('tn_购买次数汇总',        customerBuyCntsUI('mid_customerBuyCnts')),
                tabItem('tn_省份城市分析',        provinceCityUI('mid_province_city')),
                tabItem('tn_RFM维度分布',        RFMDistributionUI('mid_rfm_distribution')),
                tabItem('tn_关联规则',        rulesUI('mid_rules')),
                tabItem('tn_单品关联规则',     itemRuleUI('mid_item_Rule')),
                tabItem('tn_会员等级分布',        customerLevelUI('mid_customerLevel')),
                tabItem('tn_会员性别分布',        customerSexUI('mid_customerSex')),
                tabItem('tn_会员年龄分布',        customerAgeUI('mid_customerAge')),
                tabItem('tn_会员购买间隔分布',        customerBuyDiffUI('mid_customerBuyDiff')),
                tabItem('tn_会员生命周期分布',        customerLifeCycleUI('mid_customerLifeCycle')),
                tabItem('tn_用户总表',       customerTotalsituationUI('mid_customer_total_situation')),
                tabItem('tn_流失监控',      lostcustomerMonitorUI('mid_lost_customer_monitor')),
                tabItem('tn_新注册红包提醒',     newregisterHongbaoUI('mid_newregister_Hongbao')),
                tabItem('tn_会员购买留存',        customerBuyKeepUI('mid_customerBuyKeep')),
                
                
                # kpi 
                tabItem('tn_月度KPI仪表盘',      monthKpiUI('mid_month_kpi')),
                tabItem('tn_月度客服绩效',      kefumonthKpiUI('mid_kefu_month_kpi')),
                tabItem('tn_发货与成单金额',      fahuoVSchendanUI('mid_fahuo_vs_chendan')),
                tabItem('tn_商家月度分账统计',     merchantmonthKpiUI('mid_merchant_monthKpi')),
                
                # 流量专题 traffic  ###############################
                #搜索无结果
                tabItem('tn_搜索无结果',     keywordNoresultUI('mid_keyword_Noresult')),
                tabItem('tn_付费渠道成单跟踪',        mmsChnlUI('mid_mmsChnl')),
                
                
                ##商家##################################
                tabItem('tn_商家销售分析',        merchantUI('mid_merchant')),
                tabItem('tn_非重点商家高动销商品监测',     commonMerchantUI('mid_commonMerchant')),
                tabItem('tn_重点商家高动销商品监测',       keyMerchantUI('mid_keyMerchant')),
                tabItem('tn_圆通货到付款',     yuantongHdfkUI('mid_yuantong_Hdfk'))
              )
            )
          })
        } else {
          key <- pub_key
          # key <- NULL
          updateStore(session, "username", NULL, encrypt=key)
          # 退出登录则减去1天
          expired = as.numeric(Sys.time())-24*60*60
          updateStore(session, "expired", expired, encrypt=key)
        }
        # 如果localstorage中的expired存在且已过期
      } else {
        output$sidebarmenu <- renderMenu({
          sidebarMenu(
            menuItem('请先登录', tabName = 'tn_null', icon = icon('dashboard')),
            .list = list()
          )
        })
        output$page <- renderUI({
          ui_login
        })
      }
    }
  })
  # 初始化用户面板
  observe({
    output$userpanel = renderUI({
      # 如果expired为空则显现未登录状态
      # 如果当前时间小于expired说明session有效，现实登录状态
      if (is.null(input$store$expired)){
        sidebarUserPanel('Offline', image = "image/head.png")
      } else if (as.numeric(Sys.time()) < input$store$expired) {
        sidebarUserPanel(
          a(input$store$username, href='#shiny-tab-tn_reset_password', `data-toggle`='tab', `data-value`='tn_reset_password'),
          subtitle = actionLink('al_logout', 'Logout', icon=icon('sign-out')),
          image = "image/head.png"
        )
      }
    })
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 监控面板
  callModule(module = dashboard,             id = 'mid_dashboard')
  # 用户管理
  callModule(module = userTable,             id = 'mid_user_table')
  # 菜单管理
  callModule(module = menuTable,             id = 'mid_menu_table')
  # 退款原因报表
  #callModule(module = drawbackReasonReport,  id = 'mid_drawback_reason_report')
  
  
  #############部门主题######################
  #八百方运营数据报表
  callModule(module = bbfYyData,            id = 'mid_bbfYyData')
  # 中心日报
  callModule(module = centreDailyReport,     id = 'mid_centre_daily_report')
  # 部门日报
  callModule(module = deptDailyReport,       id = 'mid_dept_daily_report')
  
  
  
  
  ##销售主题
  # 活动总体评估
  callModule(module =  marketingEvaluation,       id = 'mid_marketing_evaluation')
  #tn_销售品种数趋势
  callModule(module =  saleskuTrends,       id = 'mid_sale_sku_Trends')
  #商品活动评估
  callModule(module =  productMarketingval,       id = 'mid_product_marketing_val')
  #日小时销售
  callModule(module = salesHour,       id = 'sales_hour')
  #每日支付方式
  callModule(module = salesDayType,       id = 'mid_salesDayType')
  #货到付款订单趋势
  callModule(module =  payFacetoface,       id = 'mid_pay_face_to_face')
  #地域销售概况
  callModule(module =  salesMap,       id = 'mid_salesMap')
  #tn_红包分析
  callModule(module =  hongbaoUse,       id = 'mid_hongbao_Use')
  #tn_电话查销售
  callModule(module =  salesbaseOnmobile,       id = 'mid_sales_base_Onmobile')
  
  ##商品主题
  # 热卖产品
  callModule(module = topProduct,            id = 'mid_top_product')
  #类目复购
  callModule(module = cateRebuy,             id = 'mid_cate_rebuy')
  # 商品复购
  callModule(module = productRebuy,          id = 'mid_product_rebuy')
  #tn_商家销售排名
  callModule(module = merchantRank,            id = 'mid_merchantRank')
  #tn_商品通用名排名
  callModule(module =  pronameProduct,       id = 'mid_proname_product')
  #品类优化
  callModule(module = proudctOptimize,            id = 'mid_proudctOptimize')
  #经营商家监测
  callModule(module =  merchantOperate,       id = 'mid_merchant_operate')
  #tn_经营商品监测
  callModule(module =  productMonitor,       id = 'mid_product_Monitor')
  #tn_圆通货到付款
  callModule(module =  yuantongHdfk,       id = 'mid_yuantong_Hdfk')
  
  #用户分析
  # 客单价区间
  callModule(module = customerArpu,           id = 'mid_customerArpu')
  # 客单价
  callModule(module = kd,                    id = 'mid_kd')
  # RFM分布/RFM细分表
  #callModule(module = rfm,                   id = 'mid_rfm')
  # RFM分类统计表
  callModule(module = rfmTable,              id = 'mid_rfm_table')
  #当天注册，当天购买
  callModule(module = registerBuy,              id = 'mid_register_Buy')
  #购买次数汇总
  callModule(module = customerBuyCnts,        id = 'mid_customerBuyCnts')
  #省份城市购买
  callModule(module = provinceCity,        id = 'mid_province_city')
  #RFM维度分布
  callModule(module = RFMDistribution,       id = 'mid_rfm_distribution')
  #关联规则
  callModule(module = rules,       id = 'mid_rules')
  #tn_单品关联规则
  callModule(module =  itemRule,       id = 'mid_item_Rule')
  #会员等级分布
  callModule(module = customerLevel,       id = 'mid_customerLevel')
  #会员性别分布
  callModule(module = customerSex,        id = 'mid_customerSex')
  #会员年龄分布
  callModule(module = customerAge,        id = 'mid_customerAge')
  #会员购买间隔分布
  callModule(module = customerBuyDiff,       id = 'mid_customerBuyDiff')
  #会员生命周期分布
  callModule(module = customerLifeCycle,       id = 'mid_customerLifeCycle')
  #会员生命周期分布
  callModule(module = customerBuyKeep,       id = 'mid_customerBuyKeep')
  
  #用户总表
  callModule(module =  customerTotalsituation,       id = 'mid_customer_total_situation')
  #tn_流失监控
  callModule(module =  lostcustomerMonitor,       id = 'mid_lost_customer_monitor')
  #新注册红包提醒
  callModule(module =  newregisterHongbao,       id = 'mid_newregister_Hongbao')
  
  
  ##KPI
  #tn_月度KPI仪表盘
  callModule(module =  monthKpi,       id = 'mid_month_kpi')
  #tn_月度客服绩效
  callModule(module =  kefumonthKpi,       id = 'mid_kefu_month_kpi')
  #tn_发货与成单金额
  callModule(module =  fahuoVSchendan,       id = 'mid_fahuo_vs_chendan')
  # 商家月度分账统计 
  callModule(module =  merchantmonthKpi,       id = 'mid_merchant_monthKpi')
  
  ##流量专题 traffic
  #tn_搜索无结果
  callModule(module =  keywordNoresult,       id = 'mid_keyword_Noresult')
  
  #付费渠道成单跟踪
  callModule(module = mmsChnl,       id = 'mid_mmsChnl')
  
  
  
  ################商家分析#####################
  #商家销售分析
  callModule(module = merchant,       id = 'mid_merchant')
  #非重点商家高动销商品监测
  callModule(module = commonMerchant,       id = 'mid_commonMerchant')
  #重点商家高动销商品监测
  callModule(module = keyMerchant,       id = 'mid_keyMerchant')
  
  #################################learn-by-gonghg#######################
  #销售概况
 # callModule(module = saleSum,       id = ('mid_sale_summary'))
  # 退款测试
 # callModule(module = drawbackReasonReport1,  id = 'mid_drawback_reason_report1')
  
  #######################################################################
  
  
  
  ################订单分析##################
  # 转单报表
  callModule(module = transferReport,        id = 'mid_transfer_report')
  # 咨询记录
  callModule(module = customer,              id = 'mid_customer')
  # 店铺退款
  callModule(module = shopDrawback,          id = 'mid_shop_drawback')
  # 商家缺货率
  callModule(module = merchantPenalty,       id = 'mid_merchant_penalty')
  #两天未发货跟踪
  callModule(module = sendProDelayTwoDays,       id = 'mid_sendProDelayTwoDays')
  #未付款跟踪
  callModule(module = nonPayment,       id = 'mid_nonPayment')
  nonPaymentUI
  

  
  
    isolate({updateTabItems(session, "tabs", "tn_dashboard")})
}
# 入口
shinyApp(ui, server)
