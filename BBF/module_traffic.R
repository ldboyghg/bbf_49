# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 搜索无结果
keywordNoresultUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(class='toolbar',
             column(6,helpText('1.此表用于查询网站搜索无结果的用于搜索关键词',br(),
                               '2.默认为昨天的数据，历史数据查询暂时定位7天，有限制，数据范围可以优化',br(),
                               '')
             ),
             column(6,helpText( '3.误差：由于采用的搜索计算方式不同，本表中包括有一些可以在网站查到商品的关键词，争取同步网站搜索算法',br(),
                                '4.此表数据慢，要等待3分钟左右',br())
             )
    ),
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('dt'), '搜索日期', start = Sys.Date()-3, end = Sys.Date()-1)),
             column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
    ),

    br(),
    # wordcloud2Output  plotOutput
    fluidRow(
      navbarPage("趋势图与表!",
                 tabPanel("表", 
                          column(11 , DT::dataTableOutput(ns('tbl')))
                 )
                 ,
                 tabPanel("图",
                          column(11 , wordcloud2Output(ns('plot1'), width = "100%", height = "800px") )  
                 )
      )
    )
  )
}
keywordNoresult <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  # 注意表得换：mms_trace_log_tmp_2016_11_18
  df <- reactive({
    dbGetQuery0('e_member', paste0("
     SELECT keyword as cal_in_keyword,sum(cnt ) as cnt 
      from bbf_shiny.tb_mms_keyword_nr_daily 
      where 1=1
      and dt>='",dt()[1],"'
      and dt<='",dt()[2],"'
      GROUP BY keyword
      order by sum(cnt ) DESC
                                    ")) %>%
      select(`无结果关键词`=cal_in_keyword,`搜索次数`=cnt)
  })
  
  
  # export
  output$btn_export <- downloadHandler(paste('bbf-keyword-noresult',Sys.Date(),'.csv',sep=''), content = function(file) {
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
        #order = list(list(11, 'desc')),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 22,
        lengthChange = FALSE,
        initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-200)+'px !important');}"),
        fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-200;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      )
    )
  })
  # renderWordcloud2  renderPlot
  output$plot1 <- renderWordcloud2({
    data <- df()
    wordcloud2(data, size = 0.8, shape='cardioid',color = 'random-dark',
               backgroundColor = "white",fontFamily = "微软雅黑")
    # wordcloud2(demoFreqC, size = 1,color = 'random-light',backgroundColor = "gray", fontWeight='bold',fontFamily = "微软雅黑",
    #            minRotation = -pi/3, maxRotation = pi/3,rotateRatio = 0.8)
    
  })
  }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#------------------------付费渠道成单跟踪-------------------
mmsChnlUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '日期',  start = Sys.Date()-30, end = Sys.Date()-1)),
             column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
    ),
    fluidRow(
      column(5,dataTableOutput(ns('tbl'))) ,
      column(7,plotlyOutput(ns('chnl')))
    )
  )
}
mmsChnl <- function(input, output, session){
  #  未登陆天数  获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  
  
  df <- reactive({
    sql <-
      paste0(" select * from tb_chnl_sale_trace 
             where timest >= '", format(pay_date()[1],'%Y%m%d') ,"'
             and timest <=  '",format(pay_date()[2],'%Y%m%d') ,"'
             ")
    
    
    
    dd <- dbGetQuery1(sql)
    
    df <- dd %>% arrange(desc(TIMEST)) %>% dplyr::rename(`流量日期`=TIMEST  , `渠道`=SRC , `销售额`=SUM_PRICE ,`客户数`=MIDS , `订单数`=OIDS ) 
    
  }) 
  
  
  output$tbl <- renderDataTable({
    datatable(  
      #df() <- df()[order(df()$流量日期),],
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
    )
  })
  
  
  # export
  output$btn_export <- downloadHandler(paste('bbf-data-mms',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      #tmp <- df()[input$tbl_rows_all, , drop = FALSE]
      #write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
      write.csv(df(), file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
    }
    
  })
  
  output$chnl <- renderPlotly({
    
    mytheme <- theme(plot.title=element_text(face="bold.italic",
                                             size="14", color="brown"),
                     axis.title=element_text(face="bold.italic",
                                             size=10, color="brown"),
                     axis.text=element_text(face="bold", size=9,
                                            color="darkblue"),
                     panel.background=element_rect(fill="white",
                                                   color="darkblue"),
                     panel.grid.major.y=element_line(color="grey",
                                                     linetype=1),
                     panel.grid.minor.y=element_line(color="grey",
                                                     linetype=2),
                     panel.grid.minor.x=element_blank(),
                     axis.text.x=element_text(angle = 315,vjust = 0.5,hjust = 0.5),
                     legend.position="none")
    
    
    ggplot(df <-  df() %>% mutate(`流量日期`=substr(`流量日期`,5,8)),aes(`流量日期`,`销售额`))+
      facet_grid(`渠道`~.)+
      #coord_flip()+
      #geom_line()+
      geom_bar(aes(fill=`渠道`),stat="identity",position="dodge",width=0.8)+    
      #geom_area(aes(fill=`渠道`),position="stack",alpha=0.5 )+
      
      # theme(
      #   #text=element_text(family = "myfont"),
      #       legend.position="none",
      #       axis.text.x=element_text(angle = 315,vjust = 0.5,hjust = 0.5),
      #       title=element_text(family = "myfont",face="bold")
      #     )
      mytheme
  })
  
}
