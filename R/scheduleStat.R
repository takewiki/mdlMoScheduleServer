#' 生产订单排程信息
#'
#' @param token 口令
#' @param FPrdNumber_start 产品编码从
#' @param FPrdNumber_end 产品编码到
#' @param FMoNumber 订单编码
#' @param FCompanyName 公司
#' @param FWorkShop  生产车间
#' @param FRouteName 工序
#' @param FPrdSeries 产品系列
#'
#' @return 返回值
#' @export
#'
#' @examples
#' schedule_query_stat()
schedule_query_stat <- function(token ='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
                                FPrdNumber_start ='801203-99',
                                FPrdNumber_end ='801203-99',
                                FMoNumber ='MO202204665',
                                FCompanyName ='苏州赛普生物技术有限公司',
                                FWorkShop ='总部生产部',
                                FRouteName ='',
                                FPrdSeries =''

) {
  sql_FMoNumber <- tsdo::where_condition(key = 'FMoNumber',value =FMoNumber,operator = '=' )
  sql_FCompanyName <- tsdo::where_condition(key = 'FCompanyName',value=FCompanyName,operator = '=')
  sql_FWorkShop <- tsdo::where_condition(key = 'FWorkShop',value=FWorkShop,operator = '=')
  sql_FRouteName <- tsdo::where_condition(key = 'FRouteName',value=FRouteName,operator = '=')
  sql_FPrdSeries <- tsdo::where_condition(key = 'FPrdSeries',value=FPrdSeries,operator = '=')
  sql <- paste0("SELECT
  [FCompanyName] as 公司
      ,[FWorkShop] as 生产车间
      ,[FRouteName] as 工序
      ,[FPrdSeries]  as 产品系列
 , [FMoNumber] as 生产订单编号
,FPrdNumber as 产品代码
,FPrdName as 产品名称
,FPackSpec as 包装规格
      ,fstartPlanDate   as  排产开始日期
      ,[FEndPlanDate]   as 排产结束日期
      ,[FTotalPlanQty]    as 总排产量
      ,[FPlanCount]    as 排产次数
      ,[FAvgPlanQty]   as 日均排产量
  FROM [dbo].[rds_mfg_moSchedule_stat]
  where FPrdNumber >='",FPrdNumber_start,"' and FPrdNumber <='",FPrdNumber_end,"'",sql_FMoNumber,sql_FCompanyName,sql_FWorkShop,sql_FRouteName,sql_FPrdSeries)
  conn = tsda::sql_getConn(token = token)
  data = tsda::sql_select(conn,sql)
  return(data)

}



#' 生产订单排产统计表
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' scheduleStatServer()
scheduleStatServer <- function(input,output,session,token) {

  var_txtscheduleStat_FPrdNumber_start <- tsui::var_text('txtscheduleStat_FPrdNumber_start')
  var_txtscheduleStat_FPrdNumber_end <- tsui::var_text('txtscheduleStat_FPrdNumber_end')
  var_txtscheduleStat_FMoNumber <- tsui::var_text('txtscheduleStat_FMoNumber')

  var_txtscheduleStat_FCompanyName <- tsui::var_text('txtscheduleStat_FCompanyName')
  var_txtscheduleStat_FWorkShop <- tsui::var_text('txtscheduleStat_FWorkShop')

  var_txtscheduleStat_FRouteName <- tsui::var_text('txtscheduleStat_FRouteName')
  var_txtscheduleStat_FPrdSeries <- tsui::var_text('txtscheduleStat_FPrdSeries')

  shiny::observeEvent(input$btnscheduleStat_query,{
    FPrdNumber_start = var_txtscheduleStat_FPrdNumber_start()
    FPrdNumber_end = var_txtscheduleStat_FPrdNumber_end()
    FMoNumber = var_txtscheduleStat_FMoNumber()
    FCompanyName = var_txtscheduleStat_FCompanyName()
    FWorkShop = var_txtscheduleStat_FWorkShop()
    FRouteName = var_txtscheduleStat_FRouteName()
    FPrdSeries = var_txtscheduleStat_FPrdSeries()





    data = schedule_query_stat(token = token
                               ,FPrdNumber_start = FPrdNumber_start
                                 ,FPrdNumber_end = FPrdNumber_end
                                 ,FMoNumber = FMoNumber,
                               FCompanyName = FCompanyName,
                               FWorkShop = FWorkShop,
                               FRouteName = FRouteName,
                               FPrdSeries =FPrdSeries
                                 )
    tsui::run_dataTable2(id = 'dataviewscheduleStat_query',data = data)
    tsui::run_download_xlsx(id = 'btnscheduleStat_dl',data = data,filename = tsui::file_name('生产订单排产统计表'))

  })

}
