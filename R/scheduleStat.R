#' 生产订单排程信息
#'
#' @param token 口令
#' @param FPrdNumber_start 产品编码从
#' @param FPrdNumber_end 产品编码到
#' @param FMoNumber 订单编码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' schedule_query_stat()
schedule_query_stat <- function(token ='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
                                FPrdNumber_start ='801203-99',
                                FPrdNumber_end ='801203-99',
                                FMoNumber ='MO202204665'



) {
  sql_FMoNumber <- tsdo::where_condition(key = 'FMoNumber',value =FMoNumber,operator = '=' )
  sql <- paste0("SELECT [FMoNumber] as 生产订单编号
,FPrdNumber as 产品代码
,FPrdName as 产品名称
,FPackSpec as 包装规格
      ,fstartPlanDate   as  排产开始日期
      ,[FEndPlanDate]   as 排产结束日期
      ,[FTotalPlanQty]    as 总排产量
      ,[FPlanCount]    as 排产次数
      ,[FAvgPlanQty]   as 日均排产量
  FROM [dbo].[rds_mfg_moSchedule_stat]
  where FPrdNumber >='",FPrdNumber_start,"' and FPrdNumber <='",FPrdNumber_end,"'",sql_FMoNumber)
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

  shiny::observeEvent(input$btnscheduleStat_query,{
    FPrdNumber_start = var_txtscheduleStat_FPrdNumber_start()
    FPrdNumber_end = var_txtscheduleStat_FPrdNumber_end()
    FMoNumber = var_txtscheduleStat_FMoNumber()

    data = schedule_query_stat(token = token
                               ,FPrdNumber_start = FPrdNumber_start
                                 ,FPrdNumber_end = FPrdNumber_end
                                 ,FMoNumber = FMoNumber
                                 )
    tsui::run_dataTable2(id = 'dataviewscheduleStat_query',data = data)
    tsui::run_download_xlsx(id = 'btnscheduleStat_dl',data = data,filename = tsui::file_name('生产订单排产统计表'))

  })

}
