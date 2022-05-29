#' 读取生产模板数据
#'
#' @param file_name 文件名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' schedule_read()
schedule_read <- function(
                          token ='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
                          file_name = "data-raw/生产订单排产信息表.xlsx"
                          ) {
  data <- readxl::read_excel(file_name)
  col_fixed = names(data)[1:13]
  ncount = nrow(data)
  if (ncount >0){
    data = reshape2::melt(data = data,id.vars=col_fixed,variable.name='FPlanDate',value.name='FPlanQty',factorsAsStrings = TRUE)
    data$FPlanDate <- tsdo::excel_date(as.integer(as.character(data$FPlanDate)))
    data = data[complete.cases(data$产品代码), ]
    data = data[complete.cases(data$生产订单编号), ]
    data$产品名称 <- tsdo::na_replace(data$产品名称,'')
    data$模具编码 <- tsdo::na_replace(data$模具编码,'')
    data$包装规格 <- tsdo::na_replace(data$包装规格,'')
    data$工单状态 <- tsdo::na_replace(data$工单状态,'')
    data$工单备注 <- tsdo::na_replace(data$工单备注,'')
    data$工单数量 <- tsdo::na_replace(data$工单数量,0)
    data$产线完成数量 <- tsdo::na_replace(data$产线完成数量,0)
    data$系统入库 <- tsdo::na_replace(data$系统入库,0)
    data$未排产数量 <- tsdo::na_replace(data$未排产数量,0)
    data$已排产数量 <- tsdo::na_replace(data$已排产数量,0)
    data$FPlanQty <- tsdo::na_replace(data$FPlanQty,0)
    data = data[data$FPlanQty>0, ]
    names(data) <- c('FMachineNumber','FPrdNumber', 'FPrdName', 'FMoldNumber', 'FPackSpec', 'FMoNumber', 'FMoStatus', 'FMoNote' ,'FMoQty' ,'FFinishQty', 'FStockInQty', 'FUnScheduleQty' ,'FScheduledQty' ,'FPlanDate', 'FPlanQty')
    conn = tsda::sql_getConn(token = token)

    ncount = nrow(data)
    data$FSeq = 1:ncount
    max_id = tsda::db_maxId2(token = token,FTableName = 'rds_mfg_moSchedule')
    data$FInterId = data$FSeq + max_id
    col_name2 = c('FInterId','FSeq','FMachineNumber','FPrdNumber', 'FPrdName', 'FMoldNumber', 'FPackSpec', 'FMoNumber', 'FMoStatus', 'FMoNote' ,'FMoQty' ,'FFinishQty', 'FStockInQty', 'FUnScheduleQty' ,'FScheduledQty' ,'FPlanDate', 'FPlanQty')
    data = data[ ,col_name2]
    #推入临时表
    tsda::db_writeTable(conn = conn,table_name = 'rds_mfg_moScheduleInput',r_object = data,append = T)
    #备份数据
    sql_bak <- paste0("insert into rds_mfg_moScheduleDel
select a.*  from rds_mfg_moSchedule a
inner join   rds_mfg_moScheduleInput b
on a.FMoNumber =  b.FMoNumber and a.FPlanDate = b.FPlanDate")
    tsda::sql_update(conn,sql_bak)
    #更新内码
    sql_upd <- paste0("update b set  b.FInterId = a.FInterId  from rds_mfg_moSchedule a
inner join   rds_mfg_moScheduleInput b
on a.FMoNumber =  b.FMoNumber and a.FPlanDate = b.FPlanDate")
    tsda::sql_update(conn,sql_upd)
    #删除数据
    sql_del <- paste0("delete a   from rds_mfg_moSchedule a
inner join   rds_mfg_moScheduleInput b
on a.FMoNumber =  b.FMoNumber and a.FPlanDate = b.FPlanDate")
    tsda::sql_update(conn,sql_del)
    #插入数据
    sql_ins <- paste0("insert into rds_mfg_moSchedule select * from rds_mfg_moScheduleInput ")
    tsda::sql_update(conn,sql_ins)
    #清空数据
    tsda::db_truncateTable(token = token,table_name = 'rds_mfg_moScheduleInput')
    #写入正式表


  }
  return(data)


}



#' 查询数据
#'
#' @param token 口令
#' @param FStartPlanDate 计划日期
#' @param FEndPlanDate  计划日期
#' @param FPrdNumber_start 单据编码
#' @param FPrdNumber_end  产品代码
#' @param FMoNumber_start 开始
#' @param FMoNumber_end 结束
#' @param FMachineNumber_start 开始
#' @param FMachineNumber_end 结束
#'
#' @return 返回值
#' @export
#'
#' @examples
#' schedule_query_detail()
schedule_query_detail <- function(token ='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
                           FStartPlanDate ='2022-04-30',
                           FEndPlanDate ='2022-05-20',
                           FPrdNumber_start ='802685',
                           FPrdNumber_end ='802685',
                           FMoNumber_start ='MO202204499',
                           FMoNumber_end ='MO202204499',
                           FMachineNumber_start ='4#',
                           FMachineNumber_end ='4#'
                           ) {
sql_FPrdNumber_start <- tsdo::where_condition(key = 'FPrdNumber',value =FPrdNumber_start,operator = '>=' )
sql_FPrdNumber_end <- tsdo::where_condition(key = 'FPrdNumber',value =FPrdNumber_end,operator = '<=' )
sql_FMoNumber_start <- tsdo::where_condition(key = 'FMoNumber',value=FMoNumber_start,operator = '>=')
sql_FMoNumber_end <- tsdo::where_condition(key = 'FMoNumber',value=FMoNumber_end,operator = '<=')
sql_FMachineNumber_start <- tsdo::where_condition(key = 'FMachineNumber',value=FMachineNumber_start,operator = '>=')
sql_FMachineNumber_end <- tsdo::where_condition(key = 'FMachineNumber',value=FMachineNumber_end,operator = '<=')

sql <- paste0("SELECT
      [FMachineNumber] as 机台编号
      ,[FPrdNumber] as 产品代码
      ,[FPrdName] as 产品名称
      ,[FMoldNumber] as 模具编码
      ,[FPackSpec] as 包装规格
      ,[FMoNumber] as 生产订单编号
      ,[FMoStatus] as 工单状态
      ,[FMoNote] as 工单备注
      ,[FMoQty] as 工单数量
      ,[FFinishQty] as 产线完成数量
      ,[FStockInQty] as 系统入库
      ,[FUnScheduleQty] as 未排产数量
      ,[FScheduledQty] as 已排产数量
      ,[FPlanDate] as 排产日期
      ,[FPlanQty] as 计划产量
  FROM [dbo].[rds_mfg_moSchedule]
where  FPlanDate >='",FStartPlanDate,"' and FPlanDate <='",FEndPlanDate,"'",sql_FPrdNumber_start,sql_FPrdNumber_end,sql_FMoNumber_start,sql_FMoNumber_end,sql_FMachineNumber_start,sql_FMachineNumber_end)
 # print(sql)
  conn = tsda::sql_getConn(token = token)
  data = tsda::sql_select(conn,sql)
  return(data)
}


#' 编码影响的查询函数
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
#' scheduleDetailServer()
scheduleDetailServer <- function(input,output,session,token) {

  #查询按纽
  var_dtscheduleDetail_dates <- tsui::var_dateRange('dtscheduleDetail_dates')
  var_txtscheduleDetail_FPrdNumber_start <- tsui::var_text('txtscheduleDetail_FPrdNumber_start')
  var_txtscheduleDetail_FPrdNumber_end <- tsui::var_text('txtscheduleDetail_FPrdNumber_end')
  var_txtscheduleDetail_FMoNumber_start <- tsui::var_text('txtscheduleDetail_FMoNumber_start')
  var_txtscheduleDetail_FMoNumber_end <- tsui::var_text('txtscheduleDetail_FMoNumber_end')
  var_txtscheduleDetail_FMachineNumber_start <- tsui::var_text('txtscheduleDetail_FMachineNumber_start')
  var_txtscheduleDetail_FMachineNumber_end <- tsui::var_text('txtscheduleDetail_FMachineNumber_end')

  shiny::observeEvent(input$btnscheduleDetail_query,{
    dates = var_dtscheduleDetail_dates()
    FStartPlanDate = dates[1]
    FEndPlanDate = dates[2]
    FPrdNumber_start = var_txtscheduleDetail_FPrdNumber_start()
    FPrdNumber_end = var_txtscheduleDetail_FPrdNumber_end()
    FMoNumber_start = var_txtscheduleDetail_FMoNumber_start()
    FMoNumber_end = var_txtscheduleDetail_FMoNumber_end()
    FMachineNumber_start = var_txtscheduleDetail_FMachineNumber_start()
    FMachineNumber_end = var_txtscheduleDetail_FMachineNumber_end()


    data = schedule_query_detail(token = token,
                                 FStartPlanDate = FStartPlanDate,
                                 FEndPlanDate = FEndPlanDate,
                                 FPrdNumber_start = FPrdNumber_start,
                                 FPrdNumber_end =FPrdNumber_end,
                                 FMoNumber_start =  FMoNumber_start,
                                 FMoNumber_end = FMoNumber_end,
                                 FMachineNumber_start = FMachineNumber_start,
                                 FMachineNumber_end = FMachineNumber_end
                                 )
    tsui::run_dataTable2(id = 'dataviewscheduleDetail_query',data = data)

    tsui::run_download_xlsx(id = 'btnscheduleDetail_dl',data = data,filename = tsui::file_name('生产订单排产信息表'))


  })
  #上传明细数据
  var_filescheduleDetail_upload <- tsui::var_file('filescheduleDetail_upload')
  shiny::observeEvent(input$btnscheduleDetail_upload,{
    file_name = var_filescheduleDetail_upload()
    tsui::file_upload(token=token,file_name = file_name,f = schedule_read,dv_id = 'dataviewscheduleDetail_query')

  })

}



