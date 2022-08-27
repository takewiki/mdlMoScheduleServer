#' 读取生产模板数据
#'
#' @param file_name 文件名
#' @param group 是否集团与多组织
#' @param token  口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' schedule_read()
schedule_read <- function(
                          token ='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
                          file_name = "data-raw/生产订单排产信息表模板_0823.xlsx",
                          group = FALSE
                          ) {

  #设置参数
  if(group){
    #启用多组织
    ncount_fixed = 21
    col_name1 = c('FCompanyName','FWorkShop','FRouteName','FPrdSeries','FMachineNumber','FPrdNumber', 'FPrdName', 'FMoldNumber','FHoleCount', 'FPackSpec','FPrdUnit', 'FMoNumber', 'FMoQty','FPcsQtyPerPkg','FPkgQty','FMoNote','FMoStatus' ,'FFinishQty', 'FStockInQty', 'FUnScheduleQty' ,'FScheduledQty' ,'FPlanDate', 'FPlanQty')
    col_name2 = c('FInterId','FSeq','FCompanyName','FWorkShop','FRouteName','FPrdSeries','FMachineNumber','FPrdNumber', 'FPrdName', 'FMoldNumber','FHoleCount', 'FPackSpec','FPrdUnit', 'FMoNumber', 'FMoQty','FPcsQtyPerPkg','FPkgQty','FMoNote','FMoStatus' ,'FFinishQty', 'FStockInQty', 'FUnScheduleQty' ,'FScheduledQty' ,'FPlanDate', 'FPlanQty')
    table_name ='rds_mfg_moScheduleGroup'
    table_name_del ='rds_mfg_moScheduleGroupDel'
    table_name_input ='rds_mfg_moScheduleGroupInput'
    }else{
    #默认状态，未启用
    ncount_fixed = 13
    col_name1 = c('FMachineNumber','FPrdNumber', 'FPrdName', 'FMoldNumber', 'FPackSpec', 'FMoNumber', 'FMoStatus', 'FMoNote' ,'FMoQty' ,'FFinishQty', 'FStockInQty', 'FUnScheduleQty' ,'FScheduledQty' ,'FPlanDate', 'FPlanQty')
    col_name2 = c('FInterId','FSeq','FMachineNumber','FPrdNumber', 'FPrdName', 'FMoldNumber', 'FPackSpec', 'FMoNumber', 'FMoStatus', 'FMoNote' ,'FMoQty' ,'FFinishQty', 'FStockInQty', 'FUnScheduleQty' ,'FScheduledQty' ,'FPlanDate', 'FPlanQty')
    table_name ='rds_mfg_moSchedule'
    table_name_del ='rds_mfg_moScheduleDel'
    table_name_input ='rds_mfg_moScheduleInput'
    }
  data <- readxl::read_excel(file_name)
  col_fixed = names(data)[1:ncount_fixed]
  ncount = nrow(data)
  if (ncount >0){
    data = reshape2::melt(data = data,id.vars=col_fixed,variable.name='FPlanDate',value.name='FPlanQty',factorsAsStrings = TRUE)
    data$FPlanDate <- tsdo::excel_date(as.integer(as.character(data$FPlanDate)))

    data = data[complete.cases(data$产品代码), ]
    data = data[complete.cases(data$生产订单编号), ]
    data$FPlanQty <- as.numeric(data$FPlanQty)
    data$FPlanQty <- tsdo::na_replace(data$FPlanQty,0)
    data = data[data$FPlanQty>0, ]
    # print(1)
    # View(data)
    if(group){
      data$公司 <- tsdo::na_replace(data$公司,'')
      data$生产车间 <- tsdo::na_replace(data$生产车间,'')
      data$工序 <- tsdo::na_replace(data$工序,'')
      data$产品系列 <- tsdo::na_replace(data$产品系列,'')
    }
    data$产品名称 <- tsdo::na_replace(data$产品名称,'')
    data$模具编码 <- tsdo::na_replace(data$模具编码,'')
    data$生产穴数 <- tsdo::na_replace(data$生产穴数,'')
    data$包装规格 <- tsdo::na_replace(data$包装规格,'')
    data$工单状态 <- tsdo::na_replace(data$工单状态,'')
    data$工单备注 <- tsdo::na_replace(data$工单备注,'')
    data$工单数量 <- tsdo::na_replace(data$工单数量,0)
    data$`产线完成数量（箱）` <- tsdo::na_replace(data$`产线完成数量（箱）`,0)
    data$系统入库 <- tsdo::na_replace(data$系统入库,0)
    data$`未排产数量（箱）` <- tsdo::na_replace(data$`未排产数量（箱）`,0)
    data$`已排产数量（箱）` <- tsdo::na_replace(data$`已排产数量（箱）`,0)
    data$`箱数（PCS换算）` <- data$工单数量
    data$`箱数（PCS换算）`[data$单位 == 'PCS'] <- round(data$工单数量[data$单位 == 'PCS']/data$`每箱PCS数`[data$单位 == 'PCS'],0)



    names(data) <- col_name1
    conn = tsda::sql_getConn(token = token)

    ncount = nrow(data)
    data$FSeq = 1:ncount
    max_id = tsda::db_maxId2(token = token,FTableName = 'rds_mfg_moSchedule')
    data$FInterId = data$FSeq + max_id
    data = data[ ,col_name2]
    # print(2)
    # print(head(data))
    # print(str(data))
    tsda::sql_pushData_InputDel(token = token,
                                data = data,
                                table_name = table_name,
                                table_name_input = table_name_input,
                                table_name_del = table_name_del,
                                keys = c('FMoNumber','FPlanDate') ,
                                FInterId = 'FInterId')




  }
  return(data)


}

#' 读取数据
#'
#' @param token  口令
#' @param file_name 文件名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' schedule_read_single()
schedule_read_single <- function(
  token ='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
  file_name = "data-raw/生产订单排产信息表.xlsx") {

  res <- schedule_read(token = token,file_name = file_name,group = FALSE)
  return(res)

}

#' 读取数据
#'
#' @param token  口令
#' @param file_name 文件名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' schedule_read_group()
schedule_read_group <- function(
  token ='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
  file_name = "data-raw/生产订单排产信息表.xlsx"

) {

  res <- schedule_read(token = token,file_name = file_name,group = TRUE)
  return(res)

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
                           FMachineNumber_end ='4#',
                           FCompanyName ='苏州赛普生物技术有限公司',
                           FWorkShop ='总部生产部',
                           FRouteName ='',
                           FPrdSeries ='',
                           group=FALSE
                           ) {
sql_FPrdNumber_start <- tsdo::where_condition(key = 'FPrdNumber',value =FPrdNumber_start,operator = '>=' )
sql_FPrdNumber_end <- tsdo::where_condition(key = 'FPrdNumber',value =FPrdNumber_end,operator = '<=' )
sql_FMoNumber_start <- tsdo::where_condition(key = 'FMoNumber',value=FMoNumber_start,operator = '>=')
sql_FMoNumber_end <- tsdo::where_condition(key = 'FMoNumber',value=FMoNumber_end,operator = '<=')
sql_FMachineNumber_start <- tsdo::where_condition(key = 'FMachineNumber',value=FMachineNumber_start,operator = '>=')
sql_FMachineNumber_end <- tsdo::where_condition(key = 'FMachineNumber',value=FMachineNumber_end,operator = '<=')
sql_FCompanyName <- tsdo::where_condition(key = 'FCompanyName',value=FCompanyName,operator = '=')
sql_FWorkShop <- tsdo::where_condition(key = 'FWorkShop',value=FWorkShop,operator = '=')
sql_FRouteName <- tsdo::where_condition(key = 'FRouteName',value=FRouteName,operator = '=')
sql_FPrdSeries <- tsdo::where_condition(key = 'FPrdSeries',value=FPrdSeries,operator = '=')

if(group){
  sql <- paste0("SELECT
      [FCompanyName] as 公司
      ,[FWorkShop] as 生产车间
      ,[FRouteName] as 工序
      ,[FPrdSeries]  as 产品系列
      ,[FMachineNumber] as 机台编号
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
  FROM [dbo].[rds_mfg_moScheduleGroup]
  where  FPlanDate >='",FStartPlanDate,"' and FPlanDate <='",FEndPlanDate,"'",sql_FPrdNumber_start,sql_FPrdNumber_end,sql_FMoNumber_start,sql_FMoNumber_end,sql_FMachineNumber_start,sql_FMachineNumber_end,sql_FCompanyName,sql_FWorkShop,sql_FRouteName,sql_FPrdSeries)
}else{
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
}

  print(sql)
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

  var_txtscheduleDetail_FCompanyName <- tsui::var_text('txtscheduleDetail_FCompanyName')
  var_txtscheduleDetail_FWorkShop <- tsui::var_text('txtscheduleDetail_FWorkShop')

  var_txtscheduleDetail_FRouteName <- tsui::var_text('txtscheduleDetail_FRouteName')
  var_txtscheduleDetail_FPrdSeries <- tsui::var_text('txtscheduleDetail_FPrdSeries')




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
    #集团公司新增内容
    FCompanyName = var_txtscheduleDetail_FCompanyName()
    FWorkShop =  var_txtscheduleDetail_FWorkShop()
    FRouteName = var_txtscheduleDetail_FRouteName()
    FPrdSeries = var_txtscheduleDetail_FPrdSeries()


    data = schedule_query_detail(token = token,
                                 FStartPlanDate = FStartPlanDate,
                                 FEndPlanDate = FEndPlanDate,
                                 FPrdNumber_start = FPrdNumber_start,
                                 FPrdNumber_end =FPrdNumber_end,
                                 FMoNumber_start =  FMoNumber_start,
                                 FMoNumber_end = FMoNumber_end,
                                 FMachineNumber_start = FMachineNumber_start,
                                 FMachineNumber_end = FMachineNumber_end,
                                 FCompanyName =FCompanyName,
                                 FWorkShop =FWorkShop,
                                 FRouteName =FRouteName,
                                 FPrdSeries =FPrdSeries,
                                 group = TRUE
                                 )
    tsui::run_dataTable2(id = 'dataviewscheduleDetail_query',data = data)

    tsui::run_download_xlsx(id = 'btnscheduleDetail_dl',data = data,filename = tsui::file_name('生产订单排产信息表'))


  })
  #上传明细数据
  var_filescheduleDetail_upload <- tsui::var_file('filescheduleDetail_upload')
  shiny::observeEvent(input$btnscheduleDetail_upload,{
    file_name = var_filescheduleDetail_upload()
    tsui::file_upload(token=token,file_name = file_name,f = schedule_read_group,dv_id = 'dataviewscheduleDetail_query')

  })

}



