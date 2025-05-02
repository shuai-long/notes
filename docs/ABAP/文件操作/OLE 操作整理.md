[TOC]



# 一. OLE 操作整理 #

## 定义 ##

```abap
DATA: EXCEL_OBJ                    TYPE OLE2_OBJECT,  " excel
      BOOK_OBJ                     TYPE OLE2_OBJECT,  " workbook(工作簿)
      SHEET_OBJ                    TYPE OLE2_OBJECT,  " sheet(页)
      CELL_OBJ                     TYPE OLE2_OBJECT.  " 单元格
```

## 方法说明 ##

```abap
CREATE OBJECT name_obj 'app'."创建 APP 应用类的一个对象 obj_name 实例
SET PROPERTY OF name_obj 'XXX' = f ."设置对象 name_obj 属性 xxx 为值 f
GET PROPERTY OF name_obj 'xxx' = f ."获取对象 name_obj 属性 xxx 的值赋给 f
CALL METHOD OF name_obj 'xxx' = f  "由f来接收返回值
  EXPORTING #1 = f1. "调用 name_obj 的方法 xxx 传入参数 f1…fn
FREE OBJECT name_obj . "释放.
```

## 使用 ##

1. **创建对象**

   ```abap
   CREATE OBJECT EXCEL_OBJ 'EXCEL.APPLICATION'. " 启动 excel 应用
   SET PROPERTY OF EXCEL_OBJ 'VISIBLE' = 1." 使 excel 可见（ 1：可见  0：后台运行不可见）
   SET PROPERTY OF EXCEL_OBJ 'SHEETSINNEWWORKBOOK' = 1."设置 Microsoft Excel 软件打开时，自动插入到新工作簿中的工作表数目（即初始 sheet 数目，默认名字依次为 Sheet1、Sheet2.....）
   ```

2. **创建 workbook**

   ```abap
   "方法 1
   CALL METHOD OF EXCEL_OBJ 'WORKBOOKS' = BOOK_OBJ.
   
   "方法2
   "由于Workbooks同时为属性，所以可以使用下面语句代替上面语句
   * GET PROPERTY OF EXCEL 'Workbooks' = WORKBOOK_OBJ .
   CALL METHOD OF BOOK_OBJ 'ADD'.
   CALL METHOD OF BOOK_OBJ 'OPEN'EXPORTING #1 = 'c:\***.xlsx'." 打开文件
   ```

3. **sheet 操作**

   ```abap
   " 添加 sheet 页
   CALL METHOD OF EXCEL_OBJ 'SHEETS' = SHEET_OBJ.
   CALL METHOD OF SHEET_OBJ 'ADD'.
   
   SET PROPERTY OF SHEET_OBJ 'NAME' = 'NAME'."sheet重命名
   
   " 切换 sheet 页
   CALL METHOD OF EXCEL_OBJ 'WORKSHEETS' = SHEET_OBJ EXPORTING #1 = 'sheet3'."sheet 页的名称 
   CALL METHOD OF SHEET_OBJ 'ACTIVATE'.
   ```

4. **单元格操作**

   ```abap
   DATA: LV_ROW   TYPE I,
   	  LV_COL   TYPE I,
   	  LV_VALUE TYPE STRING.
   	  
   CaLL METHOD OF EXCEL_OBJ 'CELLS' = CELL EXPORTING #1 = row #2 = col.  "行 ,列
   SET  PROPERTY OF CELL_OBJ 'VALUE' =  xxxx. "值
   ```

5. **执行宏**

   ```abap
   CALL METHOD OF EXCEL_OBJ 'RUN' EXPORTING #1 = 'ZRUNM'.
   ```

6. **保存与退出**

   ```abap
   GET PROPERTY OF EXCEL_OBJ 'ACTIVESHEET' = SHEET.         "激活工作簿
   GET PROPERTY OF EXCEL_OBJ 'ACTIVEWORKBOOK' = WORKBOOK.   "激活工作区
   CALL METHOD OF BOOK_OBJ 'SAVE'. "保存
   "CALL METHOD OF BOOK_OBJ 'SAVEAS'EXPORTING #1 = 'c:\1.xls' #2 = 1. "另存为
   CALL METHOD OF BOOK_OBJ 'CLOSE'. "关闭工作区
   CALL METHOD OF EXCEL_OBJ 'QUIT'. "退出excel
   ```

7. **释放资源**

   ```abap
   FREE OBJECT SHEET_OBJ.    " 释放 sheet 对象
   FREE OBJECT WORKBOOK_OBJ. " 释放 workbook 对象
   FREE OBJECT EXCEL_OBJ.    " 释放 excel 对象
   ```

# word 另存为 PDF

```ABAP
  DATA: LV_FULLPATH_PDF TYPE STRING. "文件路径
  LV_FULLPATH_PDF = |{ LV_FULLPATH }\\{ 'TEST.PDF' }|.

  DATA:LR_WORD TYPE OLE2_OBJECT,
       LR_DOCU TYPE OLE2_OBJECT.

  CREATE OBJECT LR_WORD 'WORD.APPLICATION' .

  CALL METHOD OF LR_WORD 'Documents' = LR_DOCU.

  CALL METHOD OF LR_DOCU 'Open' = LR_DOCU
    EXPORTING
      #1 = LV_FULLPATH_WORD. "word文件 的路径

  CALL METHOD OF LR_DOCU 'ExportAsFixedFormat' = LR_DOCU
    EXPORTING
       #1 = LV_FULLPATH_PDF
       #2 = '17'.

  CALL METHOD OF LR_WORD 'QUIT' .
```



## 例子 ##

```abap
REPORT ZZJX_TEST08 MESSAGE-ID ZZJXMSG.

DATA: C_EXPORT_FILENAME_XLS   TYPE STRING VALUE 'ZJX_TEST.XLSX', "导出模板默认文件名 '数据导入模板'
      C_OBJID_XLS             TYPE WWWDATATAB-OBJID VALUE 'ZZJX_TEST08'.   "存放模板的对象id

DATA:  LO_OBJDATA                 LIKE WWWDATATAB,                                "Excel模板对象
       LS_DESTINATION             LIKE RLGRAP-FILENAME,                           "下载保存的目标路径
       LC_PATH                    TYPE STRING,        							  "存储路径
       LC_FULLPATH                TYPE STRING,                                    "文件完整路径
       LI_RC                      LIKE SY-SUBRC.                                  "返回值

DATA: EXCEL_OBJ                    TYPE OLE2_OBJECT,
      BOOK_OBJ                     TYPE OLE2_OBJECT,
      SHEET_OBJ                    TYPE OLE2_OBJECT,
      CELL_OBJ                     TYPE OLE2_OBJECT.
      
LOAD-OF-PROGRAM.
  PARAMETERS: P_FILE TYPE RLGRAP-FILENAME OBLIGATORY.

INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE
    EXPORTING
      WINDOW_TITLE         = 'File Directory'
      INITIAL_FOLDER       =  'C:\Users\BRIGHT-SH-002\Desktop'
    CHANGING
      SELECTED_FOLDER      = LC_PATH
*  EXCEPTIONS
*    CNTL_ERROR           = 1
*    ERROR_NO_GUI         = 2
*    NOT_SUPPORTED_BY_GUI = 3
*    OTHERS               = 4
          .
  P_FILE = LC_PATH.

START-OF-SELECTION.
*第一步: 下载Excel到本地
  PERFORM DOWNLOAD_XLS_TEMPLATE.

*第二步：打开Excel文档
  PERFORM OPEN_EXCEL.

*第三步:   写入数据
  PERFORM WRITE_EXCEL.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_XLS_TEMPLATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOWNLOAD_XLS_TEMPLATE .
* 获取保存路径
*  LC_PATH = P_FILE.

*  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_DESKTOP_DIRECTORY
*    CHANGING
*      DESKTOP_DIRECTORY    = LC_PATH
**  EXCEPTIONS
**    CNTL_ERROR           = 1
**    ERROR_NO_GUI         = 2
**    NOT_SUPPORTED_BY_GUI = 3
**    OTHERS               = 4
*          .
*  IF LC_PATH IS INITIAL.
*    LC_PATH = 'C:\Users\BRIGHT-SH-002\Desktop'.
*  ENDIF.

  CONCATENATE LC_PATH '\' C_EXPORT_FILENAME_XLS INTO LC_FULLPATH.
*  检查模板是否存在
  SELECT SINGLE RELID OBJID FROM WWWDATA INTO CORRESPONDING FIELDS OF LO_OBJDATA
 WHERE SRTF2 = 0 AND RELID = 'MI' AND OBJID = C_OBJID_XLS.
  IF SY-SUBRC NE 0 OR LO_OBJDATA-OBJID EQ SPACE.
    MESSAGE E000(ZZJXMSG) WITH C_EXPORT_FILENAME_XLS.
  ENDIF.

*  下载模板
  LS_DESTINATION = LC_FULLPATH .
  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
    EXPORTING
      KEY         = LO_OBJDATA
      DESTINATION = LS_DESTINATION
    IMPORTING
      RC          = LI_RC
* CHANGING
*   TEMP              =
  .
  IF LI_RC NE 0.
    MESSAGE E001(ZZJXMSG) WITH C_EXPORT_FILENAME_XLS.
  ENDIF.

ENDFORM.                    " DOWNLOAD_XLS_TEMPLATE
*&---------------------------------------------------------------------*
*&      Form  OPEN_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OPEN_EXCEL .
  CREATE OBJECT EXCEL_OBJ 'EXCEL.APPLICATION'.
  IF SY-SUBRC NE 0.
    MESSAGE 'EXCEL创建错误' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  CALL METHOD OF
      EXCEL_OBJ
      'WORKBOOKS' = BOOK_OBJ.
  SET PROPERTY OF EXCEL_OBJ 'VISIBLE' = 0.
*  SET PROPERTY OF EXCEL_OBJ 'SheetInNewWorkbook' = 1.

* 打开excel文件，（新建使用：call method of book_obj 'Add' = sheet_obj）
  CALL METHOD OF
      BOOK_OBJ
      'Open'   = SHEET_OBJ
    EXPORTING
      #1       = LS_DESTINATION.

  CALL METHOD OF
      EXCEL_OBJ
      'Sheets'  = SHEET_OBJ"切换sheets
    EXPORTING
      #1        = 1.

  CALL METHOD OF
      SHEET_OBJ
      'Select'.

*  FREE OBJECT SHEET_OBJ."ok
  CALL METHOD OF SHEET_OBJ 'ACTIVATE'."激活
  SET PROPERTY OF SHEET_OBJ 'NAME' = '库存物料表'.
ENDFORM.                    " OPEN_EXCEL
*&---------------------------------------------------------------------*
*&      Form  WRITE_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_EXCEL .
  DATA:BEGIN OF IT_DATA OCCURS 10,
       MANDT TYPE MANDT,
       MATNR TYPE MATNR,
       MAKTX TYPE MAKT-MAKTX,
       WERKS TYPE WERKS_D,
       NAME1 TYPE NAME1,
       LGORT TYPE LGORT_D,
       LGOBE TYPE T001L-LGOBE,
       LABST TYPE LABST,
       MEINS TYPE MEINS,
    END OF IT_DATA.
  DATA LV_ROWS TYPE I.
  RANGES: LR_MATNR FOR MARD-MATNR.
  CLEAR LR_MATNR.
  LR_MATNR-SIGN = 'I'.
  LR_MATNR-OPTION = 'BT'.
  LR_MATNR-LOW = '000000001000000030'.
  LR_MATNR-HIGH = '000000001000000197'.
  APPEND LR_MATNR.

  SELECT A~MATNR B~MAKTX A~WERKS C~NAME1 A~LGORT D~LGOBE A~LABST E~MEINS
    INTO CORRESPONDING FIELDS OF TABLE IT_DATA
    FROM MARD AS A
    INNER JOIN MAKT  AS B ON B~MATNR = A~MATNR AND B~SPRAS = 'JA'
    INNER JOIN T001W AS C ON C~WERKS = A~WERKS
    INNER JOIN T001L AS D ON D~LGORT = A~LGORT
    INNER JOIN MARA  AS E ON E~MATNR = A~MATNR
    UP TO 5 ROWS
    WHERE A~MATNR IN LR_MATNR.


  FIELD-SYMBOLS <FS1> LIKE LINE OF IT_DATA.
  LOOP AT IT_DATA ASSIGNING <FS1> .
    <FS1>-MANDT = SY-MANDT.
  ENDLOOP.

*  PERFORM EXCEL_ROW_INSERT USING SHEET_OBJ 3 1.
* 输出Excel表头,自定义格式的列等
  PERFORM FILL_CELL USING 3 1 1 'MANDT'.
  PERFORM FILL_CELL USING 3 2 1 'MATNR'.
  PERFORM FILL_CELL USING 3 3 1 'MAKTX'.
  PERFORM FILL_CELL USING 3 4 1 'WERKS'.
  PERFORM FILL_CELL USING 3 5 1 'NAME1'.
  PERFORM FILL_CELL USING 3 6 1 'LGORT'.
  PERFORM FILL_CELL USING 3 7 1 'LGOBE'.
  PERFORM FILL_CELL USING 3 8 1 'LABST'.
  PERFORM FILL_CELL USING 3 9 1 'MEINS'.
  CLEAR LV_ROWS.
  LOOP AT IT_DATA.
    LV_ROWS = SY-TABIX + 3.
*    PERFORM EXCEL_ROW_INSERT USING SHEET_OBJ LV_ROWS 1.
    PERFORM FILL_CELL USING: LV_ROWS 1 0 IT_DATA-MANDT,
                             LV_ROWS 2 1 IT_DATA-MATNR,
                             LV_ROWS 3 0 IT_DATA-MAKTX,
                             LV_ROWS 4 0 IT_DATA-WERKS,
                             LV_ROWS 5 0 IT_DATA-NAME1,
                             LV_ROWS 6 0 IT_DATA-LGORT,
                             LV_ROWS 7 0 IT_DATA-LGOBE,
                             LV_ROWS 8 0 IT_DATA-LABST,
                             LV_ROWS 9 0 IT_DATA-MEINS.
  ENDLOOP.

  FREE OBJECT CELL_OBJ.
  GET PROPERTY OF EXCEL_OBJ 'ActiveSheet' = SHEET_OBJ. "获取活动SHEET
  FREE OBJECT SHEET_OBJ.
  GET PROPERTY OF EXCEL_OBJ 'ACTIVEWORKBOOK' = BOOK_OBJ."激活工作区
  "save
*  CALL METHOD OF
*      BOOK_OBJ
*      'SAVEAS'
*    EXPORTING
*      #1       = LC_FULLPATH
*      #2       = 1.
  CALL METHOD OF
      BOOK_OBJ
      'SAVE'.
*   SET PROPERTY OF excel 'Visible' = 1.  "是否显示EXCEL 此处显示不退出
  CALL METHOD OF
      BOOK_OBJ
      'CLOSE'.
  CALL METHOD OF
      EXCEL_OBJ
      'QUIT'.

  FREE OBJECT BOOK_OBJ.
  FREE OBJECT EXCEL_OBJ.
ENDFORM.                    " WRITE_EXCEL
*&---------------------------------------------------------------------*
*&      Form  filL_cell
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->row  行
*      -->col  列
*      -->bold 加粗
*      -->val  填充值
*----------------------------------------------------------------------*
FORM FILL_CELL  USING   VALUE(ROW)
                        VALUE(COL)
                        VALUE(BOLD)
                        VALUE(VAL).
  CALL METHOD OF
      EXCEL_OBJ
      'CELLS'   = CELL_OBJ
    EXPORTING
      #1        = ROW
      #2        = COL.
*  SET PROPERTY OF CELL_OBJ 'BOLD' = BOLD.
  SET PROPERTY OF CELL_OBJ 'VALUE' = VAL.
*  FREE OBJECT CELL_OBJ.
ENDFORM.                    " FIL_CELL
*&---------------------------------------------------------------------*
*&      Form  EXCEL_ROW_INSERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SHEET  text
*      -->P_TAB  text
*      -->P_1      text
*----------------------------------------------------------------------*
FORM EXCEL_ROW_INSERT  USING    LCOBJ_SHEET
                                LC_ROW
                                LC_COUNT.
  DATA LC_RANGE TYPE OLE2_OBJECT.
  DATA H_BORDERS  TYPE OLE2_OBJECT.
  DO LC_COUNT TIMES.
    CALL METHOD OF
        LCOBJ_SHEET
        'Rows'      = LC_RANGE
      EXPORTING
        #1          = 1.
    CALL METHOD OF LC_RANGE 'Copy'.  "COPY第6行插入一个新行
    CALL METHOD OF
        LCOBJ_SHEET
        'Rows'      = LC_RANGE
      EXPORTING
        #1          = LC_ROW.
    CALL METHOD OF
        LC_RANGE
        'Insert'.
    CALL METHOD OF LC_RANGE 'ClearContents'. "是否需要清空Cell
  ENDDO.
ENDFORM.                    " EXCEL_ROW_INSERT
```



相关 FORM 及变量定义

```abap
DATA: O_APPLICATION TYPE OLE2_OBJECT,
      O_WORKBOOK    TYPE OLE2_OBJECT,
      O_SHEET       TYPE OLE2_OBJECT,
      O_COLUMNS     TYPE OLE2_OBJECT,
      O_ROWS        TYPE OLE2_OBJECT,
      O_RANGE       TYPE OLE2_OBJECT,
      O_FONT        TYPE OLE2_OBJECT,
      O_CELL        TYPE OLE2_OBJECT,
      O_SELECT      TYPE OLE2_OBJECT,
      O_PICTURES    TYPE OLE2_OBJECT,
      O_SHAPES      TYPE OLE2_OBJECT,
      O_SHAPERANGE  TYPE OLE2_OBJECT,
      O_BORDERS     TYPE OLE2_OBJECT.
      
"使用文件路径打开 excel      
PERFORM OPEN_EXCEL_SHEET  USING  LV_DESTINATION.
"EXCEL写值 可以在该 form 中选择 sheet 页
PERFORM ADD_DATA_INTO_EXCEL USING LV_ROW LV_COL LV_STR.
"关闭 excel
PERFORM CLOSE_EXCEL.
"释放 excel
PERFORM FREE_OBJECT.      

FORM OPEN_EXCEL_SHEET  USING PV_DESTINATION LIKE RLGRAP-FILENAME.

  CREATE OBJECT O_APPLICATION 'EXCEL.APPLICATION'.
  SET PROPERTY OF O_APPLICATION 'Visible' = 0."1显示EXCEL,0不显示EXCEL

  CALL METHOD OF O_APPLICATION
      'Workbooks' = O_WORKBOOK.
  CALL METHOD OF O_WORKBOOK
    'Open'
    EXPORTING
      #1 = PV_DESTINATION
      #2 = 0
      #3 = 0.             "只读参数
  IF SY-SUBRC <> 0.
    PERFORM FREE_OBJECT.
    MESSAGE E001(00) WITH '打开模板文件失败,代码表无法写入!'.
  ENDIF.

ENDFORM.

FORM FREE_OBJECT .

  FREE OBJECT: O_FONT,
               O_BORDERS,
               O_RANGE,
               O_SELECT,
               O_SHAPERANGE,
               O_PICTURES,
               O_COLUMNS,
               O_ROWS,
               O_CELL,
               O_SHEET,
               O_WORKBOOK,
               O_APPLICATION.

ENDFORM.

FORM CLOSE_EXCEL .
  GET PROPERTY OF O_APPLICATION 'ACTIVESHEET' = O_SHEET. " 获取活动SHEET
  GET PROPERTY OF O_APPLICATION 'ActiveWorkbook' =  O_WORKBOOK. "必须加上,否则无法保存excel.
  CALL METHOD OF O_WORKBOOK 'Save'.
  CALL METHOD OF O_WORKBOOK 'Close'
    EXPORTING
      #1 = 0.
  CALL METHOD OF O_APPLICATION 'Quit'.
ENDFORM.


FORM ADD_DATA_INTO_EXCEL USING PV_ROW TYPE I
                               PV_COL TYPE I
                               PV_STR TYPE STRING.

  CALL METHOD OF
    O_APPLICATION
    'Worksheets' = O_SHEET
   EXPORTING
     #1           = '代码表'."2."指定第几个SHEET，直接写数字

  CALL METHOD OF O_SHEET 'Activate'.
*  GET PROPERTY OF o_APPLICATION 'ActiveWorkbook' =  o_WORKBOOK. "必须加上,否则无法保存excel.

  PERFORM FILL_CELL USING  PV_ROW PV_COL 0 0 PV_STR.   "给excel单元格赋值


ENDFORM.

FORM FILL_CELL  USING I_ROW TYPE I
                      I_COL TYPE I
                      BOLD TYPE ANY
                      I_OFFSET TYPE ANY
                      P_VALUE TYPE ANY.

  CALL METHOD OF O_APPLICATION 'CELLS' = O_CELL
    EXPORTING
      #1 = I_ROW
      #2 = I_COL.

  SET PROPERTY OF O_CELL 'VALUE' = P_VALUE.

ENDFORM.
```

