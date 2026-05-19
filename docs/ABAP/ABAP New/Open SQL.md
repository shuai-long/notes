# Open SQL 常用函数总结

> 示例采用 ABAP 7.40+ 常见写法：`@` 表示 ABAP host variable，`@DATA(...)` 表示内联声明。不同 NetWeaver / ABAP Platform 版本支持的函数和参数可能不同，项目中以系统语法检查和 F1 文档为准。

---

## 聚合函数

- **`COUNT( * )`**
  - 函数作用：统计查询结果集或分组中的行数。
  - 参数意义：
    - [`*`](#param-star)：表示统计行数，不指定具体字段。
    - `COUNT( * )` 会统计满足 `WHERE` 条件的所有行。
    - 如果配合 `GROUP BY`，则统计每个分组中的行数。
  - 使用示例：统计物料主数据总行数。

    ```abap
    SELECT COUNT( * )
      FROM mara
      INTO @DATA(lv_count).
    ```

  - 使用示例：按物料类型统计数量。

    ```abap
    SELECT mtart,
           COUNT( * ) AS mat_count
      FROM mara
      GROUP BY mtart
      INTO TABLE @DATA(lt_count_by_type).
    ```

- **`COUNT( DISTINCT col )`**
  - 函数作用：统计某字段去重后的数量。
  - 参数意义：
    - [`DISTINCT`](#param-distinct)：表示先排除重复值，再统计剩余值的数量。
    - [`col`](#param-col)：需要统计的字段。
    - `COUNT( DISTINCT matkl )` 表示统计不同的物料组数量，而不是统计物料行数。
  - 使用示例：统计物料主数据中不同物料组的数量。

    ```abap
    SELECT COUNT( DISTINCT matkl )
      FROM mara
      INTO @DATA(lv_matkl_count).
    ```

- **`SUM( col )`**
  - 函数作用：对数值字段求和。
  - 参数意义：
    - [`col`](#param-col)：需要求和的数值字段，例如数量、库存、金额等。
    - [`col`](#param-col) 通常应为数值类型，例如 `INT`、`DEC`、`CURR`、`QUAN` 等。
    - 如果字段可能存在数据库 `NULL`，聚合时通常会忽略 `NULL`。
  - 使用示例：按物料汇总库存数量。

    ```abap
    SELECT matnr,
           SUM( labst ) AS total_stock
      FROM mard
      GROUP BY matnr
      INTO TABLE @DATA(lt_stock).
    ```

- **`AVG( col )`**
  - 函数作用：对数值字段求平均值。
  - 参数意义：
    - [`col`](#param-col)：需要求平均值的数值字段。
    - 平均值的计算逻辑是“总和 / 参与计算的行数”。
    - 如果字段可能存在数据库 `NULL`，`NULL` 通常不参与平均值计算。
  - 使用示例：按物料统计采购订单行平均净价。

    ```abap
    SELECT matnr,
           AVG( netpr ) AS avg_price
      FROM ekpo
      GROUP BY matnr
      INTO TABLE @DATA(lt_avg_price).
    ```

- **`MAX( col )`**
  - 函数作用：获取字段或表达式的最大值。
  - 参数意义：
    - [`col`](#param-col)：需要比较大小的字段或 SQL 表达式。
    - 对数字字段，比较数值大小。
    - 对日期字段，通常可用于取最大日期。
    - 对字符字段，通常按数据库排序规则比较。
  - 使用示例：获取每个采购订单的最大行项目号。

    ```abap
    SELECT ebeln,
           MAX( ebelp ) AS max_item
      FROM ekpo
      GROUP BY ebeln
      INTO TABLE @DATA(lt_max_item).
    ```

- **`MIN( col )`**
  - 函数作用：获取字段或表达式的最小值。
  - 参数意义：
    - [`col`](#param-col)：需要比较大小的字段或 SQL 表达式。
    - 对日期字段，常用于取最早日期。
    - 对数量或金额字段，常用于取最小值。
  - 使用示例：获取每个采购订单的最小行项目号。

    ```abap
    SELECT ebeln,
           MIN( ebelp ) AS min_item
      FROM ekpo
      GROUP BY ebeln
      INTO TABLE @DATA(lt_min_item).
    ```

---

## 字符串函数

- **`CONCAT( arg1, arg2 )`**
  - 函数作用：拼接两个字符串。
  - 参数意义：
    - [`arg1`](#param-arg1)：第一个字符串字段、字面量或表达式，作为拼接结果的前半部分。
    - [`arg2`](#param-arg2)：第二个字符串字段、字面量或表达式，作为拼接结果的后半部分。
    - `CONCAT` 只能直接拼接两个参数；如果要拼接三个及以上内容，需要嵌套使用。
  - 使用示例：拼接物料号和物料类型。

    ```abap
    SELECT matnr,
           CONCAT( matnr, mtart ) AS mat_type
      FROM mara
      INTO TABLE @DATA(lt_data)
      UP TO 10 ROWS.
    ```

  - 使用示例：拼接三个内容，需要嵌套。

    ```abap
    SELECT matnr,
           CONCAT( CONCAT( matnr, '-' ), mtart ) AS mat_type_text
      FROM mara
      INTO TABLE @DATA(lt_data)
      UP TO 10 ROWS.
    ```

- **`CONCAT_WITH_SPACE( arg1, arg2, spaces )`**
  - 函数作用：拼接两个字符串，并在中间插入指定数量的空格。
  - 参数意义：
    - [`arg1`](#param-arg1)：第一个字符串字段、字面量或表达式。
    - [`arg2`](#param-arg2)：第二个字符串字段、字面量或表达式。
    - [`spaces`](#param-spaces)：插入在 [`arg1`](#param-arg1) 和 [`arg2`](#param-arg2) 中间的空格数量。
    - [`spaces`](#param-spaces) 为 `1` 时，中间插入 `1` 个空格。
    - [`spaces`](#param-spaces) 为 `3` 时，中间插入 `3` 个空格。
  - 直观理解：
    - `CONCAT_WITH_SPACE( 'A', 'B', 1 )` 的结果类似 `'A B'`。
    - `CONCAT_WITH_SPACE( 'A', 'B', 3 )` 的结果类似 `'A   B'`。
  - 使用示例：物料号和物料类型中间加一个空格。

    ```abap
    SELECT matnr,
           CONCAT_WITH_SPACE( matnr, mtart, 1 ) AS mat_text
      FROM mara
      INTO TABLE @DATA(lt_data)
      UP TO 10 ROWS.
    ```

- **`SUBSTRING( arg, pos, len )`**
  - 函数作用：从字符串中截取指定长度的子串。
  - 参数意义：
    - [`arg`](#param-arg)：原字符串字段、字面量或表达式。
    - [`pos`](#param-pos)：截取起始位置，从 `1` 开始计数。
    - [`len`](#param-len)：要截取的字符长度，不是结束位置。
    - [`pos`](#param-pos) 为 `1` 时，表示从第 `1` 个字符开始取。
    - [`pos`](#param-pos) 为 `2` 时，表示从第 `2` 个字符开始取。
    - [`len`](#param-len) 为 `1` 时，表示取 `1` 个字符。
    - [`len`](#param-len) 为 `2` 时，表示取 `2` 个字符。
  - 重点示例：
    - 对字符串 `'abc'`，`SUBSTRING( 'abc', 2, 2 )` 表示从第 `2` 位的 `b` 开始取 `2` 个字符，所以结果是 `'bc'`。
    - 如果只想取 `b`，应使用 `SUBSTRING( 'abc', 2, 1 )`。
    - 如果想取 `ab`，应使用 `SUBSTRING( 'abc', 1, 2 )`。
  - 边界要求：
    - [`pos`](#param-pos) 必须落在字符串范围内。
    - [`len`](#param-len) 必须让截取结果仍在字符串范围内。
    - 可以用公式理解：`pos + len - 1` 不能超过字符串长度。
    - 例如 `'abc'` 长度是 `3`，`SUBSTRING( 'abc', 3, 2 )` 想从第 `3` 位开始取 `2` 位，会越界。
  - 使用示例：取物料号前 `4` 位。

    ```abap
    SELECT matnr,
           SUBSTRING( matnr, 1, 4 ) AS mat_prefix
      FROM mara
      INTO TABLE @DATA(lt_data)
      UP TO 10 ROWS.
    ```

  - 使用示例：判断物料号第 `2` 位开始的 `2` 个字符是否等于 `BC`。

    ```abap
    SELECT matnr
      FROM mara
      WHERE SUBSTRING( matnr, 2, 2 ) = 'BC'
      INTO TABLE @DATA(lt_matnr).
    ```

  - 使用示例：判断物料号中任意位置是否包含 `BC`。这种场景更推荐使用 `INSTR`。

    ```abap
    SELECT matnr
      FROM mara
      WHERE INSTR( matnr, 'BC' ) > 0
      INTO TABLE @DATA(lt_matnr).
    ```

- **`LEFT( arg, len )`**
  - 函数作用：从字符串左侧截取指定长度的字符。
  - 参数意义：
    - [`arg`](#param-arg)：原字符串字段、字面量或表达式。
    - [`len`](#param-len)：从左侧开始要取出的字符数量。
    - `LEFT( 'abc', 2 )` 的结果类似 `'ab'`。
    - [`len`](#param-len) 不能大于字符串实际可处理长度，否则可能产生语法或运行时问题，具体以系统版本为准。
  - 使用示例：取物料号左侧 `4` 位。

    ```abap
    SELECT matnr,
           LEFT( matnr, 4 ) AS left_part
      FROM mara
      INTO TABLE @DATA(lt_data)
      UP TO 10 ROWS.
    ```

- **`RIGHT( arg, len )`**
  - 函数作用：从字符串右侧截取指定长度的字符。
  - 参数意义：
    - [`arg`](#param-arg)：原字符串字段、字面量或表达式。
    - [`len`](#param-len)：从右侧开始要取出的字符数量。
    - `RIGHT( 'abc', 2 )` 的结果类似 `'bc'`。
    - [`len`](#param-len) 为 `1` 时，表示只取最右侧 `1` 个字符。
  - 使用示例：取物料号右侧 `4` 位。

    ```abap
    SELECT matnr,
           RIGHT( matnr, 4 ) AS right_part
      FROM mara
      INTO TABLE @DATA(lt_data)
      UP TO 10 ROWS.
    ```

- **`LENGTH( arg )`**
  - 函数作用：返回字符串长度。
  - 参数意义：
    - [`arg`](#param-arg)：需要计算长度的字符串字段、字面量或表达式。
    - 对字符型字段，部分版本或数据库处理时会忽略尾部空格。
    - 如果需要严格处理尾部空格，建议结合系统版本和实际数据验证。
  - 使用示例：获取物料号长度。

    ```abap
    SELECT matnr,
           LENGTH( matnr ) AS mat_len
      FROM mara
      INTO TABLE @DATA(lt_data)
      UP TO 10 ROWS.
    ```

- **`UPPER( arg )`**
  - 函数作用：将字符串转换为大写。
  - 参数意义：
    - [`arg`](#param-arg)：需要转换为大写的字符串字段、字面量或表达式。
    - 常用于大小写不敏感的查询辅助处理。
  - 使用示例：把物料描述转成大写输出。

    ```abap
    SELECT maktx,
           UPPER( maktx ) AS maktx_upper
      FROM makt
      INTO TABLE @DATA(lt_text)
      UP TO 10 ROWS.
    ```

- **`LOWER( arg )`**
  - 函数作用：将字符串转换为小写。
  - 参数意义：
    - [`arg`](#param-arg)：需要转换为小写的字符串字段、字面量或表达式。
  - 使用示例：把物料描述转成小写输出。

    ```abap
    SELECT maktx,
           LOWER( maktx ) AS maktx_lower
      FROM makt
      INTO TABLE @DATA(lt_text)
      UP TO 10 ROWS.
    ```

- **`INSTR( arg, sub )`**
  - 函数作用：查找子串第一次出现的位置。如果找不到，通常返回 `0`。
  - 参数意义：
    - [`arg`](#param-arg)：原字符串字段、字面量或表达式。
    - [`sub`](#param-sub)：需要查找的子串。
    - 返回值是 [`sub`](#param-sub) 在 [`arg`](#param-arg) 中第一次出现的位置，位置通常从 `1` 开始。
    - `INSTR( 'abc', 'bc' )` 的结果是 `2`。
    - `INSTR( 'abc', 'd' )` 的结果是 `0`。
    - 该函数通常区分大小写，例如 `'A'` 和 `'a'` 可能被视为不同字符。
  - 使用示例：查询物料号中包含 `A` 的物料。

    ```abap
    SELECT matnr
      FROM mara
      WHERE INSTR( matnr, 'A' ) > 0
      INTO TABLE @DATA(lt_data).
    ```

  - 使用示例：输出字符 `A` 第一次出现的位置。

    ```abap
    SELECT matnr,
           INSTR( matnr, 'A' ) AS pos_a
      FROM mara
      INTO TABLE @DATA(lt_data)
      UP TO 10 ROWS.
    ```

- **`REPLACE( arg1, arg2, arg3 )`**
  - 函数作用：将字符串中的指定内容替换为新内容。
  - 参数意义：
    - [`arg1`](#param-arg1)：原字符串字段、字面量或表达式。
    - [`arg2`](#param-arg2)：需要被替换的内容。
    - [`arg3`](#param-arg3)：替换后的内容。
    - `REPLACE( 'A-B-C', '-', ' ' )` 的结果类似 `'A B C'`。
    - 替换通常是大小写敏感的，例如替换 `'a'` 不一定会影响 `'A'`。
  - 使用示例：将物料描述中的 `-` 替换为空格。

    ```abap
    SELECT maktx,
           REPLACE( maktx, '-', ' ' ) AS text_new
      FROM makt
      INTO TABLE @DATA(lt_text)
      UP TO 10 ROWS.
    ```

- **`LPAD( arg, len, src )`**
  - 函数作用：在字符串左侧补齐字符，使结果达到指定长度。
  - 参数意义：
    - [`arg`](#param-arg)：原字符串字段、字面量或表达式。
    - [`len`](#param-len)：目标字符串总长度，不是要补几个字符。
    - [`src`](#param-src)：用于补齐的字符或字符串。
    - `LPAD( '123', 5, '0' )` 的结果类似 `'00123'`。
    - 因为目标长度是 `5`，原字符串长度是 `3`，所以左侧补 `2` 个 `0`。
    - 如果 [`src`](#param-src) 是多字符字符串，系统会重复使用 [`src`](#param-src) 直到达到目标长度。
    - 如果 [`len`](#param-len) 小于原字符串长度，结果可能会被截断，具体行为以系统版本为准。
  - 使用示例：将物料号左侧补 `0` 到 `18` 位。

    ```abap
    SELECT matnr,
           LPAD( matnr, 18, '0' ) AS matnr_18
      FROM mara
      INTO TABLE @DATA(lt_data)
      UP TO 10 ROWS.
    ```

- **`RPAD( arg, len, src )`**
  - 函数作用：在字符串右侧补齐字符，使结果达到指定长度。
  - 参数意义：
    - [`arg`](#param-arg)：原字符串字段、字面量或表达式。
    - [`len`](#param-len)：目标字符串总长度，不是要补几个字符。
    - [`src`](#param-src)：用于补齐的字符或字符串。
    - `RPAD( '123', 5, '0' )` 的结果类似 `'12300'`。
    - 因为目标长度是 `5`，原字符串长度是 `3`，所以右侧补 `2` 个 `0`。
  - 使用示例：将物料号右侧补空格到 `20` 位。

    ```abap
    SELECT matnr,
           RPAD( matnr, 20, ' ' ) AS matnr_pad
      FROM mara
      INTO TABLE @DATA(lt_data)
      UP TO 10 ROWS.
    ```

- **`LTRIM( arg, char )`**
  - 函数作用：从字符串左侧去除指定字符。
  - 参数意义：
    - [`arg`](#param-arg)：原字符串字段、字面量或表达式。
    - [`char`](#param-char)：要从左侧连续去除的字符。
    - `LTRIM( '000123', '0' )` 的结果类似 `'123'`。
    - 只有左侧连续匹配 [`char`](#param-char) 的字符会被去除，中间或右侧的字符不会被去除。
    - 如果 [`char`](#param-char) 是空格 `' '`，表示去除左侧空格。
  - 使用示例：去除物料号左侧前导 `0`。

    ```abap
    SELECT matnr,
           LTRIM( matnr, '0' ) AS matnr_no_zero
      FROM mara
      INTO TABLE @DATA(lt_data)
      UP TO 10 ROWS.
    ```

- **`RTRIM( arg, char )`**
  - 函数作用：从字符串右侧去除指定字符。
  - 参数意义：
    - [`arg`](#param-arg)：原字符串字段、字面量或表达式。
    - [`char`](#param-char)：要从右侧连续去除的字符。
    - `RTRIM( '123000', '0' )` 的结果类似 `'123'`。
    - 只有右侧连续匹配 [`char`](#param-char) 的字符会被去除，中间或左侧的字符不会被去除。
    - 如果 [`char`](#param-char) 是空格 `' '`，表示去除右侧空格。
  - 使用示例：去除物料描述右侧空格。

    ```abap
    SELECT maktx,
           RTRIM( maktx, ' ' ) AS maktx_trim
      FROM makt
      INTO TABLE @DATA(lt_text)
      UP TO 10 ROWS.
    ```

---

## 数值函数

- **`ABS( arg )`**
  - 函数作用：返回数值的绝对值。
  - 参数意义：
    - [`arg`](#param-arg)：数值字段、数值字面量或数值表达式。
    - `ABS( -10 )` 的结果是 `10`。
    - `ABS( 10 )` 的结果仍然是 `10`。
  - 使用示例：取数量字段的绝对值。

    ```abap
    SELECT ebeln,
           ABS( menge ) AS qty_abs
      FROM ekpo
      INTO TABLE @DATA(lt_qty)
      UP TO 10 ROWS.
    ```

- **`CEIL( arg )`**
  - 函数作用：向上取整，返回大于或等于原数值的最小整数。
  - 参数意义：
    - [`arg`](#param-arg)：数值字段、数值字面量或数值表达式。
    - `CEIL( 1.2 )` 的结果是 `2`。
    - `CEIL( 1.0 )` 的结果是 `1`。
    - `CEIL( -1.2 )` 的结果通常是 `-1`，因为 `-1` 大于 `-1.2`。
  - 使用示例：数量向上取整。

    ```abap
    SELECT ebeln,
           CEIL( menge ) AS qty_ceil
      FROM ekpo
      INTO TABLE @DATA(lt_qty)
      UP TO 10 ROWS.
    ```

- **`FLOOR( arg )`**
  - 函数作用：向下取整，返回小于或等于原数值的最大整数。
  - 参数意义：
    - [`arg`](#param-arg)：数值字段、数值字面量或数值表达式。
    - `FLOOR( 1.8 )` 的结果是 `1`。
    - `FLOOR( 1.0 )` 的结果是 `1`。
    - `FLOOR( -1.2 )` 的结果通常是 `-2`，因为 `-2` 小于 `-1.2`。
  - 使用示例：数量向下取整。

    ```abap
    SELECT ebeln,
           FLOOR( menge ) AS qty_floor
      FROM ekpo
      INTO TABLE @DATA(lt_qty)
      UP TO 10 ROWS.
    ```

- **`ROUND( arg, pos )`**
  - 函数作用：对数值进行四舍五入。
  - 参数意义：
    - [`arg`](#param-arg)：需要四舍五入的数值字段、数值字面量或数值表达式。
    - [`pos`](#param-pos)：指定四舍五入的位置。
    - [`pos`](#param-pos) `> 0`：保留小数点右侧第 [`pos`](#param-pos) 位。
    - [`pos`](#param-pos) `= 0`：四舍五入到整数。
    - [`pos`](#param-pos) `< 0`：四舍五入到小数点左侧，例如十位、百位。
  - 直观理解：
    - `ROUND( 123.456, 2 )` 的结果类似 `123.46`。
    - `ROUND( 123.456, 0 )` 的结果类似 `123`。
    - `ROUND( 123.456, -1 )` 的结果类似 `120`。
  - 使用示例：金额保留 `2` 位小数。

    ```abap
    SELECT vbeln,
           ROUND( netwr, 2 ) AS netwr_round
      FROM vbak
      INTO TABLE @DATA(lt_vbak)
      UP TO 10 ROWS.
    ```

- **`DIV( arg1, arg2 )`**
  - 函数作用：执行整数除法，返回商的整数部分。
  - 参数意义：
    - [`arg1`](#param-arg1)：被除数。
    - [`arg2`](#param-arg2)：除数。
    - [`arg2`](#param-arg2) 不能为 `0`，否则会发生除零错误。
    - `DIV( 7, 3 )` 的结果是 `2`。
    - `DIV( 10, 5 )` 的结果是 `2`。
  - 使用示例：根据数量计算完整包装数。

    ```abap
    SELECT ebeln,
           DIV( menge, 10 ) AS pack_count
      FROM ekpo
      INTO TABLE @DATA(lt_pack)
      UP TO 10 ROWS.
    ```

- **`DIVISION( arg1, arg2, dec )`**
  - 函数作用：执行小数除法，并将结果四舍五入到指定小数位。
  - 参数意义：
    - [`arg1`](#param-arg1)：被除数。
    - [`arg2`](#param-arg2)：除数。
    - [`dec`](#param-dec)：结果保留的小数位数。
    - [`arg2`](#param-arg2) 不能为 `0`，否则会发生除零错误。
    - `DIVISION( 10, 3, 2 )` 的结果类似 `3.33`。
    - `DIVISION( 10, 3, 0 )` 的结果类似 `3`。
  - 使用示例：金额除以 `100` 后保留 `2` 位小数。

    ```abap
    SELECT vbeln,
           DIVISION( netwr, 100, 2 ) AS value_div
      FROM vbak
      INTO TABLE @DATA(lt_vbak)
      UP TO 10 ROWS.
    ```

- **`MOD( arg1, arg2 )`**
  - 函数作用：返回除法运算后的余数。
  - 参数意义：
    - [`arg1`](#param-arg1)：被除数。
    - [`arg2`](#param-arg2)：除数。
    - [`arg2`](#param-arg2) 不能为 `0`，否则会发生除零错误。
    - `MOD( 7, 3 )` 的结果是 `1`。
    - `MOD( 10, 5 )` 的结果是 `0`。
  - 使用示例：计算数量除以 `10` 后的余数。

    ```abap
    SELECT ebeln,
           MOD( menge, 10 ) AS qty_mod
      FROM ekpo
      INTO TABLE @DATA(lt_mod)
      UP TO 10 ROWS.
    ```

  - 使用注意：
    - SQL 函数 `DIV`、`MOD` 对负数符号的处理可能与 ABAP 运算符 `DIV`、`MOD` 不完全相同。
    - 涉及负数时，不要只凭经验判断，建议用测试数据验证。

---

## 空值处理函数

- **`COALESCE( arg1, arg2, ..., argn )`**
  - 函数作用：返回参数列表中第一个非数据库 `NULL` 的值。
  - 参数意义：
    - [`arg1`](#param-arg1)：第一个待判断的字段、字面量、表达式或 host variable。
    - [`arg2`](#param-arg2)：第二个待判断的字段、字面量、表达式或 host variable。
    - [`argn`](#param-argn)：后续待判断的字段、字面量、表达式或 host variable。
    - 系统会从左到右依次判断参数。
    - 如果 [`arg1`](#param-arg1) 不是数据库 `NULL`，则直接返回 [`arg1`](#param-arg1)。
    - 如果 [`arg1`](#param-arg1) 是数据库 `NULL`，则继续判断 [`arg2`](#param-arg2)。
    - 常见写法是 `COALESCE( 字段, 默认值 )`。
  - 使用示例：外连接查询物料描述，如果没有描述则显示 `无描述`。

    ```abap
    SELECT a~matnr,
           COALESCE( b~maktx, '无描述' ) AS maktx
      FROM mara AS a
      LEFT OUTER JOIN makt AS b
        ON b~matnr = a~matnr
       AND b~spras = @sy-langu
      INTO TABLE @DATA(lt_mat_text)
      UP TO 10 ROWS.
    ```

  - 使用注意：
    - `COALESCE` 处理的是数据库 `NULL`。
    - ABAP 字段的初始值，例如空字符串、`0`、`00000000`，不等同于数据库 `NULL`。
    - `COALESCE( field, '默认值' )` 不会把空字符串自动当成 `NULL`。
    - `COALESCE` 常用于 `LEFT OUTER JOIN` 后给右表不存在的数据设置默认值。

---

## 日期函数

- **`DATS_IS_VALID( date )`**
  - 函数作用：判断日期是否合法。
  - 参数意义：
    - [`date`](#param-date)：需要判断的日期，通常为 `DATS` 类型，格式为 `YYYYMMDD`。
    - 合法日期通常返回 `1`。
    - 不合法日期通常返回 `0`。
    - 例如 `20260517` 是合法日期。
    - 例如 `00000000` 通常不是合法日期。
  - 使用示例：判断销售订单创建日期是否合法。

    ```abap
    SELECT SINGLE DATS_IS_VALID( erdat ) AS valid
      FROM vbak
      INTO @DATA(lv_valid).
    ```

- **`DATS_DAYS_BETWEEN( date1, date2 )`**
  - 函数作用：计算两个日期之间相差的天数。
  - 参数意义：
    - [`date1`](#param-date1)：第一个日期，通常作为起始日期。
    - [`date2`](#param-date2)：第二个日期，通常作为结束日期。
    - 结果通常按 [`date2`](#param-date2) `-` [`date1`](#param-date1) 理解。
    - 如果 [`date2`](#param-date2) 晚于 [`date1`](#param-date1)，结果为正数。
    - 如果 [`date2`](#param-date2) 早于 [`date1`](#param-date1)，结果为负数。
    - 如果两个日期相同，结果为 `0`。
  - 直观理解：
    - `DATS_DAYS_BETWEEN( '20260501', '20260517' )` 的结果是 `16`。
    - `DATS_DAYS_BETWEEN( '20260517', '20260501' )` 的结果是 `-16`。
  - 使用示例：计算销售订单创建至今多少天。

    ```abap
    SELECT vbeln,
           erdat,
           DATS_DAYS_BETWEEN( erdat, @sy-datum ) AS days_old
      FROM vbak
      INTO TABLE @DATA(lt_days)
      UP TO 10 ROWS.
    ```

- **`DATS_ADD_DAYS( date, days )`**
  - 函数作用：对日期增加或减少指定天数。
  - 参数意义：
    - [`date`](#param-date)：原始日期，通常为 `DATS` 类型，格式为 `YYYYMMDD`。
    - [`days`](#param-days)：需要增加或减少的天数。
    - [`days`](#param-days) `> 0` 表示向后加天数。
    - [`days`](#param-days) `< 0` 表示向前减天数。
    - [`days`](#param-days) `= 0` 表示日期不变。
  - 直观理解：
    - `DATS_ADD_DAYS( '20260517', 1 )` 的结果类似 `'20260518'`。
    - `DATS_ADD_DAYS( '20260517', -1 )` 的结果类似 `'20260516'`。
  - 使用示例：销售订单创建日期加 `30` 天。

    ```abap
    SELECT vbeln,
           DATS_ADD_DAYS( erdat, 30 ) AS date_plus_30
      FROM vbak
      INTO TABLE @DATA(lt_date)
      UP TO 10 ROWS.
    ```

  - 版本差异说明：
    - 某些版本或 CDS / ABAP SQL 上下文中，语法可能是 `DATS_ADD_DAYS( date, days, on_error )`。
    - [`on_error`](#param-on-error) 用来指定计算出错时如何处理，例如 `'FAIL'`、`'NULL'`、`'INITIAL'`、`'UNCHANGED'`。
    - 如果你的系统提示需要第三个参数，可写成如下形式。

    ```abap
    SELECT vbeln,
           DATS_ADD_DAYS( erdat, 30, 'INITIAL' ) AS date_plus_30
      FROM vbak
      INTO TABLE @DATA(lt_date)
      UP TO 10 ROWS.
    ```

- **`DATS_ADD_MONTHS( date, months )`**
  - 函数作用：对日期增加或减少指定月份。
  - 参数意义：
    - [`date`](#param-date)：原始日期，通常为 `DATS` 类型，格式为 `YYYYMMDD`。
    - [`months`](#param-months)：需要增加或减少的月份数。
    - [`months`](#param-months) `> 0` 表示向后加月份。
    - [`months`](#param-months) `< 0` 表示向前减月份。
    - [`months`](#param-months) `= 0` 表示日期不变。
  - 直观理解：
    - `DATS_ADD_MONTHS( '20260517', 1 )` 的结果类似 `'20260617'`。
    - `DATS_ADD_MONTHS( '20260517', -1 )` 的结果类似 `'20260417'`。
    - 如果目标月份没有对应日期，例如从 `20260131` 加 `1` 个月，结果通常会调整到目标月份的最后一天，具体以系统规则为准。
  - 使用示例：销售订单创建日期加 `1` 个月。

    ```abap
    SELECT vbeln,
           DATS_ADD_MONTHS( erdat, 1 ) AS next_month
      FROM vbak
      INTO TABLE @DATA(lt_date)
      UP TO 10 ROWS.
    ```

  - 版本差异说明：
    - 某些版本或 CDS / ABAP SQL 上下文中，语法可能是 `DATS_ADD_MONTHS( date, months, on_error )`。
    - [`on_error`](#param-on-error) 用来指定计算出错时如何处理。
    - 如果你的系统提示需要第三个参数，可写成如下形式。

    ```abap
    SELECT vbeln,
           DATS_ADD_MONTHS( erdat, 1, 'INITIAL' ) AS next_month
      FROM vbak
      INTO TABLE @DATA(lt_date)
      UP TO 10 ROWS.
    ```

---

## 类型转换表达式

- **`CAST( operand AS dtype )`**
  - 函数作用：将字段或 SQL 表达式转换为指定的 ABAP Dictionary 类型。
  - 参数意义：
    - [`operand`](#param-operand)：需要转换的字段、字面量或 SQL 表达式。
    - [`dtype`](#param-dtype)：目标数据类型。
    - [`dtype`](#param-dtype) 可以是系统支持的字典内置类型，例如 `CHAR(10)`、`NUMC(10)`、`DEC(15,2)`、`FLTP`、`DATS`、`TIMS`、`INT4` 等。
    - `CAST( netwr AS FLTP )` 表示把 `netwr` 转成浮点类型再参与后续计算或输出。
  - 使用示例：将金额转换为浮点类型。

    ```abap
    SELECT vbeln,
           CAST( netwr AS FLTP ) AS netwr_f
      FROM vbak
      INTO TABLE @DATA(lt_vbak)
      UP TO 10 ROWS.
    ```

  - 使用示例：先转换类型，再做除法计算。

    ```abap
    SELECT vbeln,
           CAST( netwr AS FLTP ) / 100 AS netwr_div
      FROM vbak
      INTO TABLE @DATA(lt_result)
      UP TO 10 ROWS.
    ```

  - 使用注意：
    - `CAST` 严格来说是 SQL 表达式，不是普通函数，但在 Open SQL 中非常常用。
    - 转换时要注意目标类型长度和小数位，长度不够可能导致截断、溢出或语法错误。
    - 金额 `CURR` 和数量 `QUAN` 字段通常还涉及币种或单位语义，单纯 `CAST` 只处理数值类型，不会自动做币种或单位换算。

---

## 常用组合示例

- 查询物料描述，处理空值并转大写。

  ```abap
  SELECT a~matnr,
         UPPER( COALESCE( b~maktx, '无描述' ) ) AS maktx_upper
    FROM mara AS a
    LEFT OUTER JOIN makt AS b
      ON b~matnr = a~matnr
     AND b~spras = @sy-langu
    INTO TABLE @DATA(lt_result)
    UP TO 20 ROWS.
  ```

- 按采购订单汇总数量。

  ```abap
  SELECT ebeln,
         COUNT( * )   AS item_count,
         SUM( menge ) AS total_qty,
         MAX( menge ) AS max_qty,
         MIN( menge ) AS min_qty
    FROM ekpo
    GROUP BY ebeln
    INTO TABLE @DATA(lt_po_sum).
  ```

- 查询销售订单创建了多少天。

  ```abap
  SELECT vbeln,
         erdat,
         DATS_DAYS_BETWEEN( erdat, @sy-datum ) AS days_from_create
    FROM vbak
    INTO TABLE @DATA(lt_vbak_days)
    UP TO 20 ROWS.
  ```

- 拼接字段生成显示文本。

  ```abap
  SELECT matnr,
         CONCAT_WITH_SPACE( matnr, mtart, 1 ) AS mat_display
    FROM mara
    INTO TABLE @DATA(lt_mat)
    UP TO 20 ROWS.
  ```

- 金额四舍五入并转换类型。

  ```abap
  SELECT vbeln,
         ROUND( netwr, 2 )     AS netwr_round,
         CAST( netwr AS FLTP ) AS netwr_float
    FROM vbak
    INTO TABLE @DATA(lt_amount)
    UP TO 20 ROWS.
  ```

- 将 `DATS` 日期 `YYYYMMDD` 拆分成年、月、日。

  ```abap
  SELECT vbeln,
         erdat,
         SUBSTRING( erdat, 1, 4 ) AS year,
         SUBSTRING( erdat, 5, 2 ) AS month,
         SUBSTRING( erdat, 7, 2 ) AS day
    FROM vbak
    INTO TABLE @DATA(lt_date_parts)
    UP TO 20 ROWS.
  ```

- 判断固定位置的字符。

  ```abap
  SELECT matnr
    FROM mara
    WHERE SUBSTRING( matnr, 2, 2 ) = 'BC'
    INTO TABLE @DATA(lt_matnr).
  ```

- 判断任意位置是否包含某个子串。

  ```abap
  SELECT matnr
    FROM mara
    WHERE INSTR( matnr, 'BC' ) > 0
    INTO TABLE @DATA(lt_matnr).
  ```

---

## 使用注意点

- 函数参数位置要按定义写，不能随意交换。
  - `SUBSTRING( arg, pos, len )` 中，第二个参数一定是 [`pos`](#param-pos)，第三个参数一定是 [`len`](#param-len)。
  - `SUBSTRING( 'abc', 2, 2 )` 是从第 `2` 位开始取 `2` 位，结果是 `'bc'`。

- [`pos`](#param-pos) 和 ABAP offset 不一样。
  - Open SQL 的 `SUBSTRING`：[`pos`](#param-pos) `= 1` 表示第一个字符。
  - ABAP 字符串偏移：`lv_text+0(1)` 表示第一个字符。
  - 因此，Open SQL 的 `SUBSTRING( 字符串, 2, 2 )` 大致对应 ABAP 偏移写法中的 `字符串+1(2)`。

- [`len`](#param-len) 是长度，不是结束位置。
  - `SUBSTRING( 'abcdef', 2, 3 )` 表示从第 `2` 位开始取 `3` 位，结果是 `'bcd'`。
  - 它不是“从第 `2` 位取到第 `3` 位”。

- 固定位置判断用 `SUBSTRING`。
  - 例如判断物料号第 `2` 到第 `3` 位是不是 `BC`，用 `SUBSTRING( matnr, 2, 2 ) = 'BC'`。

- 任意位置包含判断用 `INSTR`。
  - 例如判断物料号任意位置是否包含 `BC`，用 `INSTR( matnr, 'BC' ) > 0`。

- 尽量让数据库端完成计算。
  - 函数写在 `SELECT` 中，会在数据库层参与计算，通常比先查出大量数据再在 ABAP `LOOP` 中处理更合适。

- 注意数据库 `NULL` 与 ABAP initial value 的区别。
  - `COALESCE` 处理的是数据库 `NULL`。
  - 空字符串、`0`、`00000000` 是 ABAP 或字段值层面的初始值，不一定是数据库 `NULL`。

- 聚合函数与普通字段同时出现在 `SELECT` 列表中时，通常需要配合 `GROUP BY` 使用。
  - 例如选择 `matnr` 和 `SUM( labst )` 时，应使用 `GROUP BY matnr`。

- 不同 ABAP 版本支持的 Open SQL / ABAP SQL 函数可能不同。
  - 老系统中请以 SE38 / ADT 语法检查和 F1 文档为准。
  - 特别是日期函数 `DATS_ADD_DAYS`、`DATS_ADD_MONTHS` 是否需要 [`on_error`](#param-on-error)，要看系统版本和使用场景。

---

## 参考资料

- [SAP Help - Open SQL Functions](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-us/abenopen_sql_functions.htm)
- [SAP Help - SQL String Functions](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abensql_functions_string.htm)
- [SAP Help - SQL Numeric Functions](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-us/abensql_functions_numeric.htm)
- [SAP Help - Open SQL Date Functions 7.51](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-us/abensql_date_func.htm)
- [SAP Help - Date and Time Functions 7.53](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/abenddic_date_time_functions.htm)
- [SAP Help - COALESCE](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-us/abensql_coalesce.htm)
- [SAP Help - CAST Expression](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-us/abensql_cast.htm)

---

## 附录 A：通用参数写法

> 本附录用于统一解释正文中反复出现的参数名。正文里的参数链接会跳转到这里。

- <a id="param-arg"></a>`arg`
  - 表示一个“函数参数”或“单个操作数”。
  - 它可以是数据库字段、SQL 表达式、字面量、host variable，或者另一个 SQL 函数的结果。
  - 示例：`matnr`、`'ABC'`、`@lv_text`、`UPPER( maktx )` 都可以作为某些函数的 `arg`。

- <a id="param-arg1"></a>`arg1`
  - 表示第 `1` 个参数。
  - 在字符串拼接、替换、除法等函数中，`arg1` 通常是最主要的输入值。
  - 示例：`CONCAT( arg1, arg2 )` 中，`arg1` 是拼接结果的前半部分。
  - 示例：`DIV( arg1, arg2 )` 中，`arg1` 是被除数。

- <a id="param-arg2"></a>`arg2`
  - 表示第 `2` 个参数。
  - 它的含义取决于具体函数。
  - 示例：`CONCAT( arg1, arg2 )` 中，`arg2` 是拼接结果的后半部分。
  - 示例：`DIV( arg1, arg2 )` 中，`arg2` 是除数，不能为 `0`。

- <a id="param-arg3"></a>`arg3`
  - 表示第 `3` 个参数。
  - 示例：`REPLACE( arg1, arg2, arg3 )` 中，`arg3` 是替换后的新内容。

- <a id="param-argn"></a>`argn`
  - 表示第 `n` 个参数，也就是参数列表中的后续参数。
  - 常见于参数数量可变的函数，例如 `COALESCE( arg1, arg2, ..., argn )`。
  - `COALESCE` 会从左到右依次判断这些参数，返回第一个非数据库 `NULL` 的值。

- <a id="param-col"></a>`col`
  - 表示数据库表、数据库视图或 CDS View 中的字段。
  - 在聚合函数中，`col` 是被统计、求和、求平均、求最大值或求最小值的字段。
  - 示例：`SUM( labst )` 中，`labst` 就是 `col`。
  - 示例：`COUNT( DISTINCT matkl )` 中，`matkl` 就是 `col`。

- <a id="param-star"></a>`*`
  - 在 `COUNT( * )` 中表示统计行数。
  - 它不是某个具体字段，而是表示“统计满足条件的记录行”。
  - 如果有 `GROUP BY`，则统计每个分组内的行数。

- <a id="param-distinct"></a>`DISTINCT`
  - 表示去重。
  - 在 `COUNT( DISTINCT col )` 中，系统会先对 `col` 的值去重，再统计去重后的数量。
  - 示例：`COUNT( DISTINCT matkl )` 表示统计不同物料组的数量。

- <a id="param-pos"></a>`pos`
  - 表示位置。
  - 在 Open SQL 字符串函数中，字符位置通常从 `1` 开始数，不是从 `0` 开始。
  - 对字符串 `'abc'`：
    - `a` 的位置是 `1`。
    - `b` 的位置是 `2`。
    - `c` 的位置是 `3`。
  - 因此，`SUBSTRING( 'abc', 2, 2 )` 表示从第 `2` 个字符 `b` 开始，取 `2` 个字符，结果是 `'bc'`。
  - 注意区别：ABAP 字符串偏移写法如 `lv_text+1(2)` 是从偏移量 `0` 开始算；Open SQL 的 `SUBSTRING` 参数 `pos` 是从位置 `1` 开始算。
  - 在 `ROUND( arg, pos )` 中，`pos` 表示四舍五入的位置：
    - `pos > 0` 表示保留小数位。
    - `pos = 0` 表示取整。
    - `pos < 0` 表示对十位、百位等整数位进行四舍五入。

- <a id="param-len"></a>`len`
  - 表示长度，不是结束位置。
  - 在 `SUBSTRING( arg, pos, len )` 中，`len` 表示从 `pos` 开始连续取多少个字符。
  - 示例：`SUBSTRING( 'abc', 2, 2 )` 中，`len = 2` 表示“从第 `2` 位开始取 `2` 个字符”，结果是 `'bc'`。
  - 对字符串 `'abc'`：
    - `SUBSTRING( 'abc', 1, 1 )` 的结果是 `'a'`。
    - `SUBSTRING( 'abc', 2, 1 )` 的结果是 `'b'`。
    - `SUBSTRING( 'abc', 2, 2 )` 的结果是 `'bc'`。
    - `SUBSTRING( 'abc', 3, 1 )` 的结果是 `'c'`。
  - 在 `LEFT( arg, len )`、`RIGHT( arg, len )` 中，`len` 表示从左侧或右侧要取出的字符数量。
  - 在 `LPAD( arg, len, src )`、`RPAD( arg, len, src )` 中，`len` 表示最终结果的目标总长度，不是要补几个字符。

- <a id="param-spaces"></a>`spaces`
  - 表示要插入的空格数量。
  - 用于 `CONCAT_WITH_SPACE( arg1, arg2, spaces )`。
  - 示例：`CONCAT_WITH_SPACE( 'A', 'B', 1 )` 结果类似 `'A B'`。
  - 示例：`CONCAT_WITH_SPACE( 'A', 'B', 3 )` 结果类似 `'A   B'`。

- <a id="param-sub"></a>`sub`
  - 表示要查找的子串。
  - 用于 `INSTR( arg, sub )`。
  - 示例：`INSTR( 'abc', 'bc' )` 中，`sub` 是 `'bc'`，结果是 `2`。
  - 如果找不到 `sub`，通常返回 `0`。

- <a id="param-src"></a>`src`
  - 表示用于补齐的字符或字符串。
  - 用于 `LPAD( arg, len, src )` 和 `RPAD( arg, len, src )`。
  - 示例：`LPAD( '123', 5, '0' )` 中，`src` 是 `'0'`，结果类似 `'00123'`。
  - 如果 `src` 是多个字符，系统会重复使用它直到达到目标长度。

- <a id="param-char"></a>`char`
  - 表示要去除的指定字符。
  - 用于 `LTRIM( arg, char )` 和 `RTRIM( arg, char )`。
  - 示例：`LTRIM( '000123', '0' )` 中，`char` 是 `'0'`，结果类似 `'123'`。
  - `LTRIM` 只处理左侧连续匹配的字符；`RTRIM` 只处理右侧连续匹配的字符。

- <a id="param-dec"></a>`dec`
  - 表示小数位数。
  - 用于 `DIVISION( arg1, arg2, dec )`。
  - 示例：`DIVISION( 10, 3, 2 )` 表示 `10 / 3` 后保留 `2` 位小数，结果类似 `3.33`。
  - `dec = 0` 时，结果保留 `0` 位小数。

- <a id="param-date"></a>`date`
  - 表示日期。
  - 通常是 ABAP Dictionary 类型 `DATS`，格式为 `YYYYMMDD`。
  - 示例：`'20260517'` 表示 2026 年 5 月 17 日。
  - 常用于 `DATS_IS_VALID( date )`、`DATS_ADD_DAYS( date, days )`、`DATS_ADD_MONTHS( date, months )`。

- <a id="param-date1"></a>`date1`
  - 表示第 `1` 个日期。
  - 在 `DATS_DAYS_BETWEEN( date1, date2 )` 中，通常作为起始日期。
  - 结果可以按 `date2 - date1` 理解。

- <a id="param-date2"></a>`date2`
  - 表示第 `2` 个日期。
  - 在 `DATS_DAYS_BETWEEN( date1, date2 )` 中，通常作为结束日期。
  - 如果 `date2` 晚于 `date1`，结果通常为正数。
  - 如果 `date2` 早于 `date1`，结果通常为负数。

- <a id="param-days"></a>`days`
  - 表示天数。
  - 用于 `DATS_ADD_DAYS( date, days )`。
  - `days > 0` 表示向后加天数。
  - `days < 0` 表示向前减天数。
  - `days = 0` 表示日期不变。

- <a id="param-months"></a>`months`
  - 表示月份数。
  - 用于 `DATS_ADD_MONTHS( date, months )`。
  - `months > 0` 表示向后加月份。
  - `months < 0` 表示向前减月份。
  - `months = 0` 表示日期不变。
  - 如果目标月份没有对应日期，例如 `20260131` 加 `1` 个月，结果通常会调整到目标月份的最后一天，具体以系统规则为准。

- <a id="param-on-error"></a>`on_error`
  - 表示日期计算出错时的处理方式。
  - 部分 ABAP SQL / CDS 日期函数版本中会出现该参数。
  - 常见值包括：
    - `'FAIL'`：出错时抛出错误。
    - `'NULL'`：出错时返回数据库 `NULL`。
    - `'INITIAL'`：出错时返回初始值。
    - `'UNCHANGED'`：出错时返回原始日期。
  - `on_error` 一般要求传字面量，不能随意传变量。
  - 如果系统语法检查提示 `DATS_ADD_DAYS` 或 `DATS_ADD_MONTHS` 需要第三个参数，可以补上 `on_error`。

- <a id="param-operand"></a>`operand`
  - 表示需要被处理或转换的操作数。
  - 在 `CAST( operand AS dtype )` 中，`operand` 是要被转换类型的字段、字面量或 SQL 表达式。
  - 示例：`CAST( netwr AS FLTP )` 中，`netwr` 就是 `operand`。

- <a id="param-dtype"></a>`dtype`
  - 表示目标数据类型。
  - 在 `CAST( operand AS dtype )` 中，`dtype` 是转换后的 ABAP Dictionary 类型。
  - 示例：`CHAR(10)`、`NUMC(10)`、`DEC(15,2)`、`FLTP`、`DATS`、`TIMS`、`INT4`。
  - 使用 `dtype` 时要注意长度和小数位，长度不足可能导致截断、溢出或语法错误。
