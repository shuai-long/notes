## 扣减定额不按补偿时间生效

- 0416中补偿定额扣减的定额记录不按时间生效，在函数`HR_SETTLE_QUOTA`中修改，修改点如下：

![image-20260511111438227](https://picture-bj.oss-cn-beijing.aliyuncs.com/pciture/image-20260511111438227.png)

```abap
ENHANCEMENT 1  ZFIXED_COMPENSATION_MATCHING.    "active version
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""$"$\SE:(2) Function Module HR_SETTLE_QUOTA, Start, Enhancement ZFIXED_COMPENSATION_MATCHING, Start                                                           A
*
** buffers and structures
  data: t2006xi_1 like p2006x occurs 0 with header line,
        t2007xi_1 like p2007x occurs 0 with header line.
** variables
  data: w503_1   like t503,     "statics buffers placed in subroutines
        w001p_1  like t001p,
        sinfty_1 like p2006-infty.
* preparations
  if not buffer_refresh is initial.
    refresh tquoded.
  endif.
* input checks
  if infty <> '2006' and
     infty <> '2007'.
    raise wrong_infty.
  endif.
  if srule is initial and
     qtype is initial and
     quonr is initial.
    raise zero_settlement.
  endif.
* determine settlement date
  if sdate is initial.
    sdate = sy-datum.
  endif.

  loop at t0001 where pernr = pernum
                and   sprps = space
                and   begda <= sdate
                and   endda >= sdate.
    exit.
  endloop.
  if sy-subrc <> 0.
* Infotype 0001 for & does not exist or is not complete
    perform error_message using 'P2'
                                'P'
                                '839'
                                pernum space space space.
*   raise it0001_missing.
  endif.
*--- end of input checks

* transfer quota to the operational buffers
  t2006xi_1[] = t2006x[].
  t2007xi_1[] = t2007x[].
* determine groupings for T556A/P and T556R
  perform read_t001p using  w001p_1   "with error handling
                            t0001-werks
                            t0001-btrtl.
  perform read_t503  using  w503_1    "with error handling
                            t0001-persg
                            t0001-persk.
* evaluate IGNORE_INTERVAL flag
  if not ignore_interval is initial and docnr+16(4) ne '0416'.
    perform open_deduction_intervals
                   tables t2006xi_1
                          t2007xi_1
                   using  infty.
  endif.
  if arch_mode eq space.                                    "Note1702071
* eliminate quota records which are locked for compensation (T556A/P)
    perform check_out_locked_recs
                     tables t2006xi_1
                            t2007xi_1
                     using  infty
                            sdate
                            w503_1-konty
                            w001p_1-mozko.
  endif.                                                    "Note1702071
* set technical infotype for deduction
  sinfty_1 = infty - 5.
* determine settlement method
  if quonr cn ' 0'.
    perform settle_quota_record
                   tables  t2006xi_1
                           t2007xi_1
                           tquoded
                   using   pernum
                           sinfty_1
                           sdate
                           number
                           docnr
                           quonr
                           w503_1-konty
                           w001p_1-mozko
                           buffer_refresh
                           arch_mode.                       "Note1702071
  elseif qtype cn ' 0'.
* direct method of quota settlement, only from one quota type
    perform settle_quota_ktart
                   tables   t2006xi_1
                            t2007xi_1
                            tquoded
                   using    pernum
                            sinfty_1
                            qtype
                            sdate
                            number
                            docnr
                            w503_1-konty
                            w001p_1-mozko
                            buffer_refresh
                            arch_mode.                      "Note1702071
  else.
*   extended method of quota settlement from several quota types,
*   order of quota records defined by T556R
    perform settle_quota_t556r
                   tables   t2006xi_1
                            t2007xi_1
                            tquoded
                   using    pernum
                            sinfty_1
                            srule
                            sdate
                            number
                            docnr
                            w503_1-konty
                            w001p_1-mozko
                            buffer_refresh.
  endif.
*--- restore DESTA/DEEND and return quota buffers
  loop at t2006xi_1 where not chngd is initial.
    read table t2006x with key quonr = t2006xi_1-quonr.
    check sy-subrc = 0.
    t2006xi_1-desta = t2006x-desta.
    t2006xi_1-deend = t2006x-deend.
    modify t2006x from t2006xi_1 index sy-tabix.
  endloop.
  loop at t2007xi_1 where not chngd is initial.
    read table t2007x with key quonr = t2007xi_1-quonr.
    check sy-subrc = 0.
    t2007xi_1-desta = t2007x-desta.
    t2007xi_1-deend = t2007x-deend.
    modify t2007x from t2007xi_1 index sy-tabix.
  endloop.

  exit .

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""$"$\SE:(3) Function Module HR_SETTLE_QUOTA, Start, Enhancement ZFIXED_COMPENSATION_MATCHING, End                                                             A
ENDENHANCEMENT.
```

