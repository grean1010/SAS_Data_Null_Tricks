*********************************************************************************;
* PROGRAM:  dataproperties.sas                                                  *;
* TASK:     SESTAT - NSF, Contractor Specifications (8797-110)                  *;
* PURPOSE:  Make format library, labels, length statements, and                 *;
*           data dictionary for SESTAT contractor specifications.               *;
*                                                                               *;
* WRITTEN:  By Maria Cupples Hudson after 2006 contractor specifications        *;
*                                                                               *;
* MODIFIED:                                                                     *;
*                                                                               *;
* INPUTS:  1) sestat_metadata.mdb - New Sestat Metadata Database                *;
*             a) DICTIONARY Table                                               *;
*             b) CODEVAL Table                                                  *;
*             c) CODE_FMT Table                                                 *;
*                                                                               *;
* OUTPUTS: 1) allformats.sas  - PROC FORMAT Statements                          *;
*          2) xxxx_sssss.FMT2 - DATA FORMAT Statements                          *;
*          3) xxxx_LABELS_sssss.SAS - 2006 LABEL Statements                     *;
*          4) xxxx_LENGTH_sssss.SAS - 2006 LENGTH Statements                    *;
*          5) sssss.DD1 - 2006 Base File DD for Contractor Specifications       *;
*          6) sssss.DD2 - 2006 Text File DD for Contractor Specifications       *;
*             Where sssss = Survey                                              *;
*                   xxxx = type (BASE or TEXT)                                  *;
*                                                                               *;
* NOTES:                                                                        *;
*                                                                               *;
*********************************************************************************;

* include the program that sets all parameters and libnames for contractor specs ;
%include "J:\06850\Common\Task411_2010_Specs\set_parameters.sas";

*********************************************************************************;
*                          Change these macros                                  *;
*********************************************************************************;

* Normally these will not need changing;
%let endline=%str(;);
%let quote=%str("");

******************************************************************************;
*                    Done changing macros and libnames                       *;
******************************************************************************;

* Bring in the data dictionary;
* Do this with a data step and then a sort.  SAS 9 and access are not;
* working well together and proc copy hangs up indefinitely.         ;
data dict;  
  set metadata.dictionary;
run;

proc sort data=dict;  by sas_name;  run;

* bring in all code names used in the current round of surveys;
* This includes all values of code_name_sestat and code_name_contractor;

* Delete the datasets used in the proc append just in case;
proc datasets lib=work;
  delete code1 code2 allcodes;
run;

* bring in the code names;
%macro codenames(survey);

  * bring in the code names used to create contractors specs;
  data code1;
    set metadata.&survey. (keep=code_nam_contractor createdby
                                where=(createdby in ('Questionnaire','Survey Contractor'))
                                rename=(code_nam_contractor=code_nam));
  run;

  * keep one record for each code name used;
  proc sort data=code1 nodupkey;  by code_nam;  run;

  * append to the allcodes dataset;
  proc append data=code1 (drop=createdby) base=allcodes;
  run;

  * Now create another dataset wtih all of the code names used;
  * in the sestat system;
  data code2;
    set metadata.&survey. (keep=code_nam_sestat createdby
                                where=(createdby in ('Questionnaire','Survey Contractor'))
                                rename=(code_nam_sestat=code_nam));
  run;

  * keep one record for each code name used;
  proc sort data=code2 nodupkey; by code_nam;  run;

  * append this to the allcodes dataset;
  proc append data=code2 (drop=createdby) base=allcodes;
  run;

%mend codenames;
%codenames(NSRCG&yr.);
%codenames(NSCG&yr.);
%codenames(SDR&yr.);
%codenames(NSCGN&yr.);

* remove blanks and duplicates (there will be many duplicates);
proc sort data=allcodes (where=(code_nam ne '')) nodupkey;  by code_nam;  run;

* Bring in and sort all code names and code values from the database ;
* Do this with a data step and then a sort.  SAS 9 and access are not;
* working well together and proc copy hangs up indefinitely.         ;
data codeval;
  set metadata.codeval;
run;
proc sort data=codeval;  by code_nam codeval;  run;

data codes2 problem;
  merge codeval (in=a) allcodes (in=b);
  by code_nam;

  * create a variable to hold the length of each value.;
  length lencode 8.;
  lencode = length(compress(codeval));

  * create a flag to indicate if the code name is used this year;
  if b then for&yr.dd = "Y";
  else for&yr.dd = "N";

  * Delete code values that make no sense or do not apply here;
  if code_nam = 'YES_NO_CODES' and codeval = '3' then delete;
  if code_nam = 'REFYR_CODES' and codeval ne "&year" then delete;
  if code_nam = 'SURVEY_MODES' and codeval = '7' then delete;

  if b and not a then output problem;
  output codes2;
run;

title "PROBLEM:  These code names are in the survey tables but not in the code value table!";
proc print data=problem;
run;
title;

proc sort data=codes2 noduprec;  by code_nam codeval;  run;

data codes2;
  set codes2;
  by code_nam codeval;
  if first.codeval + last.codeval ne 2 then dupflag = 1;
  else dupflag=0;
run;

title "PROBLEM:  These code values are not unique.  This will cause the";
title2 " resulting proc format to bomb.  Determine which code is not correct";
title3 " using the descr variable.  Fix in the previous data step and rerun.";
proc print data=codes2 (where=(dupflag=1)) noobs;
  by code_nam;
  var codeval descr;
run;
title;

* sort so that for each code name, the shortest lenth is at ;
* the top of the dataset and the longest is at the bottom.  ;
proc sort data=codes2;  
  by code_nam lencode;
run;

* lengths dataset will have the highest possible length for ;
* any code value associated with a given code name.  We also;
* take care of special cases in this data step.             ;
data lengths (keep=code_nam lencode);
  set codes2;
  by code_nam;

  * refid will always be 9 characters;
  if index(upcase(code_nam),"REFERENCE_ID") > 0 or 
     index(upcase(code_nam),"REFID") > 0 then lencode=9;

  * weights will always be 8 characters;
  if index(upcase(code_nam),"WEIGHT") > 0 then lencode = 8;

  * institution codes will always be 6 characters;
  if index(upcase(code_nam),"EDUCATION_INST_CODES") > 0 then lencode = 6;

  * response options for these variables was 1 - 10, but  ;
  * the current survey has options 1 - 9.  For the sake of;
  * consistency w/ previous years we set datasize to 2.   ;
  if code_nam = "AMOUNT_BORROW_OWE_NEW" then lencode = 2;

  if last.code_nam;
run;

* bring in the code_fmt table to get the name of the sas format to use;
data codefmt;
  set metadata.code_fmt;
  sas_fmt = compress(sas_fmt);

  * if it is a character format to be created then create a flag with $;
  * and change the sas_fmt variable to remove the first and last character;
  if index(sas_fmt,"$") > 0 then do;
    flag = "$";
    sas_fmt = substr(sas_fmt,2,length(sas_fmt)-2);
  end;
  else flag = " ";

  if substr(sas_fmt,length(sas_fmt),1) = '.' then sas_fmt = substr(sas_fmt,1,length(sas_fmt)-1);

run;

proc sort data=codefmt;  by code_nam;  run;

data codes2create;
  merge codes2 (in=a) codefmt (in=b) lengths;
  by code_nam;
  if a and for&yr.dd = "Y";
  
  if upcase(compress(codeval)) = 'NAP' then delete;

  * make not-valid descriptions consistend;
  if compress(upcase(descr)) in ('NOTVALID','INVALID') then descr="INVALID";
  * codevalc is the codeval with quotation marks around it;
  codevalc=compress("'"||codeval||"'");     
  * descc is the description with quotes around it;   
  descc=""""||trim(descr)||"""";

 
  * delete all refid and weighting codes. We will create;
  * these formats at the end of the next data _null_;
  if index(upcase(code_nam),'REFERENCE_ID')>0 or 
     index(upcase(code_nam),'REFID')>0 or
     index(upcase(code_nam),'WT_SURV')>0 or
     index(upcase(code_nam),'WEIGHT')>0 or
     index(upcase(code_nam),'REFYR')>0
     then delete;

run;

proc sort data=codes2create; 
  by code_nam codeval;
run;

* create a program with all possible formats for all 3 surveys;
data _null_;
  set codes2create end=last;
  by code_nam;

  file "&fmtpath.\formats.sas";

  if code_nam = 'WTSURVY_CODES' then delete;

  retain other;

  * at the first record, set up the program and proc format;
  if _n_ = 1 then do;
    put "***************************************************************;";
    put "* PROGRAM:  formats.sas                                       *;";
    put "* CREATED:  &sysdate &systime by dataproperties.sas               *;";
    put "* PURPOSE:  To create all possible formats for &year SESTAT    *;";
    put "*           variables.                                        *;";
    put "***************************************************************;";
    put //;
    put "proc format;";  
  end;

  if first.code_nam then do;

    * set an indicator for whether we have seen an OTHER category or not;
    other = 0;

    put @3 "/* format for code name " @27 code_nam @60 "*/";
    put @3 "value" @9 flag @11 sas_fmt;

    * add additional values to the year format;
    if upcase(sas_fmt) = 'YEARZ' then put @5 "0 - &year" @30 "= '0 - &year'";

  end;

  if flag = "$" then put @5 codevalc @30 "=" @32 descc;
  else put @5 codeval @30 "=" @32 descc;

  if upcase(codeval)='OTHER' then other=1;

  if last.code_nam then do;

    * if an other category has not been specified in the database;
    * then make sure it is specified here;
    if other=0 then put @5 "OTHER" @30 "= 'INVALID'";

    put @5 ";";

  end;
  if last then do;

    * create reference year format;
    put @3 "value" @11 "refyr";
    put @5 "&year" @30 "= '&year'";
    put @5 "OTHER" @30 "= 'INVALID'";
    put @5 ";";

    * create weight formats;
    put @3 "value" @11 "weights";
    put @5 "0 - 999.999" @30 "= '0 - 999.999'";
    put @5 "OTHER" @30 "= 'INVALID'";
    put @5 ";";

    put "run;";
  end;
run;

data _null_;
  file print;
  put "File &fmtpath.\format.sas Created on &sysdate &systime";
run;

* Include the program we just created.  That way if there is ;
* problem with it it will show up in the log of this program.;
%include "&fmtpath.\formats.sas";

* Now for each survey, apply the appropriate formats to each sas name;
%macro applyformats(surv=);

  %let survey = &surv.&yr.;

  * bring in the sas name and associated code name for all variables;
  * the survey contractor will be delivering.                       ;
  data &surv;
    set metadata.&survey. (keep=code_nam_contractor createdby sas_name
                           where=(createdby in ('Questionnaire','Survey Contractor'))
                           rename=(code_nam_contractor=code_nam));
    drop createdby;
  run;

  * we need to merge on the dictionary to get the base/text info;
  proc sort data=&surv.;  by sas_name;  run;
  proc sort data=dict;  by sas_name;  run;

  data &surv;
    merge &surv (in=a) dict;
    by sas_name;
    if a;
  run;

  data _null_;
    set &surv end=last;
    * Create keep statements for base variables;
    file "&fmtpath.\base_keepvars_&surv..sas";

    * set up the program;
    if _n_ = 1 then do;
      put "/***************************************************************/";
      put "/* PROGRAM:  base_keepvars_&surv..sas                             */";
      put "/* CREATED:  &sysdate &systime by dataproperties.sas               */";
      put "/* PURPOSE:  To keep all base variables relevant to &surv &year */";
      put "/***************************************************************/";
      put //;
      put "KEEP ";
    end;
    
    if basetext ne 'T' then put @6 sas_name;
    if last then put @6 ";";

    * Create keep statements for text variables;
    file "&fmtpath.\text_keepvars_&surv..sas";

    * set up the program;
    if _n_ = 1 then do;
      put "/***************************************************************/";
      put "/* PROGRAM:  text_keepvars_&surv..sas                             */";
      put "/* CREATED:  &sysdate &systime by dataproperties.sas               */";
      put "/* PURPOSE:  To keep all text variables relevant to &surv &year */";
      put "/***************************************************************/";
      put //;
      put "KEEP ";
    end;
    
    if basetext = 'T' then put @6 sas_name;
    if last then put @6 ";";

    * Create label statements for base variables;
    file "&fmtpath.\base_labels_&surv..sas";

    * set up the program;
    if _n_ = 1 then do;
      put "/***************************************************************/";
      put "/* PROGRAM:  base_labels_&surv..sas                               */";
      put "/* CREATED:  &sysdate &systime by dataproperties.sas               */";
      put "/* PURPOSE:  To label all base variables for &surv &year        */";
      put "/***************************************************************/";
      put //;
      put "LABEL ";
    end;
    
    if basetext ne 'T' then put @6 sas_name @20 "= '" new_name "'";
    if last then put @6 ";";


    * Create label statements for text variables;
    file "&fmtpath.\text_labels_&surv..sas";

    * set up the program;
    if _n_ = 1 then do;
      put "/***************************************************************/";
      put "/* PROGRAM:  text_labels_&surv..sas                               */";
      put "/* CREATED:  &sysdate &systime by dataproperties.sas               */";
      put "/* PURPOSE:  To label all text variables for &surv &year        */";
      put "/***************************************************************/";
      put //;
      put "LABEL ";
    end;
    
    if basetext = 'T' then put @6 sas_name @20 "= '" new_name "'";
    if last then put @6 ";";

    if last then do;
      file print;
      put "Files Created for &survey on &sysdate, &systime:";
      put "&fmtpath.\base_keepvars_&surv..sas";
      put "&fmtpath.\text_keepvars_&surv..sas";
      put "&fmtpath.\base_labels_&surv..sas";
      put "&fmtpath.\text_labels_&surv..sas";
    end;

  run;

  proc sort data=&surv.;  by code_nam;  run;

  * count up the number of variables having each code name;
  proc freq data=&surv. ;
    table code_nam / noprint out=countvars&surv. (drop=percent);
  run;

  * sort so that the max is at the end of the dataset;
  proc sort data=countvars&surv.;  by count;  run;

  * read in the dataset so that we can create a macro variable ;
  * with the maximum number of variables any code name can have;
  * associated with it for this survey-year contractor specs.  ;
  data _null_;
    set countvars&surv. end=last;
    if last then do;

      * the number of formats for this survey.                  ;
      call symput("numfmt&surv",left(compress(_n_)));

      * the max # of variables associated with any code name.   ;
      call symput("max&surv",left(compress(count)));

      * how many rows we will need to list 5 names at a time.   ;
      mod5 = int(count/5) + 1;
      call symput("mod5&surv",left(compress(mod5)));

      * the total # of variables the rows can possibly hold.    ;
      all = 5 * mod5;
      call symput("all&surv",left(compress(all)));

      * the difference between the total number of variables the;
      * rows can hold and the max ever used.                    ;
      diff = all - count;
      call symput("diff&surv",left(compress(diff)));

      * the first blank variable we will have to create.        ;
      firstblank = count + 1;
      call symput("firstblank&surv",left(compress(firstblank)));

    end;
  run;


  * now merge all info for the code names together;
  proc sort data=countvars&surv.;  by code_nam;  run;
  proc sort data=codefmt;  by code_nam;  run;

  data codeinfo;
    merge countvars&surv. (in=a) codefmt (in=b) lengths;
    by code_nam;
    if a;
    length codefmt2 lenstatement $30. ;

    * Blank code names are text variables.  These are all 40 characters;
    if code_nam = '' then do;
      lencode = &&&surv._maxchar;
      flag = "$";
    end;

    codefmt2 = flag||sas_fmt||".;";
    codefmt2 = compress(codefmt2);

    if flag = "$" then lenstatement = flag||put(lencode,3.)||".;";
    else lenstatement = "8.;";
    lenstatement = compress(lenstatement);

  run;

  * use the codeinfo dataset to create macro variables holding ;
  * the code name, code format name, and the count of variables;
  * for each code name.  We will use this in the data null.    ;
  data codeinfo;
    set codeinfo;
    length num $6. code fmt len $40. ;

    num = "num"||left(compress(_n_));
    code = "code"||left(compress(_n_));
    fmt = "fmt"||left(compress(_n_));
    len = "len"||left(compress(_n_));
    
    num = compress(num);
    code = compress(code);
    fmt = compress(fmt);
    len = compress(len);

    call symput(num,count);
    call symput(code,code_nam);
    call symput(fmt,codefmt2);
    call symput(len,lenstatement);

  run;

  * transpose the survey-year dataset so that each code name   ;
  * will have a list of associated variables.                  ;
  proc transpose data=&surv. out=&surv.trans;
    by code_nam;
    var sas_name;
  run;

  data new&surv;
    merge &surv.trans (in=a) codeinfo;
    by code_nam;
    if a;

    length row1 - row&&&mod5&surv $75. ;
    

    * create blank extra name variables if necessary;
    %if &&&all&surv > &&&max&surv %then %do;
      length name&&&firstblank&surv - name&&&all&surv $10.;   
      %do i = &&&firstblank&surv %to &&&all&surv;
        name&i = '';
      %end;
    %end;

    * create an array of all of the sas names associated  ;
    * with each code name.  These variables were renamed  ;
    * col1 - colxx when we transposed.                    ;
    array names (&&&max&surv) $ col1 - col&&&max&surv;

    * create an array that will hold the rows we plan to  ;
    * write to the programs applyformats.sas & lengths.sas;
    array rows (&&&mod5&surv) $ row1 - row&&&mod5&surv;

    * trimnames array holds compressed, left-justified    ;
    * sas names.                                          ;
    array trimnames (*) $ name1 - name&&&all&surv  ;

    * create newer variables with the sas names compressed;
    * and left-justified.                                 ;
    do i = 1 to dim(names);
      trimnames(i) = left(compress(names(i)));
    end; 

    do i = 1 to &&&mod5&surv.;
      rows(i) = trimnames(5*i - 4)||" "||
                trimnames(5*i - 3)||" "||
                trimnames(5*i - 2)||" "||
                trimnames(5*i - 1)||" "||
                trimnames(5*i);
      rows(i) = trim(rows(i));
    end;

    * create macro variables to hold each row associated with;
    * each code name.                                        ;
    %do i = 1 %to &&&mod5&surv;
      length row&i.out $10.;
      row&i.out = "row_"||left(compress(_n_))||"_&i.";
      row&i.out = compress(row&i.out);
  
      * output each row to a global macro variable;
      call symput(row&i.out,row&i.);
    %end;

    drop col1 - col&&&max&surv _name_ _label_ i;
  run;

  * create a sas program to assign the formats to the correct variables;
  * We use all of the macro variables previously created to do this.   ;
  data _null_;
    set new&surv;
    array rows (&&&mod5&surv) $ row1 - row&&&mod5&surv;

    file "&fmtpath.\applyformats_&surv..sas";

    * set up the program;
    if _n_ = 1 then do;
      put "/***************************************************************/";
      put "/* PROGRAM:  applyformats_&surv..sas                              */";
      put "/* CREATED:  &sysdate &systime by dataproperties.sas               */";
      put "/* PURPOSE:  To apply the appropriate format to each &year      */";
      put "/*           sestat variable.                                  */";
      put "/***************************************************************/";
      put //;
    end;

    * applysformats.sas is only being written for base variables so we leave;
    * out any blank code names.  Also leave out refid codes since we will   ;
    * not be applying a format to refids.;      
    if code_nam not in ('','REFID_CODES') then do;

      * write format statement and the first row;
      put @1 "FORMAT" @8 rows(1);

      * write out any remaining blank rows;
      do i = 2 to &&&mod5&surv;
        if rows(i) ne '' then put @8 rows(i);
      end;
 
      * put the format in the last line;
      put @8 codefmt2;

    end;

    * Now do the same thing to create length statements for base variables;
    file "&fmtpath.\base_lengths_&surv..sas";

    * set up the program;
    if _n_ = 1 then do;

      put "/***************************************************************/";
      put "/* PROGRAM:  base_lengths_&surv..sas                            */";
      put "/* CREATED:  &sysdate &systime by dataproperties.sas               */";
      put "/* PURPOSE:  To apply the appropriate length to each &year      */";
      put "/*           sestat variable.                                  */";
      put "/***************************************************************/";
      put //;

    end;

    * base_lengths.sas is only being written for base variables so we leave ;
    * out any blank code names.                                             ;      
    if code_nam not in ('') then do;

      * write format statement and the first row;
      put @1 "LENGTH" @8 rows(1);

      * write out any remaining blank rows;
      do i = 2 to &&&mod5&surv;
        if rows(i) ne '' then put @8 rows(i);
      end;
 
      * put the format in the last line;
      put @8 lenstatement;

    end;


    * Now do the same thing to create length statements for base variables;
    file "&fmtpath.\text_lengths_&surv..sas";

    * set up the program;
    if _n_ = 1 then do;
      put "/***************************************************************/";
      put "/* PROGRAM:  text_lengths_&surv..sas                            */";
      put "/* CREATED:  &sysdate &systime by dataproperties.sas               */";
      put "/* PURPOSE:  To apply the appropriate length to each &year      */";
      put "/*           sestat variable.                                  */";
      put "/***************************************************************/";
      put //;
    end;

    * text_lengths.sas is only being written for base variables so we keep  ;
    * only blank code names.                                                ;      
    if code_nam in ('') then do;

      * write format statement and the first row;
      put @1 "LENGTH" @8 rows(1);

      * write out any remaining blank rows;
      do i = 2 to &&&mod5&surv;
        if rows(i) ne '' then put @8 rows(i);
      end;
 
      * put the format in the last line;
      put @8 lenstatement;

    end;

  run;

  * Include the programs we just created.  This way all        ;
  * problems with them will show up in the log of this program.;

  data test&surv;
    %include "&fmtpath.\base_lengths_&surv..sas";
    %include "&fmtpath.\text_lengths_&surv..sas";

    * create arrays for numeric and character variables.;
    array nums(*) _numeric_;
    array chars(*) _character_;
    
    * initialize everything to be missing;
    do i = 1 to dim(nums);
      nums(i) = .;
    end;
    do i = 1 to dim(chars);
      chars(i) = '';
    end;

    %include "&fmtpath.\applyformats_&surv..sas";

    %include "&fmtpath.\base_labels_&surv..sas";
    %include "&fmtpath.\text_labels_&surv..sas";

    %include "&fmtpath.\base_keepvars_&surv..sas";
    %include "&fmtpath.\text_keepvars_&surv..sas";


  run;

  data _null_;
    file print;
    put "Files created for &survey on &sysdate, &systime";
    put "&fmtpath.\applyformats_&surv..sas";
    put "&fmtpath.\base_lengths_&surv..sas";
    put "&fmtpath.\text_lengths_&surv..sas";
  run;

%mend applyformats;
%applyformats(surv=sdr);
%applyformats(surv=nscg);
%applyformats(surv=nsrcg);
%applyformats(surv=nscgn);
