*****************************************************************************;
* PROGRAM:  rfd_prep.sas                                                    *;
* TASK:     SESTAT - NSF, Contractor Specifications (8797-110)              *;
* PURPOSE:  Bring in all metadata information and create the datasets we    *;
*           need to generate the RFD files for contractor specificaitons.   *;
*                                                                           *;
* WRITTEN:  By Maria Cupples Hudson after 2006 contractor specifications    *;
*           Stealing liberally from Keith Rathbun                           *;
*                                                                           *;
* MODifIED:                                                                 *;
*                                                                           *;
* INPUTS:  1) sestat_metadata.mdb - Sestat Metadata Database                *;
*             a) DICTIONARY Table                                           *;
*             b) SDR, NSCG, and NSRCG Tables for this year                  *;
*                                                                           *;
* OUTPUTS: SAS datasets:                                                    *;
*          1) allinfo to be used to create RFD files and for other-specify  *;
*          2) allwithcodes_sdr to be used to create SDR RFD files           *;
*          3) allwithcodes_nscg to be used to create NSCG RFD files         *;
*          4) allwithcodes_nsrcg to be used to create NSRCG RFD files       *;
*                                                                           *;
* NOTES:                                                                    *;
*                                                                           *;
*****************************************************************************;

* include the program that sets all parameters and libnames for specs ;
%include "J:\06850\Common\Task411_2010_Specs\set_parameters.sas";


* this macro acts as a find/replace;
* Note that this is slightly different than the one in the set_parameters.sas;
* program because we need to account for left and right parentheses.         ;
%macro replaceit(chkvar=,search=,replace=);

  * look for a search term indicating left or right parenthenses;
  * change the search accordingly (these are difficult to pass to a macro);
  %if "&search" = "left paren" %then %let search = (;
  %if "&search" = "right paren" %then %let search = );

  * length of search string;
  %let lenrep = %length(&replace);
  %let lenrep2 = %eval(&lenrep + 1);

  /* find the first occurrance of the phrase to b replaced;*/
  textpos = index(upcase(&chkvar), upcase("&search"));


  /* We run this loop until there are no more occurances (i.e. textpos=0);
  * The i=1 to 1000 will ensure that we do not wind up with an infinite ;
  * loop, but in actuality, the loop will stop once textpos=0.          ;*/
  do i=1 to length(&chkvar) while ( 0 < textpos <= length(&chkvar));

    if textpos=1 then do;
      if length(&chkvar)=1 then &chkvar="&replace";
      else &chkvar = "&replace"||substr(&chkvar,length("&search")+1,length(&chkvar)-(length("&search")+1));
    end;
    else if textpos=length(&chkvar) then 
      &chkvar = substr(&chkvar,1,length(&chkvar)-1)||"&replace";
    else if 1 < textpos < length(&chkvar) then 
      &chkvar = substr(&chkvar, 1, (textpos - 1))||
                "&replace"||substr(&chkvar,(textpos +length("&search")));

    /* reset the text position before looping again */
    textpos = index(upcase(&chkvar), upcase("&search"));

  end;

%mend replaceit;


******************************************************************************;
*   Step 1:  Bring in the data that applies to all surveys                   *;
*            i.e., All Code and Dictionary information.                      *;
******************************************************************************;


* Bring in the codeval table from the metadata;
* Clean up and add extra codes as needed for the purpose of contractor specs.   ;
data codes;
  set metadata.codeval;

  * Refid does not need any values.  Remove anything there.;
  if code_nam = 'REFID_CODES' then codeval = '';

  * Create a variable print position.  This simply ensures that the Yes/No codes;
  * will print in the order Yes, No, everthing else.                            ;
  if codeval eq 'Y' then printpos=1;
  else if codeval eq 'N' then printpos=2;
  else printpos = 99;

  * Delete code values that make no sense or do not apply here;
  if code_nam = 'YES_NO_CODES' and codeval = '3' then delete;
  if code_nam = 'REFYR_CODES' and codeval ne "&year" then delete;
  if code_nam = 'SURVEY_MODES' and codeval = '7' then delete;

run;

proc sort data=codes;  by code_nam codeval;  run;

* This dataset contains any extra code names and/or code values that we need to ;
* add for the sake of contractor specs.  Include here referencs to appendices,  ;
* year ranges that are dependent on which survey year this is.                  ;
data extra;
  length code_nam $30. codeval $15. descr $80.;

  * in general we ae creating blank codevalues and default print positions of 99;
  * any other cases will be addressed for the specific code names as necessary. ;
  code_nam = '';
  printpos = 99;

  * create a code name to point to numeric coding instructions;
  code_nam = 'NUMERIC';
  descr    = 'Refer to instructions for coding numeric values.';
  output;

  * create a code ame to point to instructions for handling text;
  code_nam = 'TEXT';
  descr = 'See instructions for coding text fields';
  output;

  * There are several appendices we will need to refer to in the specs;
  code_nam = 'APPENDA';
  descr    = 'Refer to Appendix A-- Education Codes for coding instructions.';
  output;

  code_nam = 'APPENDB';
  descr    = 'Refer to Appendix B-- Institution Codes for coding instructions.';
  output;

  code_nam = 'APPENDC';
  descr    = 'Refer to Appendix C-- Location Codes for coding instructions.';
  output;

  code_nam = 'APPENDD';
  descr    = 'Refer to Appendix D-- Occupation Codes for coding instructions.';
  output;

  * for all year variables there is a minimum allowable value of 61 years;
  * prior to the survey year and a maximum of the survey year.           ;
  code_nam = 'YEAR_CODES';
  codeval  = "&yearmin - &year";
  descr    = 'Year';
  output;   

run;

* Combine the two datasets, then sort to create the dataset we will merge onto  ;
* the variable level dataset to create the final with-codes dataset.            ;
data codes;
  set codes extra;
run;

* sort the resulting dataset in preparation for the merge later in the program. ;
proc sort data=codes;
 by code_nam printpos codeval;
run;

* Bring in the data dictionary.  We will add all survey-specific information to;
* this dataset inside the macro survs.                                         ;
data dict;
  set metadata.dictionary (keep = sas_name col_name new_name
                                  descripti basetext type datasize
                                  datascale dataprecision notes);

  * replace date-holders with specific dates in the variable descriptions      ;
  %replaceit(chkvar=descripti,search=[survey reference date],replace=&refdate);
  %replaceit(chkvar=descripti,search=[survey reference week],replace=&refdate);
  %replaceit(chkvar=descripti,search=[previous survey reference date],replace=&prefdate);
  %replaceit(chkvar=descripti,search=[current survey month/year],replace=&refmonyr);
  %replaceit(chkvar=descripti,search=[current survey reference month/year],replace=&refmonyr);
  %replaceit(chkvar=descripti,search=[previous survey month/year],replace=&prefmonyr);
  %replaceit(chkvar=descripti,search=[previous survey reference month/year],replace=&prefmonyr);
  %replaceit(chkvar=descripti,search=[prior calendar year],replace=&pcyear);
  %replaceit(chkvar=descripti,search=[previous calendar year],replace=&pcyear);

run;

proc sort data=dict;  by sas_name;  run;

******************************************************************************;
*   Step 2:  Bring in the survey-year information, keeping only what we need *;
*            to create contractor specifications.  Add all of this to the    *;
*            dictionary information to create a master dataset to be used in *;
*            all other contractor specs programs.                            *;
******************************************************************************;

%macro survs(surv=);
  %let survey=&surv.&yr;

  * Bring in the survey-year specific information.  Notice that we are only   ;
  * using the code name for the contractor specifications.  Notice that we are;
  * using the contractor name as the final sas name to be used.  If a variable;
  * gets renamed between specs and load files the sas_name will be changed to ;
  * reflect as much.                                                          ;
  data &surv; 
    set metadata.&survey (where=(index(upcase(createdby),'RECODE')=0
                                 and source not in ('N/A','RECODE','SED')));
    rename cont_name = cont_name_&surv
           source = source_&surv
           sortid = sortid_&surv
           createdby = createdby_&surv
           code_nam_contractor = code_nam_&surv
           question = question_&surv
           notes = notes_&surv
           question = question_&surv;

  run;

  proc sort data=&surv;  by cont_name_&surv;  run;

  title "Frequency of created-by variable in the metadata on the dataset for &survey";
  proc freq data = &surv;
    table createdby_&surv;
  run;
  title;

  proc sort data=dict;  by sas_name;  run;
  proc sort data=&surv;  by sas_name;  run;

  data dict;
    merge dict  (in=dict) 
          &surv (in=&surv
                 keep = sas_name cont_name_&surv source_&surv sortid_&surv
                        code_nam_&surv notes_&surv question_&surv createdby_&surv);
    by sas_name;

    * flag anything that appears in the survey table but not in the dictionary;
    if &surv and not dict then problem_&surv = 1;
    else problem_&surv = 0;

    * if a variable is not in the survey table then set the source to n/a;
    if not &surv then source_&surv = 'N/A';

    if basetext = "T" then code_nam_&surv = 'TEXT';
   
  run;

  title "Frequency of merge checks for survey &survey onto the dictionary";
  title2 "This should be zero for all observations.  Specific observations printed below.";
  proc freq data=dict;
    table problem_&surv / nocum;
  run;
  title;
  
  title "Variables on the &survey table that were not in the dictionary";
  proc print data=dict (where=(problem_&surv = 1));
    var sas_name col_name new_name descripti;
  run;
  title;


%mend survs;
%survs(surv=sdr);
%survs(surv=nscg);
%survs(surv=nsrcg);
%survs(surv=nscgn);

******************************************************************************;
*   Step 3:  Now that we have information from the dictionary and all 3      *;
*            surveys we need to run a few checks and then create variables we*;
*            will need later in the specs creation process.                  *;
******************************************************************************;

data allinfo;
  set dict;

  * keep only variables that apply to at least one of the current surveys;
  if source_sdr ne 'N/A' or source_nscg ne 'N/A' or source_nsrcg ne 'N/A';

  array contnames (*) $ cont_name_sdr cont_name_nscg cont_name_nsrcg cont_name_nscgn;
  array codenames (*) $ code_nam_sdr  code_nam_nscg  code_nam_nsrcg  code_nam_nscgn;
  length cont_name $50.;

  * initialize the contractor-name-problem-flag to zero;
  contprob = 0;

  * Set the contractor name to the value of one of the nonblank cont_names;
  do i = 1 to dim(contnames);
   if cont_name = '' and contnames(i) ne '' then cont_name = contnames(i);
  end;  

  * Now make sure all the nonblank contractor names match.;
  do i = 1 to dim(contnames);
   if contnames(i) ne '' and cont_name ne contnames(i) then contprob = 1;
  end;  

  * make sure that the contractor name has somthing in in;
  if cont_name = '' then contprob = 0;


  * Change records referring to education, occupation, and location codes.      ;
  * These code names have hundreds of values.  We will instead refer to the     ;
  * appendices.                                                                 ;
  do i = 1 to dim(contnames);
    if codenames(i) in ('EDUCATION_CODES_CONT','EDUCATION_CODES_NEW')
      then codenames(i) = 'APPENDA';
    else if codenames(i) in ('EDUCATION_INST_CODES') 
      then codenames(i) = 'APPENDB';
    else if codenames(i) in ('LOCATION_CODES') 
      then codenames(i) = 'APPENDC';
    else if codenames(i) in ('OCCUPATION_CODES_CONT','OCCUPATION_CODES_NEW',
                             'OCC_CODES_2010_CONT','OCCUPATION_CODES_2010')
      then codenames(i) = 'APPENDD';
  end;

  * remove left and right parentheses from any description that is over 80     ;
  * characters long. Otherwise it really messes up the RFD creation process.   ;
  if length(descripti) > 80 then do;
    %replaceit(chkvar=descripti,search=left paren,replace=[);
    %replaceit(chkvar=descripti,search=right paren,replace=]);
  end;

run;

title "Frequency of mismatched or missing contractor names";
title2 "All of these should be zero.  Problem cases printed below.";
proc freq data=allinfo;  
  table contprob / nocum nopercent;
run;
title;

title "Places where contractor name was different between surveys.";
title2 "This is a problem with the database.  You will have to fix and rerun.";
proc print data=allinfo (where=(contprob = 1));
  var sas_name cont_name_sdr cont_name_nscg cont_name_nsrcg cont_name_nscgn;
run;
title;

* Clean up and finalize the dataset with all variable-level information.  This;
* dataset will be used to create edit/imputation flags as well as the other-  ;
* specify information.                                                        ;
data rfd.allinfo_v&vv;
  set allinfo;
  drop sas_name cont_name_sdr cont_name_nscg cont_name_nsrcg cont_name_nscgn
       contprob problem_sdr problem_nscg problem_nsrcg problem_nscgn;
  rename cont_name = sas_name;
run;


******************************************************************************;
*   Step 4:  Now we will separate the data into 3 datasets, one for each     *;
*            survey.  Then we add all possible values that each variable can *;
*            take.   These will be used to create the RFD files.             *;
******************************************************************************;

%macro addcodes(surv=);

  * Sort the variable-level dataset, keeping on the observations and variables ;
  * that apply to this survey.                                                 ;
  proc sort data = allinfo  (where  = (source_&surv not in ('N/A','SED','RECODE')))
            out  = all&surv (keep   =  sas_name        col_name        new_name 
                                       descripti       datascale       datasize
                                       dataprecision   basetext        type 
                                       notes           notes_&surv     source_&surv
                                       code_nam_&surv  question_&surv  sortid_&surv
                                       createdby_&surv
                             rename = (source_&surv    = source
                                       sortid_&surv    = sortid
                                       code_nam_&surv  = code_nam
                                       question_&surv  = question
                                       notes_&surv     = notes_surv
                                       question_&surv  = question
                                       createdby_&surv = createdby));
     by code_nam_&surv;
  run;

  * Now merge on all possible codevalues for each sas_name using the code name.;
  * This is a many-to-many merge.                                              ;
  data allwithcodes_&surv.;
    set all&surv;

    * set up a temporary merge variable and set it equal to the code name.     ;
    length temp $30.;
    temp = code_nam;

    * for each observation, we will merge on all code values associated with   ;
    * the code name applied to that variable.                                  ;    
    do k = 1 to nobs;
      set codes point=k nobs=nobs;
      if temp = code_nam then output;
    end;

  run;

  data rfd.allwithcodes_&surv._v&vv;
    set allwithcodes_&surv.;

    * delete codevalues that are not valid for contractor specs;
    if sas_name="ALTPRI" and codeval="0" then delete;
    if sas_name="NRREA" and codeval="0" then delete;
    if sas_name="WAPRI" and codeval="Z" then delete;
    if sas_name="PDPRI" and codeval="0" then delete;
    if sas_name="PATHPRI" and codeval="00" then delete;
    if sas_name="INPRI" and codeval="0" then delete;
    if sas_name="ADQPRI" and codeval="00" then delete;

    * Get rid of all values other than Y for the variable VALIND;
    if sas_name='VALIND' and codeval ne "Y" then delete;

    * The value of no specific degree does not apply to EARNED degrees;
    if code_nam = 'DEGREE_CODES' and sas_name in ('MRDG','D2DG','D3DG','D4DG','D5DG','RCGDG')
      and codeval = '0' then delete;

    * change descriptions for certain codevals;
    if code_nam="INCOME_CODES" and codeval="1" then descr = "No Income";

    * Age must be between 0 and 75.;
    if sas_name="AGE" and codeval not in ('96','97','98','99') then do;
      codeval = "00-75";
      descr = "Age";
    end;

    * DIFAGE (the age at which difficulty started) must be between 1 and 75.;
    if sas_name="DIFAGE" and codeval not in ('96','97','98','99') then do;
      codeval = "01-75";
      descr = "Age";
    end;

    * Set the acceptable range for birth year to be in line with age;
    if sas_name="BIRYR" and codeval not in ('9996','9997','9998','9999') then do;
      codeval = "&newbirlow.-&year.";
      descr = "Year";
    end;

    * The year permenance was acheived for non-us-born must be at least one year;
    * after birth so the acceptable range is the same as for birth with the min ;
    * one year later.  The max is the current year.                             ;
    if sas_name="PERMVYR" and codeval not in ('9996','9997','9998','9999') then do;
      codeval = "&minplus1.-&year.";
      descr = "Year";
    end;

    * The number of children for each age category is generally top-coded to 9. ;
    * Make sure the code values reflect this.                                   ;
    if sas_name in ('CHU2','CH25','CH611','CH1218','CH19') and codeval 
      not in ('96','97','98','99') then do;
      codeval = "00 - 09";
      descr = "0 through 9 (or more)";
    end;

    * The year entering the US will have the same minimum as birth year;
    * and the current year as the top of the range;
    if sas_name="FNUSYR" and codeval not in ('9996','9997','9998','9999') then do;
      codeval = "&newbirlow.-&year.";
      descr = "Year";
    end;

    * Weeks salary is based on (if not 52) per year must be between 1 and 51.;
    if sas_name="WKSLYR" and codeval not in ('96','97','98','99') then do;
      codeval = "01-51";
      descr = "Number of Weeks";
    end;

    * Also note that WLSLYR cannot take on the value 96 as other 2_0_CODES;
    * variables do;
    if sas_name="WKSLYR" and codeval = '96' then delete;

    * remove 00/0000 as options for non-postdoc end month/year variables;
    if index(upcase(col_name),'POST_DOC')=0 or 
      (index(upcase(col_name),'POST_DOC')>0 and index(upcase(col_name),'ENDED')=0) then do;
      if code_nam = "MONTH_CODES" and codeval = '00' then delete;
      if code_nam = "YEAR_CODES" and codeval = '0000' then delete;
    end;

    * most recent degree cannot be equal to 5 (Other) for 2008 NSCG;
    %if ("&year" = "2008" and "&surv" = "nscg") %then %do;
      if sas_name = 'MRDG' and codeval = '5' then delete;
    %end;

    * For SDR and NSCG the most recent degree must have been earned in the;
    * interval between the previous survey and the current one.;
    %if ("&surv" = "nscg" or "&surv" = "sdr") %then %do;
      if sas_name = "MRYR" and codeval = "&yearmin - &year" then codeval = "&pyear - &year";
    %end;

    * Create flags for whether or not a variable is subject to edit/impuation;
    * Initialze to 1 for all non-verbatim contractor-delivered variables;
    if createdby in ("Survey Contractor","Questionnaire") and basetext ne "T" then do;
      sub2imp = 1;
      sub2edit = 1;

      * these are never subject to imputation/editing;
      if sas_name in ("REFID","SURID","REFYR","WTSURVY","COHORT",'VALIND',
                      'SMODEP','SRVMODE',&&&surv._critical,"EDMOM","EDDAD",
                      "MRDG","D2DG","D3DG","D4DG","D5DG","BAIND") then do;
        sub2imp = 0;
        sub2edit = 0;
      end;

      * reported codes are not subject to imputation/editing;
      if index(upcase(descripti),"REPORTED")>0 then do;
        sub2imp = 0;
        sub2edit = 0;
      end;

      * institution codes, carnegie codes, pub/private, and state;
      * are subject to edits but not to impuation;
      if index(upcase(descripti),"CARNEGIE")>0 or 
         index(upcase(descripti),"INSTITUTION CODE")>0 or
         index(upcase(descripti),"PUBLIC/PRIVATE")>0 or
         (index(upcase(descripti),"STATE/COUNTRY CODE")>0 and
           (index(upcase(descripti),"EMPLOYER")>0 or index(upcase(descripti),"AWARDING")>0))
        then do;
        sub2imp = 0;
      end;

    end;

    * No other variables are subject to edit/imputation;
    else do;
      sub2imp = 0;
      sub2edit = 0;
    end;



    * Take out MISSING values for all variables that are not allowed to be missing;
    * i.e., critical complete, subject to edit/imputation, or others specified above;

    if (sub2imp = 1 or sas_name in (&&&surv._critical,&&&surv._nomiss))
      and index(upcase(descr),"MISSING")>0 then delete;
      
    * Take out SUPPRESSED values for all variables that are not allowed to be supressed;
    * i.e., critical complete and those specified above;
    if sas_name in (&&&surv._critical,&&&surv._nosup) and
      (index(upcase(descr),'SUPPRES')>0 or index(upcase(descr),'SURVEY EXCLUSION')>0)
      then delete;

    * Take out LOGICAL SKIP values for all variables that are not allowed to be skipped;
    * i.e., critical complete and those specified above;
    if sas_name in (&&&surv._critical,&&&surv._noskip) and 
      index(upcase(descr),'LOGIC')>0 and index(upcase(descr),'SKIP')>0
      then delete;

    * replace date-holders with specific dates in the code value descriptions    ;
    %replaceit(chkvar=descr,search=[survey reference date],replace=&refdate);
    %replaceit(chkvar=descr,search=[survey reference week],replace=&refdate);
    %replaceit(chkvar=descr,search=[previous survey reference date],replace=&prefdate);
    %replaceit(chkvar=descr,search=[current survey month/year],replace=&refmonyr);
    %replaceit(chkvar=descr,search=[current survey reference month/year],replace=&refmonyr);
    %replaceit(chkvar=descr,search=[previous survey month/year],replace=&prefmonyr);
    %replaceit(chkvar=descr,search=[previous survey reference month/year],replace=&prefmonyr);
    %replaceit(chkvar=descr,search=[prior calendar year],replace=&pcyear);
    %replaceit(chkvar=descr,search=[previous calendar year],replace=&pcyear);
    %replaceit(chkvar=descr,search=[4 weeks prior to survey reference date],replace=&minus4wks);
    %replaceit(chkvar=descr,search=[survey reference month/day],replace=&refmonthday);

    %replaceit(chkvar=question,search=[survey reference date],replace=&refdate);
    %replaceit(chkvar=question,search=[survey reference week],replace=&refdate);
    %replaceit(chkvar=question,search=[previous survey reference date],replace=&prefdate);
    %replaceit(chkvar=question,search=[current survey month/year],replace=&refmonyr);
    %replaceit(chkvar=question,search=[current survey reference month/year],replace=&refmonyr);
    %replaceit(chkvar=question,search=[previous survey month/year],replace=&prefmonyr);
    %replaceit(chkvar=question,search=[previous survey reference month/year],replace=&prefmonyr);
    %replaceit(chkvar=question,search=[prior calendar year],replace=&pcyear);
    %replaceit(chkvar=question,search=[previous calendar year],replace=&pcyear);
    %replaceit(chkvar=question,search=[4 weeks prior to survey reference date],replace=&minus4wks);
    %replaceit(chkvar=question,search=[survey reference month/day],replace=&refmonthday);

    * replace page-number holders for job category questions                     ;
    if index(upcase(question),"JOB CATEGORY")>0 then do;
      %replaceit(chkvar=question,search=[XX - XX],replace=&&&surv._occ_pg);
      %replaceit(chkvar=question,search=[XX-XX],replace=&&&surv._occ_pg);
    end;

    * replace page-number holders for field of study questions                   ;
    if index(upcase(question),"FIELD OF STUDY")>0 then do;
      %replaceit(chkvar=question,search=[XX - XX],replace=&&&surv._ed_pg);
      %replaceit(chkvar=question,search=[XX-XX],replace=&&&surv._ed_pg);
    end;

    * replace previous-question holders for question where this is known.        ;
    if sas_name in ('NRREA','NRSEC') then do;
      %replaceit(chkvar=descr,search=[the previous question],replace=question &&&surv._notrel);
      %replaceit(chkvar=question,search=[the previous question],replace=question &&&surv._notrel);
    end;
    if sas_name in ('WAPRI','WASEC') then do;
      %replaceit(chkvar=descr,search=[the previous question],replace=question &&&surv._wact);
      %replaceit(chkvar=question,search=[the previous question],replace=question &&&surv._wact);
    end;
    if sas_name in ('WTREASN') then do;
      %replaceit(chkvar=descr,search=[the previous question],replace=question &&&surv._train);
      %replaceit(chkvar=question,search=[the previous question],replace=question &&&surv._train);
    end;
    if sas_name in ('PDPRI','PDSEC') then do;
      %replaceit(chkvar=descr,search=[the previous question],replace=question &&&surv._pdoc);
      %replaceit(chkvar=question,search=[the previous question],replace=question &&&surv._pdoc);
    end;
    if sas_name in ('CCPRI','CCSEC') then do;
      %replaceit(chkvar=descr,search=[the previous question],replace=question &&&surv._cc);
      %replaceit(chkvar=question,search=[the previous question],replace=question &&&surv._cc);
    end;
    if sas_name in ('CMPRI','CMSEC') then do;
      %replaceit(chkvar=descr,search=[the previous question],replace=question &&&surv._2us);
      %replaceit(chkvar=question,search=[the previous question],replace=question &&&surv._2us);
    end;


    keep sas_name code_nam codeval descr basetext col_name createdby dataprecision
         datascale datasize descripti new_name notes notes_surv printpos 
         question sortid source sub2edit sub2imp type;

  run;

  title "These variables may have generic information still in the DESCR text";
  title2 "Check and either fix manually or rerun so that only survey-year specific information is dispayed";
  proc freq data=rfd.allwithcodes_&surv._v&vv;
    where index(descr,'[') > 0 or index(descr,']') > 0;
    table sas_name * descr / list missing nocum;
  run;

  title "These variables may have generic information still in the QUESTION text";
  title2 "Check and either fix manually or rerun so that only survey-year specific information is dispayed";
  proc freq data=rfd.allwithcodes_&surv._v&vv;
    where index(question,'[') > 0 or index(question,']') > 0;
    table sas_name * question / list missing nocum;
  run;


  proc sort data=rfd.allwithcodes_&surv._v&vv noduprec;
    by sas_name code_nam codeval;
  run;


  * Now, compare this version to the last version (if applicable) and create;
  * text output with all changes.;
  %if "&pversion" ne "0.0" %then %do;


    proc sort data = rfd.allwithcodes_&surv._v&pv (keep = sas_name code_nam 
                                                         basetext col_name 
                                                         descripti new_name printpos 
                                                         question sortid source type
                                                        /* sub2edit sub2imp createdby */
                                                         )
               out = old_var_info nodupkey;
      by sas_name ;
    run;

    proc sort data = rfd.allwithcodes_&surv._v&pv (keep=sas_name code_nam codeval descr)
               out = old_code_info noduprec;
      by sas_name codeval;
    run;

    proc sort data = rfd.allwithcodes_&surv._v&vv (keep = sas_name code_nam  
                                                         basetext col_name  
                                                         descripti new_name printpos 
                                                         question sortid source type
                                                        /* sub2edit sub2imp createdby */
                                                         )
               out = new_var_info nodupkey;
      by sas_name ;
    run;

    proc sort data = rfd.allwithcodes_&surv._v&vv (keep=sas_name code_nam codeval descr)
               out = new_code_info noduprec;
      by sas_name codeval;
    run;

   %macro compval(val=);
     if &val._old = &val._new then &val._comp = 1;
     else do;
       &val._comp = 0;
       anychange = 1;
     end;
   %mend compval;
  
    data varcompare;
      merge old_var_info (in=old
                          rename=(code_nam     = code_nam_old
                                  basetext     = basetext_old
                                  col_name     = col_name_old
                                  question     = question_old
                                  sortid       = sortid_old
                                  source       = source_old
  /*                                sub2edit     = sub2edit_old
                                  sub2imp      = sub2imp_old
                                  createdby    = createdby_old  */
                                  type         = type_old))
            new_var_info (in=new
                          rename=(code_nam     = code_nam_new
                                  basetext     = basetext_new
                                  col_name     = col_name_new
                                  question     = question_new
                                  sortid       = sortid_new
                                  source       = source_new
  /*                                sub2edit     = sub2edit_new
                                  sub2imp      = sub2imp_new
                                  createdby    = createdby_new */
                                  type         = type_new));
      by sas_name;
  
      length mergeflag $75.;
  
      if old and new then do;
  
        mergeflag = "Variable in Both Version &version and Version &pversion";
  
        %compval(val=code_nam);
        %compval(val=basetext);
        %compval(val=col_name);
        %compval(val=question);
        %compval(val=sortid);
        %compval(val=source);
  /*      %compval(val=sub2edit);
        %compval(val=sub2imp);
        %compval(val=createdby);*/
        %compval(val=type);
  
      end;
  
      else if old then mergeflag = "Variable Dropped from Version &version";
      else if new then do;
        mergeflag = "Variable New in Version &version";
        anychange = 2;
      end;
  
    run;
  
    data codecompare;
      merge old_code_info (in=old
                           rename=(descr        = descr_old))
            new_code_info (in=new
                           rename=(descr        = descr_new));
      by sas_name code_nam codeval;

  
      length mergeflag $75.;
  
      if old and new then do;
  
        mergeflag = "Code Name and Code Value in Both Version &version and Version &pversion";
  
        %compval(val=descr);
  
      end;
  
      else if old then mergeflag = "Code Name or Value Dropped from Version &version";
      else if new then do;
        mergeflag = "Code Name or Value New in Version &version";
        anychange = 2;
      end;
      
  
    run;

    proc freq data=varcompare;
      table code_nam_comp / noprint list missing nocum out=comp1;
      table basetext_comp / noprint list missing nocum out=comp2;
      table col_name_comp / noprint list missing nocum out=comp3;
      table question_comp / noprint list missing nocum out=comp4;
      table sortid_comp / noprint list missing nocum out=comp5;
      table source_comp / noprint list missing nocum out=comp6;
      table type_comp / noprint list missing nocum out=comp7;
  /*    table sub2edit_comp / noprint list missing nocum out=comp8;
      table sub2imp_comp / noprint list missing nocum out=comp9;
      table createdby_comp / noprint list missing nocum out=comp10;*/
    run;

    proc freq data=codecompare;
      table descr_comp / noprint list missing nocum out=comp20;
    run;

    data allcomps;
      set   comp1  (where=(code_nam_comp = 1)   in=in1)
            comp2  (where=(basetext_comp = 1)   in=in2)
            comp3  (where=(col_name_comp = 1)   in=in3)
            comp4  (where=(question_comp = 1)   in=in4)
            comp5  (where=(sortid_comp = 1)     in=in5)
            comp6  (where=(source_comp = 1)     in=in6)
            comp7  (where=(type_comp = 1)       in=in7)
  /*          comp8  (where=(sub2edit_comp = 1)   in=in8)
            comp9  (where=(sub2imp_comp = 1)    in=in9)
            comp10 (where=(createdby_comp = 1)  in=in10)*/
            comp20 (where=(descr_comp = 1)      in=in20);
  
      length attribute $30.;
  
      if in1  then attribute = "Code Name";
      if in2  then attribute = "Base/Text Indicator";
      if in3  then attribute = "Column Name";
      if in4  then attribute = "Question Text";
      if in5  then attribute = "Sort Order";
      if in6  then attribute = "Source/Question #";
      if in7  then attribute = "Variable Type";
  /*    if in8  then attribute = "Subject to Edit Flag";
      if in9  then attribute = "Subject to Imputation Flag";
      if in10 then attribute = "Created-By Flag"; */
      if in20 then attribute = "Code Value Description";
  
      keep attribute count percent;
  
    run;

    * Delete the file we are about to write to, just in case;
    x "del &rfdpath.\&surv.\RDF_Changes_Between_Versions_&pv._and_&vv..txt";

    data _null_;
      set allcomps;

      file "&rfdpath.\&surv.\Changes_between_versions_&pv._and_&vv..txt";
  
      if _n_ = 1 then do;
        put "Summary of Variable and Code Name Attribute Matching between Version &version and Version &pversion";
        put "For &year &surv among all Variables and Code Names in Both Versions";
        put "Details of Nonmatches will Print Below.";
        put //;
        put @1 "Variable Attribute"   @35 "Number Matching"   @55 "Percent Matching";
        put @1 "------------------"   @35 "---------------"   @55 "----------------";
        put ;
      end;

      put @1 attribute @35 count comma10. @55 percent 5.2;

    run;


    %macro printcomp(val=,vdesc=);

      if &val._comp = 0 then do;
        put;
        put @5 "&vdesc Changed.";
        put @7 "Previous Value:  " &val._old;
        put @7 "New Value:       " &val._new;
      end;
  
    %mend printcomp;

    data _null_;
      set varcompare (where=(anychange = 1));
      file "&rfdpath.\&surv.\Changes_between_versions_&pv._and_&vv..txt" mod;
           
      put / "---------------------------------------------------------------------------";
      put   "    List of Variable Attribute Changes for SAS Variable Name " sas_name;
      put   "---------------------------------------------------------------------------" /;
  
      %printcomp(val=code_nam,vdesc=Code Name);
      %printcomp(val=basetext,vdesc=Base/Text Flag);
      %printcomp(val=col_name,vdesc=Column Name);
      %printcomp(val=question,vdesc=Question Text);
      %printcomp(val=sortid,vdesc=Sort Order);
      %printcomp(val=source,vdesc=Source/Question Number);
    /*  %printcomp(val=sub2edit,vdesc=Subject to Edit Flag);
      %printcomp(val=sub2imp,vdesc=Subject to Imputation Flag);
      %printcomp(val=createdby,vdesc=Created-By Flag);*/
      %printcomp(val=type,vdesc=Variable Type);
    run;

    data _null_;
      set codecompare (where=(anychange = 1));
      by sas_name;

      file "&rfdpath.\&surv.\Changes_between_versions_&pv._and_&vv..txt" mod;

      put / "---------------------------------------------------------------------------";
      put   "Acceptable Value Changes for SAS Variable " sas_name ;
      put   "---------------------------------------------------------------------------" /;
      put   "Code Name:             " code_nam;
      put   "Acceptable Value:      " codeval;
      put   "Previous Description:  " descr_old;
      put   "New Description:       " descr_new /;
  
    run;


    title "Frequency of Variable Names Matching Between Version &version and Version &pversion";
    proc freq data=varcompare;
      table mergeflag / out=vcomp;
    run;
    title;

    data _null_;
      set vcomp end=last;
      file "&rfdpath.\&surv.\Changes_between_versions_&pv._and_&vv..txt" mod;

      if _n_ = 1 then put // "***************************************************************************";  

      put mergeflag;
      put @5 "Number:  " count comma10.;
      put @5 "Percent: " percent /;

      if last then put "***************************************************************************" //;

    run;

    data added (keep=sortby added);
      set varcompare (where=(index(upcase(mergeflag),'NEW')>0));
      rename sas_name = added;
      sortby = _n_;
    run;

    data dropped (keep=sortby dropped);
      set varcompare (where=(index(upcase(mergeflag),'DROPPED')>0));
      rename sas_name = dropped;
      sortby = _n_;
    run;

    data add_drop;
      merge added dropped;
      by sortby;
    run;

    data _null_;
      set add_drop end=last;

      file "&rfdpath.\&surv.\Changes_between_versions_&pv._and_&vv..txt" mod;

      if _n_ = 1 then do;
        put // "***************************************************************************";  
        put    "   Summary of Variables Added or Dropped Between Versions &pversion and &version";
        put    "***************************************************************************" /;  
        put /  @10 "Variables Added"  @35 "Variables Dropped";
        put    @10 "---------------"  @35 "-----------------" /;
      end;

      put @10 added @35 dropped;

      if last then put "***************************************************************************" //;
     
    run;

    title "Variables Dropped Between Version &pversion and Version &version";
    proc print data=varcompare (where=(index(upcase(mergeflag),'DROPPED')>0)) noobs;
      var sas_name;
    run;
    title;

    title "Variables Added Between Version &pversion and Version &version";
    proc print data=varcompare (where=(index(upcase(mergeflag),'NEW')>0)) noobs;
      var sas_name;
    run;
    title;


    title "Frequency of Code Names and Values Matching Between Version &version and Version &pversion";
    proc freq data=codecompare;
      table mergeflag / out=ccomp;
    run;
    title;

    data _null_;
      set ccomp end=last;
      file "&rfdpath.\&surv.\Changes_between_versions_&pv._and_&vv..txt" mod;
        
      if _n_ = 1 then put // "***************************************************************************";  

      put mergeflag;
      put @5 "Number:  " count comma10.;
      put @5 "Percent: " percent /;
  
      if last then put "***************************************************************************" //;

    run;

    proc sort data=codecompare;
      by sas_name mergeflag;
    run;

    data _null_;
      set codecompare (where=(index(mergeflag,'New')>0 or index(mergeflag,'Dropped')>0));
      by sas_name mergeflag;

      file "&rfdpath.\&surv.\Changes_between_versions_&pv._and_&vv..txt" mod;
  
      if first.sas_name then do;
         
        put / "---------------------------------------------------------------------------";
        put   "    List of Acceptable Value Changes for SAS Variable Name " sas_name;
        put   "---------------------------------------------------------------------------";
  
      end;

      if first.mergeflag and index(mergeflag,'Dropped')>0 then put / "Values Dropped:" /;
      if first.mergeflag and index(mergeflag,'New')>0 then put / "Values Added:" /;

      if index(mergeflag,'Dropped')>0 then put @5 codeval @25 descr_old;
      if index(mergeflag,'New')>0 then put @5 codeval @25 descr_new;
  
    run;

    data rfd.varcomp_&surv._v&pv._to_v&vv.;
      set varcompare;
    run;
    data rfd.codecomp_&surv._v&pv._to_v&vv.;
      set codecompare;
    run;


  %end;

%mend addcodes;
%addcodes(surv=sdr);
%addcodes(surv=nscg);
%addcodes(surv=nsrcg);
%addcodes(surv=nscgn);

******************************************************************************;
*                                Done!!!                                     *;
******************************************************************************;
