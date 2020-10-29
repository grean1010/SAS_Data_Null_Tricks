*****************************************************************************;
* PROGRAM:  LOGIC_CHECKS.SAS
* TASK:     SESTAT - NSF, Contractor Specifications (6072-211)
* PURPOSE:  Check for consistency/logical skip patterns
*           for 2006 SESTAT contractor specifications.
*
* WRITTEN:  By Maria Cupples Hudson for 2006 contractor specifications
*
* MODIFIED:          
*           
* INPUTS:   1) xxxxxyy.XLS - Spreadsheet containing test conditions
*              Where xxxxx = Survey                          
*                       yy = Year
*           2) sestat2.mdb - new sestat database
*              a) dictionary table
*
* OUTPUTS:  1) xxxxxyy.SAS                    - Main program for checking consistency/
*                                               logical skip patterns
*           2) xxxxxyy.INC                    - Code to check for consistency/logical 
*                                               skip patterns
*           3) xxxxxyy1.SAS7BDAT              - logic check conditions SAS Dataset
*           4) xxxxxyy2.SAS7BDAT              - List of SAS names (from ACCESS DB)
*           5) xxxxxyy_logicchecks_readme.txt - a text file with a list of logic checks 
*                                               and very brief explanation
*              Where xxxxx = Survey 
*                       yy = Year
*
* NOTES:    1) This program is an enhanced version of the now obsolete GENSPSECS.SAS
*              which was itself an enhanced version of the then obsolete LOGISKIP.SAS program.
*
*****************************************************************************;


* include the program that sets all parameters and libnames for contractor specs ;
%include "J:\06850\Common\Task411_2010_Specs\set_parameters.sas";

*****************************************************************************;
**  Read in logic_checs.xls.  This spreadsheet helps identify the skips    **;
**  patterns of the survey and logical consistencies between variables for **;
**  all surveys.  We use the information in this spreadsheet to create SAS **;
**  code to check the incoming data from survey contractors.               **;
*****************************************************************************;

* Load EXCEL spreadsheet;
x "&lcheckpath.\logic_checks.xls";          

data _null_;
  x = sleep(5);
run;

* the tab containing checks common to all surveys;
filename checks dde "excel|checks&year.!r2c1:r9999c9" notab;    

* read in skip and logical consistency checks for all surveys;
data checks;
  infile checks dlm='09'x notab dsd lrecl=5000 pad missover ;
  length checknum active_sdr active_nscg active_nsrcg active_nscgn 8.
         sasname $10. testtype $25. ifcond thencond $200.;
  input checknum active_sdr active_nscg active_nsrcg active_nscgn
        sasname testtype ifcond thencond ;

  if sasname = " " then delete;

run;

* test that all check numbers are unique;
proc freq data=checks;
  table checknum / noprint out=checkprobs;
run;

* keep only check numbers that were repeated;
proc sort data=checkprobs (where=(count > 1)); by checknum;  run;
proc sort data=checks;  by checknum;  run;

* merge duplicated check numbers onto the allchecks dataset to isolate the problems;
data allprobs;
  merge checkprobs (in=a) checks (in=b);
  by checknum;
  if a;
run;

* count everything up and print a summary to the lst file;
%numobs(checks,allchecks);
%numobs(checkprobs,checkprobs);
%numobs(allprobs,allprobs);

data _null_;
  file print;
  put "Summary of checks run on the spreadsheet logic_checks.xls";
  put "---------------------------------------------------------"/;
  put "Number of checks read from spreadsheet:"       @50 "&allchecks";
  put "Number of checks with duplicate check number:" @50 "&checkprobs";
  put "Number of checks that must be fixed:"          @50 "&allprobs";
  put "All problem observations (if any) will be printed below.";
run;

title "PROBLEM!!  These check numbers are used more than once!!  Fix and rerun!!";
proc print data=checkprobs;
run;
title;

* close the spreadsheet;
filename cmds dde "excel|system";
data _null_;
  file cmds;
  put '[quit]';
run;

data _null_;
  x = sleep(5);
run;

%macro process(surv=,dsnb=,dsnt=);

* set the survey name to be surv plus 2-digit year extenstion;
%let survey = &surv.&yr.;

* create a survey-specific global macro variable to hold the number of checks;
* for that survey                                                            ;
%global &survey._probs;

*****************************************************************************;
**              Create list of SAS names from ACCESS database              **;
*****************************************************************************;
proc sort data = metadata.&survey. (keep=sas_name source createdby
                                    where=(createdby in ('Questionnaire','Survey Contractor')))
          out  = lcheck.&survey._expected_v&vv (keep=sas_name) nodupkey;
  by sas_name;
run;



data &survey.;
  set checks (where=(active_&surv = 1));

  ***************************************************************************;
  **   After loading in Excel spreadsheet with skip pattern information,   **;
  **       build SAS include code which will test the skip patterns.       **;
  ***************************************************************************;
  length ifcondx thencondx $200. prob $10. check_number $30.
         check_description $400. testfreq $20.;

  * remove any trailing spaces that may have been in the spreadsheet.        ;
  ifcond = trim(ifcond);
  thencond = trim(thencond);
  sasname = trim(sasname);
  
  * make testtype all lowercase so we do not miss things due to typos.       ;
  testtype = trim(lowcase(testtype));


  * add parens and trim extra characters from the if condition               ;
  if ifcond ne '' then ifcondx="("||trim(ifcond)||")";     

  * add parens and trim extra characters from the then condition             ;
  if thencond ne '' then thencondx="("||trim(thencond)||")"; 

  * create variable prob so that we can create indicator variables probXXX   ;
  * for each logic check we create from the input spreadsheet.               ;
  prob = "prob"||put(checknum,z5.);
  prob = compress(prob);

  * store the number of logic checks for each survey in a macro variable.    ;
  call symput("&survey._probs",compress(_n_));     
  
  check_number = "Logic check variable "||compress(prob);
  check_number = trim(check_number);

  testfreq = "testfreqs"||compress(put(checknum,z5.));
  testfreq = compress(testfreq);

  * Build the first include file.  This holds the actual logic-checking steps.;
  file "&lcheckpath\&survey..inc"; 

  if _n_ = 1 then do;
    put "/* This is the first include file used in the program &survey..sas."  @80 "*/";
    put "/* It checks skip patterns and looks for logical consistency between" @80 "*/";
    put "/* variables submitted by the survey contractors."                    @80 "*/"//;
  end;

  if testtype = "if then" then do;
    check_description = "Check that if the first condition "||trim(ifcondx)||
                        " is true then the second condition "||trim(thencondx)||
                        " cannot also be true.";
    put "/* " check_number ":" / check_description " */" /;
    put "if " ifcondx " and " thencondx " then " prob  " = 1 ;" /
        "else " prob " = 0 ;" ///;
  end;   

  else if testtype="always" then do;
    check_description = "Check that "||trim(sasname)||" always meets the condition "
                        ||trim(ifcondx)||".";
    put "/* " check_number ":" / check_description " */" /;
    put "if " ifcondx " then " prob " = 0 ;" /
        "else "  prob " = 1 ;" ///;
  end;    

  * Build the second include file that holds table statements for the proc freq;
  file "&lcheckpath\&survey..inc2";
  if _n_ = 1 then do;
    put "/* This is the second include file used in the program &survey..sas."  @80 " */";
    put "/* It runs the frequency tables for each problem variable and creates" @80 "*/";
    put "/* a dataset to be used later in the program to tabulate the results." @80 "*/"//;
  end;
  put "  tables " prob " / missing noprint out = " testfreq ";";
run;

* The third and fourth include files need to use the end=last option so we    ;
* start a new data step to create these files (Reading in from the spreadsheet;
* brings in extra blank records so the end=last option did not work there.)   ;
data _null_;
  set &survey end=last;

  * Build the third include file.  This has the set statement to bring in the ;
  * datasets created in the second include program.                           ;
  file "&lcheckpath\&survey..inc3"; 

  if _n_ = 1 then do;
    put "/* This is the third include file used in the program &survey..sas.  " @80 "*/";
    put "/* It is a set statement that brings in all of the datasets created  " @80 "*/";
    put "/* in the proc freq.                                                 " @80 "*/"//;
    put "set ";
  end;
  put @5 testfreq;
  if last then put ";";


  * Build the fourth include file that an array statement for all prob variables;
  file "&lcheckpath\&survey..inc4";
  if _n_ = 1 then do;
    put "/* This is the fourth include file used in the program &survey..sas. " @80 "*/";
    put "/* It is an array statement that creates an array of all the problem " @80 "*/";
    put "/* variables created in the previous proc freq.                      " @80 "*/"//;
    put "array probs(*) ";
  end;
  put @17 prob;
  if last then put @17 ";";

run;

*****************************************************************************;
**               Create dataset of all logic checking tests                **;
**              Output the same to an explanation text file.               **;
*****************************************************************************;
data lcheck.&survey._lchecks_v&vv;
  set &survey. (keep = prob check_number check_description testfreq);
  file "&lcheckpath.\&survey._logicchecks_readme.txt";

  if _n_ = 1 then do;
    put "This text file lists the logic checks for &survey.";
    put "As output to the include file:  &survey..inc" /;
    put "This include file will be read by the program &survey..sas."/;
    put "Other include files to be used:";
    put "  &survey..inc2" @17 "holds table statements to be used";
    put @17 "in a proc freq.";
    put "  &survey..inc3" @17 "holds a set statement to read all";
    put @17 "datasets created in &survey..inc2.";
    put "  &survey..inc4" @17 "holds an array statement holding all problem";
    put @17 "variables in the previous 2 include files.";    
    put // "The include files and this text file were generated by the program logic_checks.sas" //;
  end;

  put "*************************************";
  put "** " check_number " **" ;
  put "*************************************";
  put check_description /;
  put "Dataset Created: " testfreq //;
run;


*****************************************************************************;
**  Create a SAS program for each survey that will perform the consistency **;
**  checks according to the spreadsheets and print out results.            **;
*****************************************************************************;
data _null_;
  file "&lcheckpath.\&survey..sas";
  set &survey (keep=checknum);

  if _n_ = 1 then do;
    put "**********************************************************************";
    put  "*                                                                     ";
    put  "* PROGRAM:  &SURVEY..SAS (Generated by LOGIC_CHECKS.SAS)              ";
    put  "* TASK:     SESTAT - NSF, Contractor Specifications (6072-211)        ";
    put  "* PURPOSE:  Check for consistency/logical skip patterns               ";
    put  "*           for 2006 SESTAT contractor specifications.                ";
    put  "*                                                                     ";
    put  "* WRITTEN:  Automatically generated by logic_checks.sas               ";
    put  "*                                                                     ";
    put  "* INPUTS:   1) &DSNB..SD2 - Base file to be validated                 ";
    put  "*           2) &DSNT..SD2 - Text file to be validated                 ";
    put  "*           3) &survey._lchecks_v&vv.SAS7BDAT -                       ";
    put  "*                 Logic check conditions SAS Dataset                  ";
    put  "*           4) &survey._expected_v&vv.SAS7BDAT -                      ";
    put  "*                 List of SAS names expected by MPR                   ";
    put  "*                                                                     ";
    put  "* OUTPUT:   1) Consistency/logical skip validation summary listing    ";
    put  "*                                                                     ";
    put  "* INCLUDES: 1) &SURVEY..INC  - Code to check for logical skip patterns";
    put  "*           2) &SURVEY..INC2 - table statements for proc freq         ";
    put  "*           2) &SURVEY..INC3 - set statements data step               ";
    put  "*           2) &SURVEY..INC4 - array statement                        ";
    put  "*                                                                     ";
    put  "**********************************************************************;";
    put  "options nofmterr nocenter ls=132 ps=79 compress=yes source2            ";
    put  "        formchar='|----|+|---+=|-/\<>*';                               ";
    put ;
    put  "libname datain '.';";
    put ;
    put  "/* change dataset names here if necessary */                            ";
    put  '%' "let dsnb = &dsnb " ';                                               ';
    put  '%' "let dsnt = &dsnt " ';                                               ';
    put ;
    put "/* macro to count up number of obs in a dataset                          ";
    put "   and output to macro variable                                        */";
    put '%' "macro numobs(dsn,nbrobs);                                            ";
    put '  %' 'global &nbrobs;                                                    ';
    put '  %' 'let datasetid=%sysfunc(open(&dsn));                                ';
    put '  %' 'let &nbrobs=%sysfunc(attrn(&datasetid,nlobs));                     ';
    put '  %' 'let closing_code=%sysfunc(close(&datasetid));                      ';
    put '%' "mend;                                                                ";
    put ;
    put  "/* Merge the base and text files by refid so we can run the logical     ";
    put  "   skip tests.                                                        */";
    put ;
    put 'proc sort data=datain.&dsnb out=&dsnb;  by refid;  run;                  ';
    put 'proc sort data=datain.&dsnt out=&dsnt;  by refid;  run;                  ';
    put ;
    put 'data totest;                                                             ';
    put '  merge &dsnb &dsnt;                                                     ';
    put '  by refid;                                                              ';
    put 'run;                                                                     ';
    put ;
    put  "/* Run proc contents on the input dataset to get a list of sas_names    ";
    put  "   in the input dataset.  We will compare these to the sas_names in     ";
    put  "   the sestat database.                                               */";
    put  "proc contents data=totest noprint";
    put  "              out=sasnames(keep=name rename=(name=sas_name));";
    put  "run;";
    put ;
    put  "proc sort data=sasnames; ";
    put  "  by sas_name; ";
    put  "run;";
    put ;
    put  "proc sort data=datain.&survey._expected_v&vv ";
    put  "           out=&survey._expected;";
    put  "  by sas_name; ";
    put  "run;";
    put ;
    put  "data merged;";
    put  "  merge sasnames(in=in1) &survey._expected(in=in2);";
    put  "  by sas_name;";
    put  "  length flag $50.;";
    put  "  if in1 and in2 then flag = 'Matched';";
    put  "  else if in1 = 1 and in2 = 0 then flag = 'In Contractor Dataset but not MPR Specs';";
    put  "  else if in1 = 0 and in2 = 1 then flag = 'NOT in Contractor Dataset but in MPR Specs';";
    put  "run;";
    put ;
    put  "title '&survey..sas - Frequency of merge-check flag for variable names';";
    put  "proc freq data=merged;";
    put  "  tables flag / missing list;";
    put  "run;";
    put  "title;";
    put ;
    put "proc sort data=merged (where=(flag ne 'Matched')) out=mergeprobs;";
    put "  by flag;";
    put "run;";
    put ;
    put  "title '&survey..sas - Print of variable names that did not match';";
    put  "proc print data=mergeprobs noobs; ";
    put  "  by flag;                    ";
    put  "  var sas_name;               ";
    put  "run;                          ";
    put  "title;";
    put ;
    put  "data basedata;";
    put  '  set totest;';
    put  "  %include '&survey..inc';";
    put  "run;";
    put ;
    put "proc freq data=basedata;";
    put  "  %include '&survey..inc2';";
    put  "run;";
    put ;
    put "* Put all check frequencies together.  Delete records that are not a problem (i.e. problem flag = 0);";
    put "* create dataset ALLPROBS with all logic check problems (i.e. problem flag = 1) ;";
    put "data allprobs noprobs;";
    put "  %include '&survey..inc3';";
    put ;
    put "  length prob $15.;";
    put "  %include '&survey..inc4';";
    put ;
    put "  do i = 1 to dim(probs);";
    put "    prob = vname(probs(i));";
    put "    if probs(i) = 1 then output allprobs;";
    put "    if probs(i) = 0 and percent = 100 then output noprobs;";
    put "  end;";
    put ;
    put "  keep prob count percent;";
    put "run;";
    put ;
    put "* sort and merge on the dataset of problem names for use in the output;";
    put "proc sort data=allprobs;  by prob;  run; ";
    put "proc sort data=noprobs;   by prob;  run; ";
    put "proc sort data=datain.&survey._lchecks_v&vv ";
    put "            out=&survey._lchecks_v&vv; ";
    put "  by prob; ";
    put "run; ";
    put ;
    put "data allprobs;";
    put "  merge allprobs (in=a) &survey._lchecks_v&vv ;";
    put "  by prob;";
    put "  if a; ";
    put "run;";
    put ;
    put "data noprobs;";
    put "  merge noprobs (in=a) datain.&survey._lchecks_v&vv ;";
    put "  by prob;";
    put "  if a; ";
    put "run;";
    put ;
    put "options nocenter;";
    put ;
    put "* add a start line to the printout so we know where the problem printouts start;";
    put "data _null_;";
    put "  file print;";
    put "  put 'Begin printing problems here.';";
    put "run;";
    put ;
    put "Title 'This printout gives the number of observations that FAILED each test';";
    put "data _null_;";
    put "  set allprobs (where=(count ne 0));";
    put "  file print;";
    put ;
    put "  put '******************************************************************************************************************************************';";
    put "  put 'TEST Name:  ' prob;";
    put "  put 'TEST Descriptions:  ' check_description;";
    put "  put 'Number of Observations Failing:  ' count;";
    put "  put 'Percentage of Observations Failing:  ' percent;";
    put "  put '******************************************************************************************************************************************';";
    put "  put //;";
    put ;
    put "run;";
    put "title;";
    put ;
    put "* add one last line to the printout so we know the program ran;";
    put "data _null_;";
    put "  file print;";
    put "  put 'Done printing problems.';";
    put "run;";
    put ;
    put "* Count up the number of obs in the allprobs and the noprobs datasets;";
    put "* We will use these to create a summary of results.                  ;";
    put '%' 'numobs(allprobs,allprobs);';
    put '%' 'numobs(noprobs,noprobs);';
    put '%' 'numobs(mergeprobs,mergeprobs);';
    put ;
    put "* print a summary of results;";
    put "data _null_;";
    put "  file print;";
    put ;
    put '  allprobs = &allprobs ;';
    put '  noprobs = &noprobs ;';
    put '  mergeprobs = &mergeprobs ;';
    put ;
    put "  put '-------------------';";
    put "  put 'Summary of results:';";
    put "  put '-------------------';";
    put "  put 'Number variable names that did not match what was expected:' @65 mergeprobs;";
    put "  put 'Number of tests where all observations passed:'              @65 noprobs;";
    put "  put 'Number of tests where at least one observation failed:'      @65 allprobs;";
    put "run;";
    put ;
  end;
run;

* Look at any previous version we may have had.  We compare each logic check;
* to the previous version and list all changes.  The data _null_ prints all ;
* changes, additions and deletions to the lst file.  These can be used to   ;
* tell survey contractors exactly what changed.                             ;

%if &pversion ne 0.0 %then %do;


  * the path name for the logic check programs and output that were previously delivered;
  libname pversion "&path.\Delivered\Version&pversion\&surv\logic_checks";

  proc sort data=pversion.&survey._expected_v&pv out=pexp;
    by sas_name;
  run;

  proc sort data=lcheck.&survey._expected_v&vv out=cexp;
    by sas_name;
  run;

  data compare_expected;
    merge pexp(in=prev) cexp(in=curr);
    by sas_name;
    length status $100.;
    if prev and curr then status = "No Change";
    else if prev then status = "In Version &pversion that are NO LONGER EXPECTED in Version &version";
    else if curr then status = "NOW EXPECTED in Version &version that were not expected in Version &pversion";
  run;

  proc sort data=compare_expected;  by status;  run;

  title "Status of all &surv SAS_NAME values in the current version (V&version) versus the previous (V&pversion)";
  proc freq data=compare_expected;
    table status / missing nocum out=freq_exp;
  run;

  data _null_;
    set freq_exp;
    file "&lcheckpath.\&survey._logicchecks_readme.txt" mod;

    if _n_ = 1 then do;
      put //;
      put @1 "******************************************************************************************************";
      put @1 "* Status of all &surv SAS_NAME values in the current version (V&version) versus the previous (V&pversion)" 
          @102 "*" ;
      put @1 "******************************************************************************************************";
    end;
    put "STATUS:  "  status;
    put "Number of Variables:  " count /;
  run;

  title;


  data _null_;
    set compare_expected (where=(status ne "No Change"));
    by status;
    if _n_ = 1 then do;
      put //;
      put @1 "******************************************************************************************************";
      put @1 "* All &surv SAS_NAME values for which there is some change between this version (V&version) versus the previous (V&pversion)"
          @102 "*";
      put @1 "******************************************************************************************************";
    end;
    file "&lcheckpath.\&survey._logicchecks_readme.txt" mod;
    if first.status then put @1 status;
    put @5 sas_name;
    if last.status then put /;
  run;

  proc sort data=pversion.&survey._lchecks_v&pv
             out=pchecks (keep=prob check_description
                          rename = (check_description = previous_check_description));
    by prob;
  run;

  proc sort data=lcheck.&survey._lchecks_v&vv
             out=cchecks;
    by prob;
  run;

  data compare_checks;
    merge pchecks(in=prev) cchecks(in=curr);
    by prob;
    length status $100.;
    if prev and curr then do;
      if check_description = previous_check_description then status = "No Change";
      else status = "Change in the Logic Check";
    end;
    else if prev then status = "Logic Checks in Version &pversion That Are NO LONGER PERFORMED in Version &version";
    else if curr then status = "Logic Checks That ARE NOW PERFORMED in Version &version That Were Not performed in Version &pversion";
  run;

  title "Status of All &surv Logic Checks in the Current Version (V&version) Versus the Previous (V&pversion)";
  proc freq data=compare_checks;
    table status / missing nocum out=freq_checks;
  run;

  data _null_;
    set freq_checks;
    file "&lcheckpath.\&survey._logicchecks_readme.txt" mod;
    if _n_ = 1 then do;
      put //;
      put @1 "******************************************************************************************************";
      put @1 "* Status of All &surv Logic Checks in the Current Version (V&version) Versus the Previous (V&pversion)" 
          @102 "*" ;
      put @1 "******************************************************************************************************";
    end;
    put "STATUS:  "  status;
    put "Number of Logic Checks:  " count /;
  run;

  title;

  proc sort data=compare_checks;  by status prob;  run;

  title "All &surv Locic Checks for Which There is Some Change";
  data _null_;
    set compare_checks (where=(status ne "No Change"));
    by status;
    file "&lcheckpath.\&survey._logicchecks_readme.txt" mod;

    if first.status then do;
      put @1 "******************************************************************************************************";
      put @1 "* " status  @102 "*" ;
      put @1 "******************************************************************************************************";
    end;
    
    if substr(status,1,5) in ('Chang') then do;
      put @3 "----------------------------------------------------------------------" ;  
      put @3 "Check:  " prob ;
      put @3 "----------------------------------------------------------------------" ;  
      put @3 "Check Description in Version &pversion:";
      put @3 previous_check_description / ;
      put @3 "Check Description in Version &version:";
      put @3 check_description / ;  
      put @3 "----------------------------------------------------------------------" /;  
    end;

    else if index(status,'ARE NOW PERFORMED')>0 then do;
      put @3 "----------------------------------------------------------------------" ;  
      put @3 "NEW (or reinstated):  " prob ;
      put @3 "Check Description:  ";
      put @3 check_description  ;  
      put @3 "----------------------------------------------------------------------" /;  
    end;

    else if index(status,'NO LONGER')>0 then do;
      put @3 "----------------------------------------------------------------------" ;  
      put @3 "Check NO LONGER performed:  " prob ;
      put @3 "Check Description:  ";
      put @3 previous_check_description ;
      put @3 "----------------------------------------------------------------------" /;  
    end;

    if last.status then put ///;

  run;
  title;


  * Run the program we just created to be sure that it works properly;
  x "sas &lcheckpath.\&survey..sas";

%end;

%mend process;

%process(surv=NSRCG,dsnb=FBRCG&filexnsrcg.,dsnt=FTRCG&filexnsrcg.);
%process(surv=SDR,  dsnb=FBSDR&filexsdr.,  dsnt=FTSDR&filexsdr.);
%process(surv=NSCG, dsnb=FBSCG&filexnscg., dsnt=FTSCG&filexnscg.);
%process(surv=NSCGN,dsnb=FBCGN&filexnscgn.,dsnt=FTCGN&filexnscgn.);


