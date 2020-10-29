*****************************************************************************;
* PROGRAM:  rfd_create.sas                                                  *;
* TASK:     SESTAT - NSF, Contractor Specifications (8797-110)              *;
* PURPOSE:  Bring in all metadata information and create the datasets we    *;
*           need to generate the RFD files for contractor specificaitons.   *;
*                                                                           *;
* WRITTEN:  By Maria Cupples Hudson after 2006 contractor specifications    *;
*           Stealing liberally from Dave Edson                              *;
*                                                                           *;
* MODifIED:                                                                 *;
*                                                                           *;
* INPUTS:  1) allwithcodes_&surv datasets for each survey.                  *;
*                                                                           *;
* OUTPUTS:                                                                  *;
*                                                                           *;
* NOTES:   This program will create the Record Format Description (RFD)     *;
*          files.  There will be one Post Script file created per survey.   *;
*          Programs replaced:  makerfd.sas, mkindex1.sas, mkindex2.sas      *;
*                                                                           *;
*****************************************************************************;
* include the program that sets all parameters and libnames for specs ;
%include "J:\06850\Common\Task411_2010_Specs\set_parameters.sas";

******************************************************************************;
*                   Change these macros and libnames                         *;
******************************************************************************;

* List all sas names that have changed since the previous version of specs    ;
* here.  If none, leave it equal to ''.  Otherwise have a comma-delimted list ;
* of sas_names with quotes around them.  NOTE, these are in addition to the   ;
* changes automatically flagged in rfd_prep.sas.                              ;
%let sdr_changed = '';
%let nscg_changed = '';
%let nsrcg_changed = '';
%let nscgn_changed = '';

* Variables to help manage page spacing.  Generally these will not change.    ;
%let vpagesize = 730;                     * vertical page size for RFD        ;
%let ipagesize = 720;                     * vertical page size for index      ;
%let footerstart = 45;                    * footer start for RFD              ;
%let ifooterstart = 70;                   * footer start for index            ;
%let num_on_index_pg = 61;                * number of variables per index page;

******************************************************************************;
*                    Done changing macros and libnames                       *;
******************************************************************************;

******************************************************************************;
*   Step 1:  Create macros that we will need while creating the PS files.    *;
*            These will all be called by the macro MAIN as part of a data    *;
*            step that is writing to the PS file.                            *;
******************************************************************************;

* Macro for Horizonal Line;
%macro hrline; 
  put @1 'newpath';
  put @1 ' 72 ' vpos 'moveto';
  put @1 '575 ' vpos 'lineto';
  put @1 'closepath';
  put @1 'stroke';
%mend hrline;

* Macro for First Lines of a New Page;
%macro titlep;

  put @1 '/Helvetica-Bold findfont 11 scalefont setfont';

  * page title line;
  put @1 '72   0 add ' vpos 'moveto '
   "(&year &surv &ftype File - SESTAT Record Format Description &indextitle.- Version &version.) show";
  vpos = vpos - vincrmnt;
  put @1 '/Helvetica findfont 9 scalefont setfont';

  * Add a Dark Horizontal Line;
  %hrline;

  * put in the column headings, each time subtracting from vpos;
  * so we can keep track of where we are vertically in the page;
  vpos = vpos - vincrmnt;
  put @1 '72   0 add ' vpos 'moveto (SAS) show';
  put @1 '72  70 add ' vpos 'moveto (Source) show';
  
  vpos = vpos - vincrmnt;
  put @1 '72   0 add ' vpos 'moveto (Name) show';
  put @1 '72  65 add ' vpos 'moveto (Question) show';
  put @1 '72 130 add ' vpos 'moveto (Description) show ';

  * for variable pages we put the type in the last column;
  %if "&indextitle" = "" %then %do;
    put @1 '72 483 add ' vpos 'moveto (Type) show ';
  %end;

  * for index pages we put the page number in the last column;
  %if "&indextitle" ne "" %then %do;
    put @1 '72 483 add ' vpos 'moveto (Page) show ';
  %end;

  vpos = vpos - (vincrmnt/2);
 
 * Add another Dark Horizontal Line;
  %hrline;

  vpos = vpos - vincrmnt;

%mend titlep;

* Macro for Footer;
%macro footerp; 

  * the footer line always starts at position 45 in the page;
  vpos = 45;

  * start with a dark horizontal line;
  %hrline;

  * add the footer text with the date stamp;
  tmpdate = put(&dstamp.,worddate20.);  /* left justify */
  put @1 " 72 35 moveto (&year &surv &ftype File: " tmpdate ") show ";
 
  * if any sas_name on the page has changed, we need to add     ;
  * the footnote explaining why there is an asterisk in the page;
  if chgflag = 1 then do;
    put @1 " 300 35 moveto (* -- Denotes change from Version &pversion. ) show "; 
  end;
 
  * Finally add the page number on the far right.  For the variable descriptions;
  * we use the variable PAGENO.  For the indices we use IPAGENO.                ;

  %if "&indextitle" = "" %then %do;
     put @1 '525 35 moveto (Page:' pageno  4. ') show';  * print page number  ;
     put @1 'showpage';                                  * eject (print) page ;
  %end;
  %if "&indextitle" ne "" %then %do;
     put @1 '525 35 moveto (Page:' ipageno  4. ') show';  * print page number ;
     put @1 'showpage';                                   * eject (print) page;
  %end;
 
  * reset chgflag variable ;
  chgflag = 0;
 
  * increment the appropriate page number;
  %if "&indextitle" = "" %then %do;
    pageno = pageno + 1;
  %end;
  %if "&indextitle" ne "" %then %do;
    ipageno = ipageno + 1;
  %end;
 
  * If this is not the last record, then reset the vertical;
  * position to the top of the page AND printout out the   ;
  * title lines for the next page.                         ;
  if not eof then do; 
    vpos = &vpagesize.;
    put @1 '/Helvetica-Bold findfont 11 scalefont setfont';
    put @1 '72   0 add ' vpos 'moveto '
       "(&year &surv &ftype FILE - SESTAT Record Format Description &indextitle.- Version &version. ) show";     
    vpos = vpos - vincrmnt;
    put @1 '/Helvetica findfont 9 scalefont setfont';

    %hrline

    vpos = vpos - vincrmnt;
    put @1 '72   0 add ' vpos 'moveto (SAS) show';
    put @1 '72  70 add ' vpos 'moveto (Source) show';
    vpos = vpos - vincrmnt;
    put @1 '72   0 add ' vpos 'moveto (Name) show';
    put @1 '72  65 add ' vpos 'moveto (Question) show';
    put @1 '72 130 add ' vpos 'moveto (Description) show ';

    * for variable pages we put the type in the last column;
    %if "&indextitle" = "" %then %do;
      put @1 '72 485 add ' vpos 'moveto (Type) show ';
    %end;

    * for index pages we put the page number in the last column;
    %if "&indextitle" ne "" %then %do;
      put @1 '72 485 add ' vpos 'moveto (Page) show ';
    %end;

    vpos = vpos - (vincrmnt/2);
    %hrline
    vpos = vpos - vincrmnt;

  end; 

%mend footerp;

* Macro for Group Header;
%macro grphead;

  * This macro creates the lines that start off each sas name;

  * keep track of where we are vertically;
  vpos = vpos - vincrmnt;

  * first put the sas name and the variable source;
  put @1 '/Helvetica-Bold findfont 9 scalefont setfont';
  put @1 '72  0 add ' vpos 'moveto (' sas_name ') show';
  put @1 '/Helvetica findfont 9 scalefont setfont';
  put @1 '72  65 add ' vpos 'moveto (' source ') show';
      
  * then put the description and variable type or page number;
  put @1 '72 130 add ' vpos 'moveto (' descripti ') show ';

  * for variable pages we put the type in the last column;
  %if "&indextitle" = "" %then %do;
    put @1 '72 483 add ' vpos 'moveto (' type  ') show ';
  %end;

  * for index pages we put the page number in the last column;
  %if "&indextitle" ne "" %then %do;
    put @1 '72 483 add ' vpos 'moveto (' pageno  ') show ';
  %end;

  * if there is a second line to the description then put ;
  * it here and make sure to subtract from vpos           ;
  if desc2 ne '' then do;
    vpos = vpos - vincrmnt;
    put @1 '72 130 add ' vpos 'moveto (' desc2 ') show ';
  end;

  * Again keep track of where you are in the page.        ;
  * Also put an extra blank line in after the group header;
  * NOTE:  This is only for the variable descriptions.  We;
  *        do not skip lines in the indices.              ;
  %if "&indextitle" = "" %then %do;
    vpos = vpos - vincrmnt - vincrmnt; 
  %end;

%mend grphead;
 
%let surv=sdr;
%let ftype = base;

******************************************************************************;
*   The following macro will comprise steps 2 - 5 of the program.  These     *;
*   steps will be described within the macro.                                *;
******************************************************************************;

%macro main(surv,ftype);

  ****************************************************************************;
  * macro parameters                                                         *;
  * -----------------                                                        *;
  * year    = Year (for labels)                                              *;
  * ftype   = file type we are creating (base or text)                       *;
  ****************************************************************************;

  * When creating the RFD files the indextitle will be blank.  We only use    ;
  * this when creating the indices later in the program.                      ;
  %let indextitle=;

  * create a macro variable to hold the name of the survey in upper case      ;
  * surrounded by single quotes.  We will use this in many of the title       ;
  * statements we create later on.                                            ;
  data _null_;
    length surv survey $7. vars ind1 ind2 $300.;

    survey = upcase("&surv.&yr");
    surv   = upcase("&surv.");  
    psfile = "&rfdpath\&surv\&ftype..ps";
    vars   = "&rfdpath.\&surv.\&ftype.rfd.txt";
    ind1   = "&rfdpath.\&surv.\&ftype.ind1.txt";
    ind2   = "&rfdpath.\&surv.\&ftype.ind2.txt";

    call symput('survey',left(compress(survey)));
    call symput('surv',left(compress(surv)));
    call symput('vars',left(compress(vars)));
    call symput('ind1',left(compress(ind1)));
    call symput('ind2',left(compress(ind2)));
    call symput('psfile',left(compress(psfile)));

  run;

  * Print these to the lst file so we have a record of them and to make sure  ;
  * they were assigned correctly.                                             ;
  data _null_;
    file print;

    put / "For &surv &ftype variable listings we are using the following";
    put "     Survey:              &survey";
    put "     RFD text file:       &vars";
    put "     Index 1 (Source):    &ind1";
    put "     Index 2 (SAS Name):  &ind2";
    put "     Final post-script file to be created:  &psfile."//;

  run;

  ****************************************************************************;
  * Step 2:  Bring in the dataset created by rfd_prep.sas and clean it up as *;
  *          necessary to create the RDF diles.                              *;
  ****************************************************************************;

  * bring in the actual data                                                  ;
  proc sort data = rfd.allwithcodes_&surv._v&vv out=&surv.data;
    by sas_name;
  run;

  * bring in any new or changed variable name                                 ;
  %if "&pversion" ne "0.0" %then %do;

    * If there is a previous version then bring in the dataset containing     ;
    * all variables that have some change between versions.                   ;
    proc sort data =  rfd.varcomp_&surv._v&pv._to_v&vv. (keep=sas_name anychange
                                                         where=(anychange in (1,2)))
               out = ch1&surv (keep = sas_name);
      by sas_name;
    run;

    * bring in any new or changed code name (keeping only the sas_name that it;
    * applies to)                                                             ;
    proc sort data = rfd.codecomp_&surv._v&pv._to_v&vv.(keep=sas_name anychange
                                                        where=(anychange in (1,2)))
               out = ch2&surv (keep = sas_name);
      by sas_name;
    run;

    * Combine the two datasets so we will have one dataset with all SAS_NAMEs ;
    * that had some change between this and the previous version.             ;
    data allchange&surv;
       merge ch1&surv ch2&surv;
       by sas_name;
    
       * Make sure sas_name is a 50-character variable.                       ;
       length new $50.;
       new = left(compress(sas_name));
       drop sas_name;
       rename new=sas_name;
    run;

    proc sort data=allchange&surv (keep=sas_name) nodupkey;
      by sas_name;
    run;

  %end;

  %else %do;

    * If there is no previous version then create a blank allchanges dataset. ;
    data allchange&surv;
      length sas_name $50.;
      sas_name = "";
      if sas_name ne "";
    run;

  %end;


  * Set up variables and remove anything that do not apply to this survey or  ;
  * to this document type.                                                    ;
  data temp&surv;
  
    length sas_name $50. tempf $7. first80 revfirst80 desc2 $80. holder $200.;    

    merge &surv.data allchange&surv(in=ch);
    by sas_name;

  
    * Add an asterisk to any sas name that has been changed                   ;
    if ch or sas_name in (&&&surv._changed) then sas_name = trim(sas_name)||"*";

    * If the description is too long, truncate it and add a second line with  ;
    * the remaining text.                                                     ;
    descripti = left(trim(descripti));

    if length(descripti) > 80 then do;

      first80 = substr(descripti,1,80);

      revfirst80 = left(reverse(trim(first80)));

      lspace = index(revfirst80,' ');

      holder = substr(left(trim(descripti)),1,80 - lspace);
      desc2 = substr(left(trim(descripti)),81 - lspace,length(descripti) - 80 + lspace);
    
      descripti = holder;

    end;


    * Keep only the relevent survey ids                                       ;
    if sas_name = "SURID" and codeval ne "&&&surv._surid" then delete;
    
    * Get rid of anything that does not apply or is a recode or carryover.    ;
    if source not in ("N/A","RECODE","SED","CARRYOVER"); 

    * Keep only those variables that apply for this file type.                ;
    tempf = upcase("&ftype");

    if tempf eq 'BASE' then do;
      if basetext eq 'A' or basetext eq 'B';
    end;
    else if tempf eq 'TEXT' then do;
      if basetext eq 'A' or basetext eq 'T';
    end;

    drop holder first80 revfirst80 lspace;
  run;

  * In order to prevent varaibles from being broken up onto spearate pages, we;
  * need to count up the number of observations per sas_name.                 ;
  proc freq data=temp&surv;
    table sas_name / noprint out=namecounts&surv (drop=percent);
  run;  

  proc sort data=temp&surv;  by sas_name;  run;
  proc sort data=namecounts&surv;  by sas_name;  run;

  * Merge these counts back onto the tempname dataset.                        ;
  data temp&surv;
    merge temp&surv namecounts&surv;
    by sas_name;
  run;

  * Now sort in the order we will be outputting the variables to the PS file. ;
  proc sort data=temp&surv; 
    by sortid sas_name printpos codeval;
  run;

  * Count up the number of sas names we will be outputting to the RFD files.  ;
  * We will then divide that by the number of observations by page.  This will;
  * let us know how many pages the indices will take up so that we know what  ;
  * page number to start the main sectio of the RFD file with.                ;
  %numobs(namecounts&surv,numvars&surv);

  data _null_;

    * the number of variables to be output;
    numvars = &&&numvars&surv;

    * number of pages each index takes;
    numindex = int(numvars / &num_on_index_pg) + 1;

    * we start the second index on the page after the above number;
    ind2start = numindex + 1;

    * we create two indices so we multiply this number by 2;
    varstart = 2 * numindex + 1; 

    * put all of these numbers into macro variables for later use;
    call symput("numindex&surv",left(compress(numindex)));
    call symput("ind2start&surv",left(compress(ind2start)));
    call symput("varstart&surv",left(compress(varstart)));

  run;

  * Print these to the lst file so we have a record of them and to make sure  ;
  * they were assigned correctly.                                             ;
  data _null_;
    file print;

    put / "For &surv &ftype variable listings we calculated the following";
    put "     Number of variables:                 &&&numvars&surv";
    put "     Number of pages per index:           &&&numindex&surv";
    put "     Index 2 starts on page :             &&&ind2start&surv";
    put "     Variable listing starts on page :    &&&varstart&surv" //;

  run;

  ****************************************************************************;
  * Step 3:  The following data step does two things.                        *;
  *          1) It creates the postscript file for the variable type we want *;
  *          2) It creates a dataset of page numbers for each variable to be *;
  *             used to create the index pages.                              *;
  ****************************************************************************;
  data indexout_&surv (keep = sas_name source sortid descripti desc2 pageno type);
    set temp&surv. end=eof;
    by sortid sas_name;   

    * assign postscript output file;
    file "&vars";

    * set initial values for parameters;
    retain vpos &vpagesize.;
    retain vincrmnt 10;
    retain pageno &&&varstart&surv;
 
    * the cutoff is where we stop printing to the current;
    * page, print the footer, and move to the next page  ;
    cutoff = &footerstart. + vincrmnt;

    * Indicator if version note should be printed, initialize to zero;
    retain chgflag 0;
   
    * At the start of the index, put a title page and set the font.;
    if _n_ eq 1 then do;
      %titlep;
      put @1 '/Courier findfont 9 scalefont setfont                       ';    
    end;

    * the very first time we see a sas_name;
    if first.sas_name then do;
  
      ************************************************************;
      * keep entire variable together --                         *;
      *  ==> last line of the page we should ever print on is 88 *;
      *  ==> group header info takes about 3*vincrement worth of *;
      *      space and each line takes vincrement.               *;
      *  ==> therefore we do not print if the group header plus  *;
      *      the obs for that sas_name would go over to the next *;
      *      page (variable vspace_needed will hold this value)  *;
      *  ==> ONE EXCEPTION, if the entire sas_name will take up  *;
      *      more than a page, then we will have to split it up. *;
      ************************************************************;
    
      * If the sas_name has a * in it, then we know if has changed;
      if index(sas_name,'*') then chgflag = 1;

      * Calculate the vertical space this variable will take up ;
      * for Base file.  Since we do not print values for text   ;
      * file, it will always be 3*vincrement                    ;
      if tempf eq 'BASE' then vspace_needed = 3 * vincrmnt + count * vincrmnt;
      else vspace_needed = 3 * vincrmnt;

      * Calculate the vertical space for just the group header  ;
      * and two observations.  We only use this for sas_names   ;
      * that will definitely need more than one page.           ;
      if tempf eq 'BASE' then vspace_needed2 = 5 * vincrmnt;
      else vspace_needed2 = 3 * vincrmnt;

      * If the description went on to a second line we need to  ;
      * add that to the amount of vertical space needed.        ;
      if desc2 ne '' then do;
        vspace_needed = vspace_needed + vincrmnt;
        vspace_needed2 = vspace_needed2 + vincrmnt;
      end;

      * if the vertical space needed for this variable is more  ;
      * than the page size, we only check to see if we have room;
      * for the header and two values.  If so, then start the   ;
      * variable on this page.  Otherwise move to the next page.;
      if vspace_needed > &vpagesize. and (vpos - vspace_needed2 lt cutoff) then do;
        %footerp;
      end;

      * if the current vertical position minus that needed would;
      * go past the cutoff then we stop printing to this page.  ;
      else if vspace_needed <= &vpagesize. and (vpos - vspace_needed lt cutoff) then do;
        %footerp;
      end;

      * Now it is okay to print out the header for this sas_name;
      %grphead;

    end;

    * print values only for base file;
    tempf = compress(upcase("&ftype"));
    if tempf eq 'BASE' then do;
      * print body line ;
      if vpos lt cutoff then do;
         %footerp;
         vpos = vpos - vincrmnt;
         put @1 '/Helvetica-Bold findfont 9 scalefont setfont';      
         put @1 '72 105 add ' vpos 'moveto (Acceptable Values (Continued):) show';
         put @1 '/Helvetica findfont 9 scalefont setfont';      
         vpos = vpos - vincrmnt;
      end;
      put @1 '72 105 add ' vpos 'moveto (' codeval ') show';    
      put @1 '72 160 add ' vpos 'moveto (' descr   ') show';
      vpos = vpos - vincrmnt;
  
    end;

    * Output page information at the first obs per sas name.  This way we will;
    * have the page the variable starts on for use in creating the index pages;
    if first.sas_name then output;
  
    * finish off the file;
    if eof then do;
      %footerp;
    end;

  run;                                                                        


  ****************************************************************************;
  * Step 4:  The following macro will create the index files.  There is one  *;
  *          index by sort order and another by sas name.  The sort-order    *;
  *          index will come first, then the sas-name index and then the RFD *;
  *          for each variable.                                              *;
  ****************************************************************************;

  %macro makeindex(which=);

    * Set macro variables as necessary depeding on which index we are creating;
    data _null_;

      length test sortorder indextitle indexout $300.;
      test = "&which";

      if upcase(test) = "SOURCE" then do;
        startpage = 1;
        sortorder = "sortid sas_name";
        indextitle = "Index By Source";
        indexout = "&ind1";
      end;
      else do;
        startpage = &&&ind2start&surv;
        sortorder = "sas_name";
        indextitle = "Index By SAS Name";
        indexout = "&ind2";
      end;

      sortorder = trim(sortorder);
      indextitle = trim(indextitle);
      indexout = compress(indexout);
    
      call symput('startpage',left(compress(startpage)));
      call symput('sortorder',left(trim(sortorder)));
      call symput('indextitle',left(trim(indextitle)));
      call symput('indexout',left(compress(indexout)));
   
    run;
  
    data _null_;
      * put these values into the lst file just in case.;
      file print;

      put / "For index by &which we are using the following";
      put "     Title:       &indextitle";
      put "     Start page:  &startpage";
      put "     Sort :       &sortorder";
      put "     File :       &indexout" //;

    run;
 
    * sort the index dataset by the appropriate variables.                    ;
    proc sort data = indexout_&surv;
      by &sortorder;
    run;

    data index&ftype.&which.;
      set indexout_&surv end = eof;
      by &sortorder;
      
      file "&indexout";

      * set initial values for parameters;
      retain vpos &ipagesize.;
      retain vincrmnt 10;
      retain ipageno &startpage;
 
      * the cutoff is where we stop printing to the current;
      * page, print the footer, and move to the next page  ;
      cutoff = &ifooterstart.;

      * Indicator if version note should be printed, initialize to zero;
      retain chgflag 0;

      * At the start of the index, put a title page and set the font.;
      if _n_ = 1 then do;
        %titlep;
        put @1 '/Courier findfont 9 scalefont setfont                       ';
      end;
 
      * now output a line for each sas name;
      if first.sas_name then do;
      
        * If the sas_name has a * in it, then we know if has changed and we  ;
        * will need to account for this in the footnotes.                    ;
        if index(SAS_NAME,'*') then chgflag = 1;

        * if we are at the end of the page, print the footer and move on.    ;
        if vpos lt cutoff then do;
          %footerp;
        end;

        * print the variable information                                     ;
        %grphead;
      end;

      * at the end of the dataset, finish off the page.                       ;
      if eof then do;
        %footerp;
      end;
    run;
    
  %mend makeindex;
  %makeindex(which=source);
  %makeindex(which=sas_name);                                                                    

  ****************************************************************************;
  * Step 5:  Now we can put together all of these text files into one final  *;
  *          post-script file.                                               *;
  ****************************************************************************;

  * first we create the header and output to the post-script file.            ;
  data _null_;

    * Use the value of file type macro variable (ftype), determine if this    ;
    * will be appendix E (base) or appendix F (text).                         ;
    ftype = upcase("&ftype");
    if ftype = 'BASE' then app = 'E';
    else app = 'F';
    apptype = app||" - "||ftype;

    * Determine the vertical position for the title line and the line below.   ;
    vpos1 = &vpagesize - 10;
    vpos2 = vpos1 - 20;
    
    * The text of the second line is the year and the survey.                  ;
    line2 = upcase("&year. &surv");

    file "&psfile";

    * At the very beginning of the file, we need the header information;
    put @1 '%!PS-Adobe-2.0                                                    ';
    put @1 '%%Title: Blue Book Program 7, on page 157                         ';
    put @1 '%%Creator: Adobe Systems Incorporated                             ';
    put @1 '%%CreationDate: Thu Dec 28 13:23:13 PST 1989                      ';
    put @1 '%%EndComments                                                     ';
    put @1 '                                                                  ';
    put @1 '/scdict 3 dict def                                                ';
    put @1 '/scshow                                                           ';
    put @1 '        { scdict begin                                            ';
    put @1 '                                                                  ';
    put @1 '          gsave                                                   ';
    put @1 '            currentfont [.9 0 0 findscscale 0 0] makefont         ';
    put @1 '            setfont                                               ';
    put @1 '            show                                                  ';
    put @1 '            currentpoint                                          ';
    put @1 '          grestore                                                ';
    put @1 '          moveto                                                  ';
    put @1 '          end                                                     ';
    put @1 '        } def                                                     ';
    put @1 '                                                                  ';
    put @1 'scdict begin                                                      ';
    put @1 '        /findscscale                                              ';
    put @1 '                { gsave                                           ';
    put @1 '                    newpath                                       ';
    put @1 '                    0 0 moveto                                    ';
    put @1 '                    (X) true charpath                             ';
    put @1 '                    flattenpath                                   ';
    put @1 '                    pathbbox /capheight exch def pop pop pop      ';
    put @1 '                    newpath                                       ';
    put @1 '                    0 0 moveto                                    ';
    put @1 '                    (x) true charpath                             ';
    put @1 '                    flattenpath                                   ';
    put @1 '                    pathbbox /xheight exch def pop pop pop        ';
    put @1 '                  grestore                                        ';
    put @1 '                                                                  ';
    put @1 '                  xheight capheight xheight sub 3 div add         ';
    put @1 '                  capheight div                                   ';
    put @1 '                } def                                             ';
    put @1 'end                                                               ';
    put @1 '                                                                  ';

    * Done with header.  Now we create the title page.                         ;

    put @1 '/Helvetica-Bold findfont 11 scalefont setfont                     ';
    put @1 '72 150 add ' vpos1 'moveto (APPENDIX ' apptype ' FILE TYPE) show  ';
    put @1 '/Helvetica-Bold findfont 10 scalefont setfont                     ';
    put @1 '72 210 add ' vpos2 'moveto (' line2 ') show                       ';
    put @1 'showpage                                                          ';

  run;

  * next we modify the post-script file to add the index by source.           ;
  data _null_;
    infile "&ind1";
    input;
    file "&psfile" mod;
    put _infile_;
  run;

  * next we modify the post-script file to add the index by sas name.         ;
  data _null_;
    infile "&ind2";
    input;
    file "&psfile" mod;
    put _infile_;
  run;

  * finally we modify the post-script file to add the variable descriptions.  ;
  data _null_;
    infile "&vars";
    input;
    file "&psfile" mod;
    put _infile_;
  run;

  * Run the pdf distiller program.                                            ;
  x "&psfile";

  * Remove the text files as we no longer need them.                          ;
  x "del &ind1";
  x "del &ind2";
  x "del &vars";

  * Put sas to sleep for 7 seconds so the PDF distiller has time to run.      ;
  data _null_;
    x = sleep(7);
  run;

%mend main;
%main(surv=sdr,ftype=base);
%main(surv=sdr,ftype=text);

%main(surv=nscg,ftype=base);
%main(surv=nscg,ftype=text);

%main(surv=nsrcg,ftype=base);
%main(surv=nsrcg,ftype=text);

%main(surv=nscgn,ftype=base);
%main(surv=nscgn,ftype=text);

