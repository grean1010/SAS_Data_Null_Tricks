data _null_;

  file print;

  put "Hi Maria!";
  put ;
  put "  How is your day today?";
  put ;
  put "Love,";
  put "SAS";

run;

data indata;
  do i = 1 to 3000;
    testvar = ranuni(i) * 3000;
    output;
  end;
  keep testvar;
run;


data outdata;
  set indata end=lastobs;
  if _n_ = 1 then do;
    num1 = 0;
    num2 = 0;
    num3 = 0;
  end;

  retain num1 num2 num3;

  if 0 <= testvar <= 500 then num1 = num1 + 1;
  if 200<= testvar <= 2500 then num2 = num2 + 1;
  if 1000 <= testvar <= 5000 then num3 = num3 + 1;

  if lastobs then do;
    call symput('num1',num1);
    call symput('num2',num2);
    call symput('num3',num3);
    call symput('total',_n_);
  end;
run;

data _null_;
  file print;
  put "Processed dataset INDATA";
  put "  Total Number of Observations:               "         @60 "&total";
  put "  Number of Observations with 0 <= testvar <= 500:"     @60 "&num1";
  put "  Number of Observations with 200 <= testvar <= 2500:"  @60 "&num2";
  put "  Number of Observations with 1000 <= testvar <= 5000:" @60 "&num3";
run;

