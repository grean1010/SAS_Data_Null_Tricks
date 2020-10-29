filename mymail email "mhudson@whereeveryouare.com"
         subject="Busted!"
         attach="C:\Docume~1\mhudson\SAS_Data_Null_Tricks\testemail.sas";

data _null_;
   file mymail;
   put 'Dear Maria,'//;
   put 'Your program is done running.';
   put 'Please get off the internet and get back to work.'//;
   put 'Sincerely,';
   put 'SAS' //;
run;
