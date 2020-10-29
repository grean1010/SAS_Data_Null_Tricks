/* This is the first include file used in the program SDR10.sas.               */
/* It checks skip patterns and looks for logical consistency between           */
/* variables submitted by the survey contractors.                              */


/* Logic check variable prob00010 :
Check that WRKG always meets the condition (WRKG NE 'L').  */

if (WRKG NE 'L')  then prob00010  = 0 ;
else prob00010  = 1 ;



/* Logic check variable prob00020 :
Check that if the first condition (LOOKWK  IN ('Y','N')) is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (LOOKWK  IN ('Y','N'))  and (WRKG = 'Y')  then prob00020  = 1 ;
else prob00020  = 0 ;



/* Logic check variable prob00030 :
Check that if the first condition (LOOKWK = 'L') is true then the second condition (WRKG = 'N') cannot also be true.  */

if (LOOKWK = 'L')  and (WRKG = 'N')  then prob00030  = 1 ;
else prob00030  = 0 ;



/* Logic check variable prob00040 :
Check that if the first condition (NWRET  IN ('Y','N')) is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (NWRET  IN ('Y','N'))  and (WRKG = 'Y')  then prob00040  = 1 ;
else prob00040  = 0 ;



/* Logic check variable prob00050 :
Check that if the first condition (NWRET = 'L') is true then the second condition (WRKG = 'N') cannot also be true.  */

if (NWRET = 'L')  and (WRKG = 'N')  then prob00050  = 1 ;
else prob00050  = 0 ;



/* Logic check variable prob00060 :
Check that if the first condition ((1949<=NWRTYR <= 2010) OR NWRTYR=9999) is true then the second condition (NWRET IN ('N','L')) cannot also be true.  */

if ((1949<=NWRTYR <= 2010) OR NWRTYR=9999)  and (NWRET IN ('N','L'))  then prob00060  = 1 ;
else prob00060  = 0 ;



/* Logic check variable prob00070 :
Check that if the first condition (NWRTYR=9998) is true then the second condition (NWRET = 'Y') cannot also be true.  */

if (NWRTYR=9998)  and (NWRET = 'Y')  then prob00070  = 1 ;
else prob00070  = 0 ;



/* Logic check variable prob00080 :
Check that if the first condition (NWLAY IN ('Y','N')) is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (NWLAY IN ('Y','N'))  and (WRKG = 'Y')  then prob00080  = 1 ;
else prob00080  = 0 ;



/* Logic check variable prob00090 :
Check that if the first condition (NWLAY='L') is true then the second condition (WRKG = 'N') cannot also be true.  */

if (NWLAY='L')  and (WRKG = 'N')  then prob00090  = 1 ;
else prob00090  = 0 ;



/* Logic check variable prob00100 :
Check that if the first condition (NWSTU IN ('Y','N')) is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (NWSTU IN ('Y','N'))  and (WRKG = 'Y')  then prob00100  = 1 ;
else prob00100  = 0 ;



/* Logic check variable prob00110 :
Check that if the first condition (NWSTU='L') is true then the second condition (WRKG = 'N') cannot also be true.  */

if (NWSTU='L')  and (WRKG = 'N')  then prob00110  = 1 ;
else prob00110  = 0 ;



/* Logic check variable prob00120 :
Check that if the first condition (NWFAM IN ('Y','N')) is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (NWFAM IN ('Y','N'))  and (WRKG = 'Y')  then prob00120  = 1 ;
else prob00120  = 0 ;



/* Logic check variable prob00130 :
Check that if the first condition (NWFAM='L') is true then the second condition (WRKG = 'N') cannot also be true.  */

if (NWFAM='L')  and (WRKG = 'N')  then prob00130  = 1 ;
else prob00130  = 0 ;



/* Logic check variable prob00140 :
Check that if the first condition (NWILL IN ('Y','N')) is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (NWILL IN ('Y','N'))  and (WRKG = 'Y')  then prob00140  = 1 ;
else prob00140  = 0 ;



/* Logic check variable prob00150 :
Check that if the first condition (NWILL='L') is true then the second condition (WRKG = 'N') cannot also be true.  */

if (NWILL='L')  and (WRKG = 'N')  then prob00150  = 1 ;
else prob00150  = 0 ;



/* Logic check variable prob00160 :
Check that if the first condition (NWOCNA  IN ('Y','N')) is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (NWOCNA  IN ('Y','N'))  and (WRKG = 'Y')  then prob00160  = 1 ;
else prob00160  = 0 ;



/* Logic check variable prob00170 :
Check that if the first condition (NWOCNA='L') is true then the second condition (WRKG = 'N') cannot also be true.  */

if (NWOCNA='L')  and (WRKG = 'N')  then prob00170  = 1 ;
else prob00170  = 0 ;



/* Logic check variable prob00180 :
Check that if the first condition (NWNOND IN ('Y','N')) is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (NWNOND IN ('Y','N'))  and (WRKG = 'Y')  then prob00180  = 1 ;
else prob00180  = 0 ;



/* Logic check variable prob00190 :
Check that if the first condition (NWNOND='L') is true then the second condition (WRKG = 'N') cannot also be true.  */

if (NWNOND='L')  and (WRKG = 'N')  then prob00190  = 1 ;
else prob00190  = 0 ;



/* Logic check variable prob00200 :
Check that if the first condition (NWOT IN ('Y','N')) is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (NWOT IN ('Y','N'))  and (WRKG = 'Y')  then prob00200  = 1 ;
else prob00200  = 0 ;



/* Logic check variable prob00210 :
Check that if the first condition (NWOT='L') is true then the second condition (WRKG = 'N') cannot also be true.  */

if (NWOT='L')  and (WRKG = 'N')  then prob00210  = 1 ;
else prob00210  = 0 ;



/* Logic check variable prob00220 :
Check that if the first condition (NWOTV NE 'L') is true then the second condition (NWOT NE 'Y') cannot also be true.  */

if (NWOTV NE 'L')  and (NWOT NE 'Y')  then prob00220  = 1 ;
else prob00220  = 0 ;



/* Logic check variable prob00230 :
Check that if the first condition (NWOTV='L') is true then the second condition (NWOT = 'Y') cannot also be true.  */

if (NWOTV='L')  and (NWOT = 'Y')  then prob00230  = 1 ;
else prob00230  = 0 ;



/* Logic check variable prob00240 :
Check that if the first condition (LWNVR IN ('Y','N')) is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (LWNVR IN ('Y','N'))  and (WRKG = 'Y')  then prob00240  = 1 ;
else prob00240  = 0 ;



/* Logic check variable prob00250 :
Check that if the first condition (LWNVR='L') is true then the second condition (WRKG = 'N') cannot also be true.  */

if (LWNVR='L')  and (WRKG = 'N')  then prob00250  = 1 ;
else prob00250  = 0 ;



/* Logic check variable prob00260 :
Check that if the first condition (01<=LWMN<=12) is true then the second condition (LWNVR IN ('Y','L')) cannot also be true.  */

if (01<=LWMN<=12)  and (LWNVR IN ('Y','L'))  then prob00260  = 1 ;
else prob00260  = 0 ;



/* Logic check variable prob00270 :
Check that if the first condition (LWMN=98) is true then the second condition (LWNVR = 'N') cannot also be true.  */

if (LWMN=98)  and (LWNVR = 'N')  then prob00270  = 1 ;
else prob00270  = 0 ;



/* Logic check variable prob00280 :
Check that if the first condition (1949<=LWYR<=2010) is true then the second condition (LWNVR IN ('Y','L')) cannot also be true.  */

if (1949<=LWYR<=2010)  and (LWNVR IN ('Y','L'))  then prob00280  = 1 ;
else prob00280  = 0 ;



/* Logic check variable prob00290 :
Check that if the first condition (LWYR=9998) is true then the second condition (LWNVR = 'N') cannot also be true.  */

if (LWYR=9998)  and (LWNVR = 'N')  then prob00290  = 1 ;
else prob00290  = 0 ;



/* Logic check variable prob00300 :
Check that if the first condition (LWYR=2010) is true then the second condition (LWMN IN (10,11,12,98)) cannot also be true.  */

if (LWYR=2010)  and (LWMN IN (10,11,12,98))  then prob00300  = 1 ;
else prob00300  = 0 ;



/* Logic check variable prob00310 :
Check that if the first condition (OCLSTT='L') is true then the second condition (WRKG = 'N' AND LWNVR = 'N') cannot also be true.  */

if (OCLSTT='L')  and (WRKG = 'N' AND LWNVR = 'N')  then prob00310  = 1 ;
else prob00310  = 0 ;



/* Logic check variable prob00320 :
Check that if the first condition (OCLSTT NE 'L') is true then the second condition (WRKG = 'Y' OR LWNVR = 'Y') cannot also be true.  */

if (OCLSTT NE 'L')  and (WRKG = 'Y' OR LWNVR = 'Y')  then prob00320  = 1 ;
else prob00320  = 0 ;



/* Logic check variable prob00330 :
Check that if the first condition (OCLSTV='L') is true then the second condition (WRKG = 'N' AND LWNVR = 'N') cannot also be true.  */

if (OCLSTV='L')  and (WRKG = 'N' AND LWNVR = 'N')  then prob00330  = 1 ;
else prob00330  = 0 ;



/* Logic check variable prob00340 :
Check that if the first condition (OCLSTV NE 'L') is true then the second condition (WRKG = 'Y' OR LWNVR = 'Y') cannot also be true.  */

if (OCLSTV NE 'L')  and (WRKG = 'Y' OR LWNVR = 'Y')  then prob00340  = 1 ;
else prob00340  = 0 ;



/* Logic check variable prob00350 :
Check that if the first condition (('010'<=N2OCLST<='500')) is true then the second condition (WRKG = 'Y' OR LWNVR = 'Y') cannot also be true.  */

if (('010'<=N2OCLST<='500'))  and (WRKG = 'Y' OR LWNVR = 'Y')  then prob00350  = 1 ;
else prob00350  = 0 ;



/* Logic check variable prob00360 :
Check that if the first condition (N2OCLST='998') is true then the second condition (WRKG = 'N' AND LWNVR = 'N') cannot also be true.  */

if (N2OCLST='998')  and (WRKG = 'N' AND LWNVR = 'N')  then prob00360  = 1 ;
else prob00360  = 0 ;



/* Logic check variable prob00370 :
Check that if the first condition (EMNAME='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (EMNAME='L')  and (WRKG='Y')  then prob00370  = 1 ;
else prob00370  = 0 ;



/* Logic check variable prob00380 :
Check that if the first condition (EMDEPT='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (EMDEPT='L')  and (WRKG='Y')  then prob00380  = 1 ;
else prob00380  = 0 ;



/* Logic check variable prob00390 :
Check that if the first condition (EMCITY='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (EMCITY='L')  and (WRKG='Y')  then prob00390  = 1 ;
else prob00390  = 0 ;



/* Logic check variable prob00400 :
Check that if the first condition (EMSTATE='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (EMSTATE='L')  and (WRKG='Y')  then prob00400  = 1 ;
else prob00400  = 0 ;



/* Logic check variable prob00410 :
Check that if the first condition (EMZIP='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (EMZIP='L')  and (WRKG='Y')  then prob00410  = 1 ;
else prob00410  = 0 ;



/* Logic check variable prob00420 :
Check that if the first condition (('000'<=EMST='555')) is true then the second condition (WRKG='N') cannot also be true.  */

if (('000'<=EMST='555'))  and (WRKG='N')  then prob00420  = 1 ;
else prob00420  = 0 ;



/* Logic check variable prob00430 :
Check that if the first condition (EMST='998') is true then the second condition (WRKG='Y') cannot also be true.  */

if (EMST='998')  and (WRKG='Y')  then prob00430  = 1 ;
else prob00430  = 0 ;



/* Logic check variable prob00440 :
Check that if the first condition (EMMAIN='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (EMMAIN='L')  and (WRKG='Y')  then prob00440  = 1 ;
else prob00440  = 0 ;



/* Logic check variable prob00450 :
Check that if the first condition (EMSIZE >='1' AND EMSIZE<='8') is true then the second condition (WRKG='N') cannot also be true.  */

if (EMSIZE >='1' AND EMSIZE<='8')  and (WRKG='N')  then prob00450  = 1 ;
else prob00450  = 0 ;



/* Logic check variable prob00460 :
Check that if the first condition (EMSIZE='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (EMSIZE='L')  and (WRKG='Y')  then prob00460  = 1 ;
else prob00460  = 0 ;



/* Logic check variable prob00470 :
Check that if the first condition (NEDTP >='1' AND NEDTP<='9') is true then the second condition (WRKG='N') cannot also be true.  */

if (NEDTP >='1' AND NEDTP<='9')  and (WRKG='N')  then prob00470  = 1 ;
else prob00470  = 0 ;



/* Logic check variable prob00480 :
Check that if the first condition (NEDTP IN ('7','8')) is true then the second condition (EMSIZE NE '8') cannot also be true.  */

if (NEDTP IN ('7','8'))  and (EMSIZE NE '8')  then prob00480  = 1 ;
else prob00480  = 0 ;



/* Logic check variable prob00481 :
Check that if the first condition (NEDTP IN ('7','8')) is true then the second condition (GOVSUP NOT IN ("0","2")  and STRTYR < 2010) cannot also be true.  */

if (NEDTP IN ('7','8'))  and (GOVSUP NOT IN ("0","2")  and STRTYR < 2010)  then prob00481  = 1 ;
else prob00481  = 0 ;



/* Logic check variable prob00490 :
Check that if the first condition (NEDTP='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (NEDTP='L')  and (WRKG='Y')  then prob00490  = 1 ;
else prob00490  = 0 ;



/* Logic check variable prob00500 :
Check that if the first condition (NEDTPV NE 'L') is true then the second condition (NEDTP NE '9') cannot also be true.  */

if (NEDTPV NE 'L')  and (NEDTP NE '9')  then prob00500  = 1 ;
else prob00500  = 0 ;



/* Logic check variable prob00510 :
Check that if the first condition (NEDTPV='L') is true then the second condition (NEDTP = '9') cannot also be true.  */

if (NEDTPV='L')  and (NEDTP = '9')  then prob00510  = 1 ;
else prob00510  = 0 ;



/* Logic check variable prob00520 :
Check that if the first condition (EMED IN ('Y','N')) is true then the second condition (WRKG = 'N') cannot also be true.  */

if (EMED IN ('Y','N'))  and (WRKG = 'N')  then prob00520  = 1 ;
else prob00520  = 0 ;



/* Logic check variable prob00530 :
Check that if the first condition (EMED='L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (EMED='L')  and (WRKG = 'Y')  then prob00530  = 1 ;
else prob00530  = 0 ;



/* Logic check variable prob00540 :
Check that if the first condition (EDTP >= '1' AND EDTP <= '6') is true then the second condition (EMED IN ('N','L')) cannot also be true.  */

if (EDTP >= '1' AND EDTP <= '6')  and (EMED IN ('N','L'))  then prob00540  = 1 ;
else prob00540  = 0 ;



/* Logic check variable prob00550 :
Check that if the first condition (EDTP='L') is true then the second condition (EMED = 'Y') cannot also be true.  */

if (EDTP='L')  and (EMED = 'Y')  then prob00550  = 1 ;
else prob00550  = 0 ;



/* Logic check variable prob00560 :
Check that if the first condition (EDTPV NE 'L') is true then the second condition (EDTP NE '6') cannot also be true.  */

if (EDTPV NE 'L')  and (EDTP NE '6')  then prob00560  = 1 ;
else prob00560  = 0 ;



/* Logic check variable prob00570 :
Check that if the first condition (EDTPV='L') is true then the second condition (EDTP = '6') cannot also be true.  */

if (EDTPV='L')  and (EDTP = '6')  then prob00570  = 1 ;
else prob00570  = 0 ;



/* Logic check variable prob00580 :
Check that if the first condition (('000000'<=INSTCOD<='999996') OR INSTCOD IN ('999999','777777')) is true then the second condition (EMED IN ('N','L') OR EDTP = '1') cannot also be true.  */

if (('000000'<=INSTCOD<='999996') OR INSTCOD IN ('999999','777777'))  and (EMED IN ('N','L') OR EDTP = '1')  then prob00580  = 1 ;
else prob00580  = 0 ;



/* Logic check variable prob00590 :
Check that if the first condition (PUBPRI IN ('1','2')) is true then the second condition (EMED IN ('N','L') OR EDTP = '1' OR INSTCOD = '777777') cannot also be true.  */

if (PUBPRI IN ('1','2'))  and (EMED IN ('N','L') OR EDTP = '1' OR INSTCOD = '777777')  then prob00590  = 1 ;
else prob00590  = 0 ;



/* Logic check variable prob00600 :
Check that if the first condition (PUBPRI='L') is true then the second condition (EMED = 'Y' AND (EDTP ^='1') AND INSTCOD NE '777777') cannot also be true.  */

if (PUBPRI='L')  and (EMED = 'Y' AND (EDTP ^='1') AND INSTCOD NE '777777')  then prob00600  = 1 ;
else prob00600  = 0 ;



/* Logic check variable prob00610 :
Check that if the first condition (CARNEG IN ('11','12','13','14','21','22','31','32','40','51','52','53','54','55','56','57','58','59','60','65')) is true then the second condition (EMED IN ('N','L') OR EDTP = '1' OR INSTCOD = '777777') cannot also be tru
e.  */

if (CARNEG IN ('11','12','13','14','21','22','31','32','40','51','52','53','54','55','56','57','58','59','60','65'))  and (EMED IN ('N','L') OR EDTP = '1' OR INSTCOD = '777777')  then prob00610  = 1 ;
else prob00610  = 0 ;



/* Logic check variable prob00620 :
Check that if the first condition (CARNEG = 'L') is true then the second condition (EMED = 'Y' AND (EDTP ^='1') AND INSTCOD NE '777777') cannot also be true.  */

if (CARNEG = 'L')  and (EMED = 'Y' AND (EDTP ^='1') AND INSTCOD NE '777777')  then prob00620  = 1 ;
else prob00620  = 0 ;



/* Logic check variable prob00630 :
Check that if the first condition (PBPR05C IN ('1','2')) is true then the second condition (EMED IN ('N','L') OR EDTP = '1' OR INSTCOD = '777777') cannot also be true.  */

if (PBPR05C IN ('1','2'))  and (EMED IN ('N','L') OR EDTP = '1' OR INSTCOD = '777777')  then prob00630  = 1 ;
else prob00630  = 0 ;



/* Logic check variable prob00640 :
Check that if the first condition (PBPR05C='L') is true then the second condition (EMED = 'Y' AND (EDTP ^='1') AND INSTCOD NE '777777') cannot also be true.  */

if (PBPR05C='L')  and (EMED = 'Y' AND (EDTP ^='1') AND INSTCOD NE '777777')  then prob00640  = 1 ;
else prob00640  = 0 ;



/* Logic check variable prob00650 :
Check that if the first condition (CARN05C >= '00' AND CARN05C <= '33') is true then the second condition (EMED IN ('N','L') OR EDTP = '1' OR INSTCOD = '777777') cannot also be true.  */

if (CARN05C >= '00' AND CARN05C <= '33')  and (EMED IN ('N','L') OR EDTP = '1' OR INSTCOD = '777777')  then prob00650  = 1 ;
else prob00650  = 0 ;



/* Logic check variable prob00660 :
Check that if the first condition (CARN05C = 'L') is true then the second condition (EMED = 'Y' AND (EDTP ^='1') AND INSTCOD NE '777777') cannot also be true.  */

if (CARN05C = 'L')  and (EMED = 'Y' AND (EDTP ^='1') AND INSTCOD NE '777777')  then prob00660  = 1 ;
else prob00660  = 0 ;



/* Logic check variable prob00670 :
Check that if the first condition (OCPRT='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (OCPRT='L')  and (WRKG='Y')  then prob00670  = 1 ;
else prob00670  = 0 ;



/* Logic check variable prob00680 :
Check that if the first condition (OCPRT NE 'L') is true then the second condition (WRKG='N') cannot also be true.  */

if (OCPRT NE 'L')  and (WRKG='N')  then prob00680  = 1 ;
else prob00680  = 0 ;



/* Logic check variable prob00690 :
Check that if the first condition (OCPRV='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (OCPRV='L')  and (WRKG='Y')  then prob00690  = 1 ;
else prob00690  = 0 ;



/* Logic check variable prob00700 :
Check that if the first condition (OCPRV NE 'L') is true then the second condition (WRKG='N') cannot also be true.  */

if (OCPRV NE 'L')  and (WRKG='N')  then prob00700  = 1 ;
else prob00700  = 0 ;



/* Logic check variable prob00710 :
Check that if the first condition (('010'<=N2OCPR<='500')) is true then the second condition (WRKG='N') cannot also be true.  */

if (('010'<=N2OCPR<='500'))  and (WRKG='N')  then prob00710  = 1 ;
else prob00710  = 0 ;



/* Logic check variable prob00720 :
Check that if the first condition (N2OCPR='998') is true then the second condition (WRKG='Y') cannot also be true.  */

if (N2OCPR='998')  and (WRKG='Y')  then prob00720  = 1 ;
else prob00720  = 0 ;



/* Logic check variable prob00730 :
Check that if the first condition (MGRNAT IN ('Y','N')) is true then the second condition (WRKG='N') cannot also be true.  */

if (MGRNAT IN ('Y','N'))  and (WRKG='N')  then prob00730  = 1 ;
else prob00730  = 0 ;



/* Logic check variable prob00740 :
Check that if the first condition (MGRNAT = 'L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (MGRNAT = 'L')  and (WRKG='Y')  then prob00740  = 1 ;
else prob00740  = 0 ;



/* Logic check variable prob00750 :
Check that if the first condition (MGRSOC IN ('Y','N')) is true then the second condition (WRKG='N') cannot also be true.  */

if (MGRSOC IN ('Y','N'))  and (WRKG='N')  then prob00750  = 1 ;
else prob00750  = 0 ;



/* Logic check variable prob00760 :
Check that if the first condition (MGRSOC = 'L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (MGRSOC = 'L')  and (WRKG='Y')  then prob00760  = 1 ;
else prob00760  = 0 ;



/* Logic check variable prob00770 :
Check that if the first condition (MGROTH IN ('Y','N')) is true then the second condition (WRKG='N') cannot also be true.  */

if (MGROTH IN ('Y','N'))  and (WRKG='N')  then prob00770  = 1 ;
else prob00770  = 0 ;



/* Logic check variable prob00780 :
Check that if the first condition (MGROTH = 'L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (MGROTH = 'L')  and (WRKG='Y')  then prob00780  = 1 ;
else prob00780  = 0 ;



/* Logic check variable prob00790 :
Check that if the first condition (MGROTV = 'L') is true then the second condition (MGROTH = 'Y') cannot also be true.  */

if (MGROTV = 'L')  and (MGROTH = 'Y')  then prob00790  = 1 ;
else prob00790  = 0 ;



/* Logic check variable prob00800 :
Check that if the first condition (MGROTV NE 'L') is true then the second condition (MGROTH  = 'N') cannot also be true.  */

if (MGROTV NE 'L')  and (MGROTH  = 'N')  then prob00800  = 1 ;
else prob00800  = 0 ;



/* Logic check variable prob00810 :
Check that if the first condition (1<=STRTMN<=12) is true then the second condition (WRKG='N') cannot also be true.  */

if (1<=STRTMN<=12)  and (WRKG='N')  then prob00810  = 1 ;
else prob00810  = 0 ;



/* Logic check variable prob00820 :
Check that if the first condition (STRTMN=98) is true then the second condition (WRKG='Y') cannot also be true.  */

if (STRTMN=98)  and (WRKG='Y')  then prob00820  = 1 ;
else prob00820  = 0 ;



/* Logic check variable prob00830 :
Check that if the first condition (1949<=STRTYR<=2010) is true then the second condition (WRKG='N') cannot also be true.  */

if (1949<=STRTYR<=2010)  and (WRKG='N')  then prob00830  = 1 ;
else prob00830  = 0 ;



/* Logic check variable prob00840 :
Check that if the first condition (STRTYR=9998) is true then the second condition (WRKG='Y') cannot also be true.  */

if (STRTYR=9998)  and (WRKG='Y')  then prob00840  = 1 ;
else prob00840  = 0 ;



/* Logic check variable prob00850 :
Check that if the first condition (STRTYR=2010) is true then the second condition (STRTMN IN (11,12,98)) cannot also be true.  */

if (STRTYR=2010)  and (STRTMN IN (11,12,98))  then prob00850  = 1 ;
else prob00850  = 0 ;



/* Logic check variable prob00860 :
Check that if the first condition (OCEDRLP IN ('1','2','3')) is true then the second condition (WRKG='N') cannot also be true.  */

if (OCEDRLP IN ('1','2','3'))  and (WRKG='N')  then prob00860  = 1 ;
else prob00860  = 0 ;



/* Logic check variable prob00870 :
Check that if the first condition (OCEDRLP='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (OCEDRLP='L')  and (WRKG='Y')  then prob00870  = 1 ;
else prob00870  = 0 ;



/* Logic check variable prob00880 :
Check that if the first condition (NRPAY IN ('Y','N')) is true then the second condition (OCEDRLP IN ('1','2','L')) cannot also be true.  */

if (NRPAY IN ('Y','N'))  and (OCEDRLP IN ('1','2','L'))  then prob00880  = 1 ;
else prob00880  = 0 ;



/* Logic check variable prob00890 :
Check that if the first condition (NRPAY='L') is true then the second condition (OCEDRLP = '3') cannot also be true.  */

if (NRPAY='L')  and (OCEDRLP = '3')  then prob00890  = 1 ;
else prob00890  = 0 ;



/* Logic check variable prob00900 :
Check that if the first condition (NRCON IN ('Y','N')) is true then the second condition (OCEDRLP IN ('1','2','L')) cannot also be true.  */

if (NRCON IN ('Y','N'))  and (OCEDRLP IN ('1','2','L'))  then prob00900  = 1 ;
else prob00900  = 0 ;



/* Logic check variable prob00910 :
Check that if the first condition (NRCON='L') is true then the second condition (OCEDRLP = '3') cannot also be true.  */

if (NRCON='L')  and (OCEDRLP = '3')  then prob00910  = 1 ;
else prob00910  = 0 ;



/* Logic check variable prob00920 :
Check that if the first condition (NRLOC IN ('Y','N')) is true then the second condition (OCEDRLP IN ('1','2','L')) cannot also be true.  */

if (NRLOC IN ('Y','N'))  and (OCEDRLP IN ('1','2','L'))  then prob00920  = 1 ;
else prob00920  = 0 ;



/* Logic check variable prob00930 :
Check that if the first condition (NRLOC='L') is true then the second condition (OCEDRLP = '3') cannot also be true.  */

if (NRLOC='L')  and (OCEDRLP = '3')  then prob00930  = 1 ;
else prob00930  = 0 ;



/* Logic check variable prob00940 :
Check that if the first condition (NRCHG IN ('Y','N')) is true then the second condition (OCEDRLP IN ('1','2','L')) cannot also be true.  */

if (NRCHG IN ('Y','N'))  and (OCEDRLP IN ('1','2','L'))  then prob00940  = 1 ;
else prob00940  = 0 ;



/* Logic check variable prob00950 :
Check that if the first condition (NRCHG='L') is true then the second condition (OCEDRLP = '3') cannot also be true.  */

if (NRCHG='L')  and (OCEDRLP = '3')  then prob00950  = 1 ;
else prob00950  = 0 ;



/* Logic check variable prob00960 :
Check that if the first condition (NRFAM IN ('Y','N')) is true then the second condition (OCEDRLP IN ('1','2','L')) cannot also be true.  */

if (NRFAM IN ('Y','N'))  and (OCEDRLP IN ('1','2','L'))  then prob00960  = 1 ;
else prob00960  = 0 ;



/* Logic check variable prob00970 :
Check that if the first condition (NRFAM='L') is true then the second condition (OCEDRLP = '3') cannot also be true.  */

if (NRFAM='L')  and (OCEDRLP = '3')  then prob00970  = 1 ;
else prob00970  = 0 ;



/* Logic check variable prob00980 :
Check that if the first condition (NROCNA IN ('Y','N')) is true then the second condition (OCEDRLP IN ('1','2','L')) cannot also be true.  */

if (NROCNA IN ('Y','N'))  and (OCEDRLP IN ('1','2','L'))  then prob00980  = 1 ;
else prob00980  = 0 ;



/* Logic check variable prob00990 :
Check that if the first condition (NROCNA='L') is true then the second condition (OCEDRLP = '3') cannot also be true.  */

if (NROCNA='L')  and (OCEDRLP = '3')  then prob00990  = 1 ;
else prob00990  = 0 ;



/* Logic check variable prob01000 :
Check that if the first condition (NROT IN ('Y','N')) is true then the second condition (OCEDRLP IN ('1','2','L')) cannot also be true.  */

if (NROT IN ('Y','N'))  and (OCEDRLP IN ('1','2','L'))  then prob01000  = 1 ;
else prob01000  = 0 ;



/* Logic check variable prob01010 :
Check that if the first condition (NROT='L') is true then the second condition (OCEDRLP = '3') cannot also be true.  */

if (NROT='L')  and (OCEDRLP = '3')  then prob01010  = 1 ;
else prob01010  = 0 ;



/* Logic check variable prob01020 :
Check that if the first condition (NROTV NE 'L') is true then the second condition (NROT NE 'Y') cannot also be true.  */

if (NROTV NE 'L')  and (NROT NE 'Y')  then prob01020  = 1 ;
else prob01020  = 0 ;



/* Logic check variable prob01030 :
Check that if the first condition (NROTV='L') is true then the second condition (NROT = 'Y') cannot also be true.  */

if (NROTV='L')  and (NROT = 'Y')  then prob01030  = 1 ;
else prob01030  = 0 ;



/* Logic check variable prob01040 :
Check that if the first condition (NRREA>='1' AND NRREA<='7') is true then the second condition (OCEDRLP IN ('1','2','L')) cannot also be true.  */

if (NRREA>='1' AND NRREA<='7')  and (OCEDRLP IN ('1','2','L'))  then prob01040  = 1 ;
else prob01040  = 0 ;



/* Logic check variable prob01050 :
Check that if the first condition (NRREA='L') is true then the second condition (OCEDRLP = '3') cannot also be true.  */

if (NRREA='L')  and (OCEDRLP = '3')  then prob01050  = 1 ;
else prob01050  = 0 ;



/* Logic check variable prob01060 :
Check that NRREA always meets the condition (NRREA NE '0').  */

if (NRREA NE '0')  then prob01060  = 0 ;
else prob01060  = 1 ;



/* Logic check variable prob01070 :
Check that if the first condition (NRREA = '1') is true then the second condition (NRPAY NE 'Y') cannot also be true.  */

if (NRREA = '1')  and (NRPAY NE 'Y')  then prob01070  = 1 ;
else prob01070  = 0 ;



/* Logic check variable prob01080 :
Check that if the first condition (NRREA = '2') is true then the second condition (NRCON NE 'Y') cannot also be true.  */

if (NRREA = '2')  and (NRCON NE 'Y')  then prob01080  = 1 ;
else prob01080  = 0 ;



/* Logic check variable prob01090 :
Check that if the first condition (NRREA = '3') is true then the second condition (NRLOC NE 'Y') cannot also be true.  */

if (NRREA = '3')  and (NRLOC NE 'Y')  then prob01090  = 1 ;
else prob01090  = 0 ;



/* Logic check variable prob01100 :
Check that if the first condition (NRREA = '4') is true then the second condition (NRCHG NE 'Y') cannot also be true.  */

if (NRREA = '4')  and (NRCHG NE 'Y')  then prob01100  = 1 ;
else prob01100  = 0 ;



/* Logic check variable prob01110 :
Check that if the first condition (NRREA = '5') is true then the second condition (NRFAM NE 'Y') cannot also be true.  */

if (NRREA = '5')  and (NRFAM NE 'Y')  then prob01110  = 1 ;
else prob01110  = 0 ;



/* Logic check variable prob01120 :
Check that if the first condition (NRREA = '6') is true then the second condition (NROCNA NE 'Y') cannot also be true.  */

if (NRREA = '6')  and (NROCNA NE 'Y')  then prob01120  = 1 ;
else prob01120  = 0 ;



/* Logic check variable prob01130 :
Check that if the first condition (NRREA = '7') is true then the second condition (NROT NE 'Y') cannot also be true.  */

if (NRREA = '7')  and (NROT NE 'Y')  then prob01130  = 1 ;
else prob01130  = 0 ;



/* Logic check variable prob01140 :
Check that if the first condition (NRSEC>='0' AND NRSEC<='7') is true then the second condition (OCEDRLP IN ('1','2','L')) cannot also be true.  */

if (NRSEC>='0' AND NRSEC<='7')  and (OCEDRLP IN ('1','2','L'))  then prob01140  = 1 ;
else prob01140  = 0 ;



/* Logic check variable prob01150 :
Check that if the first condition (NRSEC='L') is true then the second condition (OCEDRLP = '3') cannot also be true.  */

if (NRSEC='L')  and (OCEDRLP = '3')  then prob01150  = 1 ;
else prob01150  = 0 ;



/* Logic check variable prob01160 :
Check that if the first condition (NRSEC = '1') is true then the second condition (NRPAY NE 'Y') cannot also be true.  */

if (NRSEC = '1')  and (NRPAY NE 'Y')  then prob01160  = 1 ;
else prob01160  = 0 ;



/* Logic check variable prob01170 :
Check that if the first condition (NRSEC = '2') is true then the second condition (NRCON NE 'Y') cannot also be true.  */

if (NRSEC = '2')  and (NRCON NE 'Y')  then prob01170  = 1 ;
else prob01170  = 0 ;



/* Logic check variable prob01180 :
Check that if the first condition (NRSEC = '3') is true then the second condition (NRLOC NE 'Y') cannot also be true.  */

if (NRSEC = '3')  and (NRLOC NE 'Y')  then prob01180  = 1 ;
else prob01180  = 0 ;



/* Logic check variable prob01190 :
Check that if the first condition (NRSEC = '4') is true then the second condition (NRCHG NE 'Y') cannot also be true.  */

if (NRSEC = '4')  and (NRCHG NE 'Y')  then prob01190  = 1 ;
else prob01190  = 0 ;



/* Logic check variable prob01200 :
Check that if the first condition (NRSEC = '5') is true then the second condition (NRFAM NE 'Y') cannot also be true.  */

if (NRSEC = '5')  and (NRFAM NE 'Y')  then prob01200  = 1 ;
else prob01200  = 0 ;



/* Logic check variable prob01210 :
Check that if the first condition (NRSEC = '6') is true then the second condition (NROCNA NE 'Y') cannot also be true.  */

if (NRSEC = '6')  and (NROCNA NE 'Y')  then prob01210  = 1 ;
else prob01210  = 0 ;



/* Logic check variable prob01220 :
Check that if the first condition (NRSEC = '7') is true then the second condition (NROT NE 'Y') cannot also be true.  */

if (NRSEC = '7')  and (NROT NE 'Y')  then prob01220  = 1 ;
else prob01220  = 0 ;



/* Logic check variable prob01230 :
Check that if the first condition (WAACC IN ('Y','N')) is true then the second condition (WRKG = 'N') cannot also be true.  */

if (WAACC IN ('Y','N'))  and (WRKG = 'N')  then prob01230  = 1 ;
else prob01230  = 0 ;



/* Logic check variable prob01240 :
Check that if the first condition (WAACC='L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (WAACC='L')  and (WRKG = 'Y')  then prob01240  = 1 ;
else prob01240  = 0 ;



/* Logic check variable prob01250 :
Check that if the first condition (WABRSH IN ('Y','N')) is true then the second condition (WRKG = 'N') cannot also be true.  */

if (WABRSH IN ('Y','N'))  and (WRKG = 'N')  then prob01250  = 1 ;
else prob01250  = 0 ;



/* Logic check variable prob01260 :
Check that if the first condition (WABRSH='L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (WABRSH='L')  and (WRKG = 'Y')  then prob01260  = 1 ;
else prob01260  = 0 ;



/* Logic check variable prob01270 :
Check that if the first condition (WAAPRSH IN ('Y','N')) is true then the second condition (WRKG = 'N') cannot also be true.  */

if (WAAPRSH IN ('Y','N'))  and (WRKG = 'N')  then prob01270  = 1 ;
else prob01270  = 0 ;



/* Logic check variable prob01280 :
Check that if the first condition (WAAPRSH='L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (WAAPRSH='L')  and (WRKG = 'Y')  then prob01280  = 1 ;
else prob01280  = 0 ;



/* Logic check variable prob01290 :
Check that if the first condition (WADEV IN ('Y','N')) is true then the second condition (WRKG = 'N') cannot also be true.  */

if (WADEV IN ('Y','N'))  and (WRKG = 'N')  then prob01290  = 1 ;
else prob01290  = 0 ;



/* Logic check variable prob01300 :
Check that if the first condition (WADEV='L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (WADEV='L')  and (WRKG = 'Y')  then prob01300  = 1 ;
else prob01300  = 0 ;



/* Logic check variable prob01310 :
Check that if the first condition (WADSN IN ('Y','N')) is true then the second condition (WRKG = 'N') cannot also be true.  */

if (WADSN IN ('Y','N'))  and (WRKG = 'N')  then prob01310  = 1 ;
else prob01310  = 0 ;



/* Logic check variable prob01320 :
Check that if the first condition (WADSN='L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (WADSN='L')  and (WRKG = 'Y')  then prob01320  = 1 ;
else prob01320  = 0 ;



/* Logic check variable prob01330 :
Check that if the first condition (WACOM IN ('Y','N')) is true then the second condition (WRKG = 'N') cannot also be true.  */

if (WACOM IN ('Y','N'))  and (WRKG = 'N')  then prob01330  = 1 ;
else prob01330  = 0 ;



/* Logic check variable prob01340 :
Check that if the first condition (WACOM='L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (WACOM='L')  and (WRKG = 'Y')  then prob01340  = 1 ;
else prob01340  = 0 ;



/* Logic check variable prob01350 :
Check that if the first condition (WAEMRL IN ('Y','N')) is true then the second condition (WRKG = 'N') cannot also be true.  */

if (WAEMRL IN ('Y','N'))  and (WRKG = 'N')  then prob01350  = 1 ;
else prob01350  = 0 ;



/* Logic check variable prob01360 :
Check that if the first condition (WAEMRL='L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (WAEMRL='L')  and (WRKG = 'Y')  then prob01360  = 1 ;
else prob01360  = 0 ;



/* Logic check variable prob01370 :
Check that if the first condition (WAMGMT IN ('Y','N')) is true then the second condition (WRKG = 'N') cannot also be true.  */

if (WAMGMT IN ('Y','N'))  and (WRKG = 'N')  then prob01370  = 1 ;
else prob01370  = 0 ;



/* Logic check variable prob01380 :
Check that if the first condition (WAMGMT='L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (WAMGMT='L')  and (WRKG = 'Y')  then prob01380  = 1 ;
else prob01380  = 0 ;



/* Logic check variable prob01390 :
Check that if the first condition (WAPROD IN ('Y','N')) is true then the second condition (WRKG = 'N') cannot also be true.  */

if (WAPROD IN ('Y','N'))  and (WRKG = 'N')  then prob01390  = 1 ;
else prob01390  = 0 ;



/* Logic check variable prob01400 :
Check that if the first condition (WAPROD='L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (WAPROD='L')  and (WRKG = 'Y')  then prob01400  = 1 ;
else prob01400  = 0 ;



/* Logic check variable prob01410 :
Check that if the first condition (WASVC IN ('Y','N')) is true then the second condition (WRKG = 'N') cannot also be true.  */

if (WASVC IN ('Y','N'))  and (WRKG = 'N')  then prob01410  = 1 ;
else prob01410  = 0 ;



/* Logic check variable prob01420 :
Check that if the first condition (WASVC='L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (WASVC='L')  and (WRKG = 'Y')  then prob01420  = 1 ;
else prob01420  = 0 ;



/* Logic check variable prob01430 :
Check that if the first condition (WASALE IN ('Y','N')) is true then the second condition (WRKG = 'N') cannot also be true.  */

if (WASALE IN ('Y','N'))  and (WRKG = 'N')  then prob01430  = 1 ;
else prob01430  = 0 ;



/* Logic check variable prob01440 :
Check that if the first condition (WASALE='L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (WASALE='L')  and (WRKG = 'Y')  then prob01440  = 1 ;
else prob01440  = 0 ;



/* Logic check variable prob01450 :
Check that if the first condition (WAQM IN ('Y','N')) is true then the second condition (WRKG = 'N') cannot also be true.  */

if (WAQM IN ('Y','N'))  and (WRKG = 'N')  then prob01450  = 1 ;
else prob01450  = 0 ;



/* Logic check variable prob01460 :
Check that if the first condition (WAQM='L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (WAQM='L')  and (WRKG = 'Y')  then prob01460  = 1 ;
else prob01460  = 0 ;



/* Logic check variable prob01470 :
Check that if the first condition (WATEA IN ('Y','N')) is true then the second condition (WRKG = 'N') cannot also be true.  */

if (WATEA IN ('Y','N'))  and (WRKG = 'N')  then prob01470  = 1 ;
else prob01470  = 0 ;



/* Logic check variable prob01480 :
Check that if the first condition (WATEA='L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (WATEA='L')  and (WRKG = 'Y')  then prob01480  = 1 ;
else prob01480  = 0 ;



/* Logic check variable prob01490 :
Check that if the first condition (WAOT IN ('Y','N')) is true then the second condition (WRKG = 'N') cannot also be true.  */

if (WAOT IN ('Y','N'))  and (WRKG = 'N')  then prob01490  = 1 ;
else prob01490  = 0 ;



/* Logic check variable prob01500 :
Check that if the first condition (WAOT='L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (WAOT='L')  and (WRKG = 'Y')  then prob01500  = 1 ;
else prob01500  = 0 ;



/* Logic check variable prob01510 :
Check that if the first condition (WAOTV='L') is true then the second condition (WAOT = 'Y') cannot also be true.  */

if (WAOTV='L')  and (WAOT = 'Y')  then prob01510  = 1 ;
else prob01510  = 0 ;



/* Logic check variable prob01520 :
Check that if the first condition (WAOTV NE 'L') is true then the second condition (WAOT NE 'Y') cannot also be true.  */

if (WAOTV NE 'L')  and (WAOT NE 'Y')  then prob01520  = 1 ;
else prob01520  = 0 ;



/* Logic check variable prob01530 :
Check that WAPRI always meets the condition (WAPRI NE '00').  */

if (WAPRI NE '00')  then prob01530  = 0 ;
else prob01530  = 1 ;



/* Logic check variable prob01540 :
Check that if the first condition (WAPRI >= '01' AND WAPRI <= '14') is true then the second condition (WRKG = 'N') cannot also be true.  */

if (WAPRI >= '01' AND WAPRI <= '14')  and (WRKG = 'N')  then prob01540  = 1 ;
else prob01540  = 0 ;



/* Logic check variable prob01550 :
Check that if the first condition (WAPRI='L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (WAPRI='L')  and (WRKG = 'Y')  then prob01550  = 1 ;
else prob01550  = 0 ;



/* Logic check variable prob01560 :
Check that if the first condition (WAPRI = '01') is true then the second condition (WAACC NE 'Y') cannot also be true.  */

if (WAPRI = '01')  and (WAACC NE 'Y')  then prob01560  = 1 ;
else prob01560  = 0 ;



/* Logic check variable prob01570 :
Check that if the first condition (WAPRI = '02') is true then the second condition (WABRSH NE 'Y') cannot also be true.  */

if (WAPRI = '02')  and (WABRSH NE 'Y')  then prob01570  = 1 ;
else prob01570  = 0 ;



/* Logic check variable prob01580 :
Check that if the first condition (WAPRI = '03') is true then the second condition (WAAPRSH NE 'Y') cannot also be true.  */

if (WAPRI = '03')  and (WAAPRSH NE 'Y')  then prob01580  = 1 ;
else prob01580  = 0 ;



/* Logic check variable prob01590 :
Check that if the first condition (WAPRI = '04') is true then the second condition (WADEV NE 'Y') cannot also be true.  */

if (WAPRI = '04')  and (WADEV NE 'Y')  then prob01590  = 1 ;
else prob01590  = 0 ;



/* Logic check variable prob01600 :
Check that if the first condition (WAPRI = '05') is true then the second condition (WADSN NE 'Y') cannot also be true.  */

if (WAPRI = '05')  and (WADSN NE 'Y')  then prob01600  = 1 ;
else prob01600  = 0 ;



/* Logic check variable prob01610 :
Check that if the first condition (WAPRI = '06') is true then the second condition (WACOM NE 'Y') cannot also be true.  */

if (WAPRI = '06')  and (WACOM NE 'Y')  then prob01610  = 1 ;
else prob01610  = 0 ;



/* Logic check variable prob01620 :
Check that if the first condition (WAPRI = '07') is true then the second condition (WAEMRL NE 'Y') cannot also be true.  */

if (WAPRI = '07')  and (WAEMRL NE 'Y')  then prob01620  = 1 ;
else prob01620  = 0 ;



/* Logic check variable prob01630 :
Check that if the first condition (WAPRI = '08') is true then the second condition (WAMGMT NE 'Y') cannot also be true.  */

if (WAPRI = '08')  and (WAMGMT NE 'Y')  then prob01630  = 1 ;
else prob01630  = 0 ;



/* Logic check variable prob01640 :
Check that if the first condition (WAPRI = '09') is true then the second condition (WAPROD NE 'Y') cannot also be true.  */

if (WAPRI = '09')  and (WAPROD NE 'Y')  then prob01640  = 1 ;
else prob01640  = 0 ;



/* Logic check variable prob01650 :
Check that if the first condition (WAPRI = '10') is true then the second condition (WASVC NE 'Y') cannot also be true.  */

if (WAPRI = '10')  and (WASVC NE 'Y')  then prob01650  = 1 ;
else prob01650  = 0 ;



/* Logic check variable prob01660 :
Check that if the first condition (WAPRI = '11') is true then the second condition (WASALE NE 'Y') cannot also be true.  */

if (WAPRI = '11')  and (WASALE NE 'Y')  then prob01660  = 1 ;
else prob01660  = 0 ;



/* Logic check variable prob01670 :
Check that if the first condition (WAPRI = '12') is true then the second condition (WAQM NE 'Y') cannot also be true.  */

if (WAPRI = '12')  and (WAQM NE 'Y')  then prob01670  = 1 ;
else prob01670  = 0 ;



/* Logic check variable prob01680 :
Check that if the first condition (WAPRI = '13') is true then the second condition (WATEA NE 'Y') cannot also be true.  */

if (WAPRI = '13')  and (WATEA NE 'Y')  then prob01680  = 1 ;
else prob01680  = 0 ;



/* Logic check variable prob01690 :
Check that if the first condition (WAPRI = '14') is true then the second condition (WAOT NE 'Y') cannot also be true.  */

if (WAPRI = '14')  and (WAOT NE 'Y')  then prob01690  = 1 ;
else prob01690  = 0 ;



/* Logic check variable prob01700 :
Check that if the first condition (WASEC >= '00' AND WASEC <= '14') is true then the second condition (WRKG = 'N') cannot also be true.  */

if (WASEC >= '00' AND WASEC <= '14')  and (WRKG = 'N')  then prob01700  = 1 ;
else prob01700  = 0 ;



/* Logic check variable prob01710 :
Check that if the first condition (WASEC='L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (WASEC='L')  and (WRKG = 'Y')  then prob01710  = 1 ;
else prob01710  = 0 ;



/* Logic check variable prob01720 :
Check that if the first condition (WASEC = '01') is true then the second condition (WAACC NE 'Y') cannot also be true.  */

if (WASEC = '01')  and (WAACC NE 'Y')  then prob01720  = 1 ;
else prob01720  = 0 ;



/* Logic check variable prob01730 :
Check that if the first condition (WASEC = '02') is true then the second condition (WABRSH NE 'Y') cannot also be true.  */

if (WASEC = '02')  and (WABRSH NE 'Y')  then prob01730  = 1 ;
else prob01730  = 0 ;



/* Logic check variable prob01740 :
Check that if the first condition (WASEC = '03') is true then the second condition (WAAPRSH NE 'Y') cannot also be true.  */

if (WASEC = '03')  and (WAAPRSH NE 'Y')  then prob01740  = 1 ;
else prob01740  = 0 ;



/* Logic check variable prob01750 :
Check that if the first condition (WASEC = '04') is true then the second condition (WADEV NE 'Y') cannot also be true.  */

if (WASEC = '04')  and (WADEV NE 'Y')  then prob01750  = 1 ;
else prob01750  = 0 ;



/* Logic check variable prob01760 :
Check that if the first condition (WASEC = '05') is true then the second condition (WADSN NE 'Y') cannot also be true.  */

if (WASEC = '05')  and (WADSN NE 'Y')  then prob01760  = 1 ;
else prob01760  = 0 ;



/* Logic check variable prob01770 :
Check that if the first condition (WASEC = '06') is true then the second condition (WACOM NE 'Y') cannot also be true.  */

if (WASEC = '06')  and (WACOM NE 'Y')  then prob01770  = 1 ;
else prob01770  = 0 ;



/* Logic check variable prob01780 :
Check that if the first condition (WASEC = '07') is true then the second condition (WAEMRL NE 'Y') cannot also be true.  */

if (WASEC = '07')  and (WAEMRL NE 'Y')  then prob01780  = 1 ;
else prob01780  = 0 ;



/* Logic check variable prob01790 :
Check that if the first condition (WASEC = '08') is true then the second condition (WAMGMT NE 'Y') cannot also be true.  */

if (WASEC = '08')  and (WAMGMT NE 'Y')  then prob01790  = 1 ;
else prob01790  = 0 ;



/* Logic check variable prob01800 :
Check that if the first condition (WASEC = '09') is true then the second condition (WAPROD NE 'Y') cannot also be true.  */

if (WASEC = '09')  and (WAPROD NE 'Y')  then prob01800  = 1 ;
else prob01800  = 0 ;



/* Logic check variable prob01810 :
Check that if the first condition (WASEC = '10') is true then the second condition (WASVC NE 'Y') cannot also be true.  */

if (WASEC = '10')  and (WASVC NE 'Y')  then prob01810  = 1 ;
else prob01810  = 0 ;



/* Logic check variable prob01820 :
Check that if the first condition (WASEC = '11') is true then the second condition (WASALE NE 'Y') cannot also be true.  */

if (WASEC = '11')  and (WASALE NE 'Y')  then prob01820  = 1 ;
else prob01820  = 0 ;



/* Logic check variable prob01830 :
Check that if the first condition (WASEC = '12') is true then the second condition (WAQM NE 'Y') cannot also be true.  */

if (WASEC = '12')  and (WAQM NE 'Y')  then prob01830  = 1 ;
else prob01830  = 0 ;



/* Logic check variable prob01840 :
Check that if the first condition (WASEC = '13') is true then the second condition (WATEA NE 'Y') cannot also be true.  */

if (WASEC = '13')  and (WATEA NE 'Y')  then prob01840  = 1 ;
else prob01840  = 0 ;



/* Logic check variable prob01850 :
Check that if the first condition (WASEC = '14') is true then the second condition (WAOT NE 'Y') cannot also be true.  */

if (WASEC = '14')  and (WAOT NE 'Y')  then prob01850  = 1 ;
else prob01850  = 0 ;



/* Logic check variable prob01860 :
Check that if the first condition (SUPWK='Y') is true then the second condition (WRKG = 'N') cannot also be true.  */

if (SUPWK='Y')  and (WRKG = 'N')  then prob01860  = 1 ;
else prob01860  = 0 ;



/* Logic check variable prob01870 :
Check that if the first condition (SUPWK='N') is true then the second condition (WRKG = 'N') cannot also be true.  */

if (SUPWK='N')  and (WRKG = 'N')  then prob01870  = 1 ;
else prob01870  = 0 ;



/* Logic check variable prob01880 :
Check that if the first condition (SUPWK='L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (SUPWK='L')  and (WRKG = 'Y')  then prob01880  = 1 ;
else prob01880  = 0 ;



/* Logic check variable prob01890 :
Check that if the first condition (0<=SUPDIR<=996) is true then the second condition (SUPWK IN ('N','L')) cannot also be true.  */

if (0<=SUPDIR<=996)  and (SUPWK IN ('N','L'))  then prob01890  = 1 ;
else prob01890  = 0 ;



/* Logic check variable prob01900 :
Check that if the first condition (SUPDIR=998) is true then the second condition (SUPWK = 'Y') cannot also be true.  */

if (SUPDIR=998)  and (SUPWK = 'Y')  then prob01900  = 1 ;
else prob01900  = 0 ;



/* Logic check variable prob01910 :
Check that if the first condition (0<=SUPIND<=9996) is true then the second condition (SUPWK IN ('N','L')) cannot also be true.  */

if (0<=SUPIND<=9996)  and (SUPWK IN ('N','L'))  then prob01910  = 1 ;
else prob01910  = 0 ;



/* Logic check variable prob01920 :
Check that if the first condition (SUPIND=9998) is true then the second condition (SUPWK = 'Y') cannot also be true.  */

if (SUPIND=9998)  and (SUPWK = 'Y')  then prob01920  = 1 ;
else prob01920  = 0 ;



/* Logic check variable prob01930 :
Check that if the first condition (JOBSATIS IN ('1','2','3','4')) is true then the second condition (WRKG = 'N') cannot also be true.  */

if (JOBSATIS IN ('1','2','3','4'))  and (WRKG = 'N')  then prob01930  = 1 ;
else prob01930  = 0 ;



/* Logic check variable prob01940 :
Check that if the first condition (JOBSATIS = 'L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (JOBSATIS = 'L')  and (WRKG = 'Y')  then prob01940  = 1 ;
else prob01940  = 0 ;



/* Logic check variable prob01950 :
Check that if the first condition (0<=SALARY<=9999996) is true then the second condition (WRKG = 'N') cannot also be true.  */

if (0<=SALARY<=9999996)  and (WRKG = 'N')  then prob01950  = 1 ;
else prob01950  = 0 ;



/* Logic check variable prob01960 :
Check that if the first condition (SALARY=9999998) is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (SALARY=9999998)  and (WRKG = 'Y')  then prob01960  = 1 ;
else prob01960  = 0 ;



/* Logic check variable prob01970 :
Check that if the first condition (WKSYR  IN ('1','2')) is true then the second condition (WRKG = 'N') cannot also be true.  */

if (WKSYR  IN ('1','2'))  and (WRKG = 'N')  then prob01970  = 1 ;
else prob01970  = 0 ;



/* Logic check variable prob01980 :
Check that if the first condition (WKSYR='L') is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (WKSYR='L')  and (WRKG = 'Y')  then prob01980  = 1 ;
else prob01980  = 0 ;



/* Logic check variable prob01990 :
Check that if the first condition (1<=WKSLYR<=51) is true then the second condition (WKSYR NE '2') cannot also be true.  */

if (1<=WKSLYR<=51)  and (WKSYR NE '2')  then prob01990  = 1 ;
else prob01990  = 0 ;



/* Logic check variable prob02000 :
Check that if the first condition (WKSLYR=98) is true then the second condition (WRKG = 'Y' AND WKSYR NE '1') cannot also be true.  */

if (WKSLYR=98)  and (WRKG = 'Y' AND WKSYR NE '1')  then prob02000  = 1 ;
else prob02000  = 0 ;



/* Logic check variable prob02010 :
Check that if the first condition (1<=HRSWK<=96) is true then the second condition (WRKG = 'N') cannot also be true.  */

if (1<=HRSWK<=96)  and (WRKG = 'N')  then prob02010  = 1 ;
else prob02010  = 0 ;



/* Logic check variable prob02020 :
Check that if the first condition (HRSWK=98) is true then the second condition (WRKG = 'Y') cannot also be true.  */

if (HRSWK=98)  and (WRKG = 'Y')  then prob02020  = 1 ;
else prob02020  = 0 ;



/* Logic check variable prob02030 :
Check that if the first condition (HRSWK>96) is true then the second condition (HRSWK NOT IN (97,98)) cannot also be true.  */

if (HRSWK>96)  and (HRSWK NOT IN (97,98))  then prob02030  = 1 ;
else prob02030  = 0 ;



/* Logic check variable prob02040 :
Check that if the first condition (PJWTFT IN ('Y','N')) is true then the second condition (WRKG = 'N' OR HRSWK>=35) cannot also be true.  */

if (PJWTFT IN ('Y','N'))  and (WRKG = 'N' OR HRSWK>=35)  then prob02040  = 1 ;
else prob02040  = 0 ;



/* Logic check variable prob02050 :
Check that if the first condition (PJWTFT='L') is true then the second condition (WRKG = 'Y' AND HRSWK<35) cannot also be true.  */

if (PJWTFT='L')  and (WRKG = 'Y' AND HRSWK<35)  then prob02050  = 1 ;
else prob02050  = 0 ;



/* Logic check variable prob02060 :
Check that if the first condition (PJRET IN ('Y','N')) is true then the second condition (WRKG = 'N' OR HRSWK>=35) cannot also be true.  */

if (PJRET IN ('Y','N'))  and (WRKG = 'N' OR HRSWK>=35)  then prob02060  = 1 ;
else prob02060  = 0 ;



/* Logic check variable prob02070 :
Check that if the first condition (PJRET='L') is true then the second condition (WRKG = 'Y' AND HRSWK<35) cannot also be true.  */

if (PJRET='L')  and (WRKG = 'Y' AND HRSWK<35)  then prob02070  = 1 ;
else prob02070  = 0 ;



/* Logic check variable prob02080 :
Check that if the first condition ((1949<=PJRETYR<=2010) OR PJRETYR=9999) is true then the second condition (PJRET IN ('N','L')) cannot also be true.  */

if ((1949<=PJRETYR<=2010) OR PJRETYR=9999)  and (PJRET IN ('N','L'))  then prob02080  = 1 ;
else prob02080  = 0 ;



/* Logic check variable prob02090 :
Check that if the first condition (PJRETYR=9998) is true then the second condition (PJRET = 'Y') cannot also be true.  */

if (PJRETYR=9998)  and (PJRET = 'Y')  then prob02090  = 1 ;
else prob02090  = 0 ;



/* Logic check variable prob02100 :
Check that if the first condition (PJSTU IN ('Y','N')) is true then the second condition (WRKG = 'N' OR HRSWK>=35) cannot also be true.  */

if (PJSTU IN ('Y','N'))  and (WRKG = 'N' OR HRSWK>=35)  then prob02100  = 1 ;
else prob02100  = 0 ;



/* Logic check variable prob02110 :
Check that if the first condition (PJSTU='L') is true then the second condition (WRKG = 'Y' AND HRSWK<35) cannot also be true.  */

if (PJSTU='L')  and (WRKG = 'Y' AND HRSWK<35)  then prob02110  = 1 ;
else prob02110  = 0 ;



/* Logic check variable prob02120 :
Check that if the first condition (PJFAM IN ('Y','N')) is true then the second condition (WRKG = 'N' OR HRSWK>=35) cannot also be true.  */

if (PJFAM IN ('Y','N'))  and (WRKG = 'N' OR HRSWK>=35)  then prob02120  = 1 ;
else prob02120  = 0 ;



/* Logic check variable prob02130 :
Check that if the first condition (PJFAM='L') is true then the second condition (WRKG = 'Y' AND HRSWK<35) cannot also be true.  */

if (PJFAM='L')  and (WRKG = 'Y' AND HRSWK<35)  then prob02130  = 1 ;
else prob02130  = 0 ;



/* Logic check variable prob02160 :
Check that if the first condition (PJNOND IN ('Y','N')) is true then the second condition (WRKG = 'N' OR HRSWK>=35) cannot also be true.  */

if (PJNOND IN ('Y','N'))  and (WRKG = 'N' OR HRSWK>=35)  then prob02160  = 1 ;
else prob02160  = 0 ;



/* Logic check variable prob02170 :
Check that if the first condition (PJNOND='L') is true then the second condition (WRKG = 'Y' AND HRSWK<35) cannot also be true.  */

if (PJNOND='L')  and (WRKG = 'Y' AND HRSWK<35)  then prob02170  = 1 ;
else prob02170  = 0 ;



/* Logic check variable prob02180 :
Check that if the first condition (PJOT IN ('Y','N')) is true then the second condition (WRKG = 'N' OR HRSWK>=35) cannot also be true.  */

if (PJOT IN ('Y','N'))  and (WRKG = 'N' OR HRSWK>=35)  then prob02180  = 1 ;
else prob02180  = 0 ;



/* Logic check variable prob02190 :
Check that if the first condition (PJOT='L') is true then the second condition (WRKG = 'Y' AND HRSWK<35) cannot also be true.  */

if (PJOT='L')  and (WRKG = 'Y' AND HRSWK<35)  then prob02190  = 1 ;
else prob02190  = 0 ;



/* Logic check variable prob02200 :
Check that if the first condition (PJOTV NE 'L') is true then the second condition (PJOT NE 'Y') cannot also be true.  */

if (PJOTV NE 'L')  and (PJOT NE 'Y')  then prob02200  = 1 ;
else prob02200  = 0 ;



/* Logic check variable prob02210 :
Check that if the first condition (PJOTV='L') is true then the second condition (PJOT = 'Y') cannot also be true.  */

if (PJOTV='L')  and (PJOT = 'Y')  then prob02210  = 1 ;
else prob02210  = 0 ;



/* Logic check variable prob02320 :
Check that if the first condition (GOVSUP='0') is true then the second condition (LWNVR = 'Y') cannot also be true.  */

if (GOVSUP='0')  and (LWNVR = 'Y')  then prob02320  = 1 ;
else prob02320  = 0 ;



/* Logic check variable prob02330 :
Check that if the first condition (GOVSUP IN ('1','2','3')) is true then the second condition (LWNVR = 'Y') cannot also be true.  */

if (GOVSUP IN ('1','2','3'))  and (LWNVR = 'Y')  then prob02330  = 1 ;
else prob02330  = 0 ;



/* Logic check variable prob02340 :
Check that if the first condition (GOVSUP='L') is true then the second condition (LWNVR IN ('N','L')) cannot also be true.  */

if (GOVSUP='L')  and (LWNVR IN ('N','L'))  then prob02340  = 1 ;
else prob02340  = 0 ;



/* Logic check variable prob02350 :
Check that if the first condition (0<=EARN<=9999996) is true then the second condition (GOVSUP IN ('0','L')) cannot also be true.  */

if (0<=EARN<=9999996)  and (GOVSUP IN ('0','L'))  then prob02350  = 1 ;
else prob02350  = 0 ;



/* Logic check variable prob02360 :
Check that if the first condition (EARN = 9999998) is true then the second condition (GOVSUP IN ('1','2','3')) cannot also be true.  */

if (EARN = 9999998)  and (GOVSUP IN ('1','2','3'))  then prob02360  = 1 ;
else prob02360  = 0 ;



/* Logic check variable prob02370 :
Check that if the first condition (WRKGP IN ('Y','N')) is true then the second condition (LWNVR = 'Y') cannot also be true.  */

if (WRKGP IN ('Y','N'))  and (LWNVR = 'Y')  then prob02370  = 1 ;
else prob02370  = 0 ;



/* Logic check variable prob02380 :
Check that if the first condition (WRKGP='L') is true then the second condition (LWNVR IN ('N','L')) cannot also be true.  */

if (WRKGP='L')  and (LWNVR IN ('N','L'))  then prob02380  = 1 ;
else prob02380  = 0 ;



/* Logic check variable prob02390 :
Check that if the first condition (EMSMI IN ('1','2','3','4')) is true then the second condition (WRKGP IN ('N','L')) cannot also be true.  */

if (EMSMI IN ('1','2','3','4'))  and (WRKGP IN ('N','L'))  then prob02390  = 1 ;
else prob02390  = 0 ;



/* Logic check variable prob02400 :
Check that if the first condition (EMSMI='L') is true then the second condition (WRKGP = 'Y') cannot also be true.  */

if (EMSMI='L')  and (WRKGP = 'Y')  then prob02400  = 1 ;
else prob02400  = 0 ;



/* Logic check variable prob02410 :
Check that if the first condition (CHPAY IN ('Y','N')) is true then the second condition (EMSMI IN ('1','L')) cannot also be true.  */

if (CHPAY IN ('Y','N'))  and (EMSMI IN ('1','L'))  then prob02410  = 1 ;
else prob02410  = 0 ;



/* Logic check variable prob02420 :
Check that if the first condition (CHPAY='L') is true then the second condition (EMSMI IN ('2','3','4')) cannot also be true.  */

if (CHPAY='L')  and (EMSMI IN ('2','3','4'))  then prob02420  = 1 ;
else prob02420  = 0 ;



/* Logic check variable prob02430 :
Check that if the first condition (CHCON IN ('Y','N')) is true then the second condition (EMSMI IN ('1','L')) cannot also be true.  */

if (CHCON IN ('Y','N'))  and (EMSMI IN ('1','L'))  then prob02430  = 1 ;
else prob02430  = 0 ;



/* Logic check variable prob02440 :
Check that if the first condition (CHCON='L') is true then the second condition (EMSMI IN ('2','3','4')) cannot also be true.  */

if (CHCON='L')  and (EMSMI IN ('2','3','4'))  then prob02440  = 1 ;
else prob02440  = 0 ;



/* Logic check variable prob02450 :
Check that if the first condition (CHLOC IN ('Y','N')) is true then the second condition (EMSMI IN ('1','L')) cannot also be true.  */

if (CHLOC IN ('Y','N'))  and (EMSMI IN ('1','L'))  then prob02450  = 1 ;
else prob02450  = 0 ;



/* Logic check variable prob02460 :
Check that if the first condition (CHLOC='L') is true then the second condition (EMSMI IN ('2','3','4')) cannot also be true.  */

if (CHLOC='L')  and (EMSMI IN ('2','3','4'))  then prob02460  = 1 ;
else prob02460  = 0 ;



/* Logic check variable prob02470 :
Check that if the first condition (CHCHG IN ('Y','N')) is true then the second condition (EMSMI IN ('1','L')) cannot also be true.  */

if (CHCHG IN ('Y','N'))  and (EMSMI IN ('1','L'))  then prob02470  = 1 ;
else prob02470  = 0 ;



/* Logic check variable prob02480 :
Check that if the first condition (CHCHG='L') is true then the second condition (EMSMI IN ('2','3','4')) cannot also be true.  */

if (CHCHG='L')  and (EMSMI IN ('2','3','4'))  then prob02480  = 1 ;
else prob02480  = 0 ;



/* Logic check variable prob02490 :
Check that if the first condition (CHFAM IN ('Y','N')) is true then the second condition (EMSMI IN ('1','L')) cannot also be true.  */

if (CHFAM IN ('Y','N'))  and (EMSMI IN ('1','L'))  then prob02490  = 1 ;
else prob02490  = 0 ;



/* Logic check variable prob02500 :
Check that if the first condition (CHFAM='L') is true then the second condition (EMSMI IN ('2','3','4')) cannot also be true.  */

if (CHFAM='L')  and (EMSMI IN ('2','3','4'))  then prob02500  = 1 ;
else prob02500  = 0 ;



/* Logic check variable prob02510 :
Check that if the first condition (CHSCH IN ('Y','N')) is true then the second condition (EMSMI IN ('1','L')) cannot also be true.  */

if (CHSCH IN ('Y','N'))  and (EMSMI IN ('1','L'))  then prob02510  = 1 ;
else prob02510  = 0 ;



/* Logic check variable prob02520 :
Check that if the first condition (CHSCH='L') is true then the second condition (EMSMI IN ('2','3','4')) cannot also be true.  */

if (CHSCH='L')  and (EMSMI IN ('2','3','4'))  then prob02520  = 1 ;
else prob02520  = 0 ;



/* Logic check variable prob02530 :
Check that if the first condition (CHLAY IN ('Y','N')) is true then the second condition (EMSMI IN ('1','L')) cannot also be true.  */

if (CHLAY IN ('Y','N'))  and (EMSMI IN ('1','L'))  then prob02530  = 1 ;
else prob02530  = 0 ;



/* Logic check variable prob02540 :
Check that if the first condition (CHLAY='L') is true then the second condition (EMSMI IN ('2','3','4')) cannot also be true.  */

if (CHLAY='L')  and (EMSMI IN ('2','3','4'))  then prob02540  = 1 ;
else prob02540  = 0 ;



/* Logic check variable prob02550 :
Check that if the first condition (CHRET IN ('Y','N')) is true then the second condition (EMSMI IN ('1','L')) cannot also be true.  */

if (CHRET IN ('Y','N'))  and (EMSMI IN ('1','L'))  then prob02550  = 1 ;
else prob02550  = 0 ;



/* Logic check variable prob02560 :
Check that if the first condition (CHRET='L') is true then the second condition (EMSMI IN ('2','3','4')) cannot also be true.  */

if (CHRET='L')  and (EMSMI IN ('2','3','4'))  then prob02560  = 1 ;
else prob02560  = 0 ;



/* Logic check variable prob02570 :
Check that if the first condition (CHOT IN ('Y','N')) is true then the second condition (EMSMI IN ('1','L')) cannot also be true.  */

if (CHOT IN ('Y','N'))  and (EMSMI IN ('1','L'))  then prob02570  = 1 ;
else prob02570  = 0 ;



/* Logic check variable prob02580 :
Check that if the first condition (CHOT='L') is true then the second condition (EMSMI IN ('2','3','4')) cannot also be true.  */

if (CHOT='L')  and (EMSMI IN ('2','3','4'))  then prob02580  = 1 ;
else prob02580  = 0 ;



/* Logic check variable prob02590 :
Check that if the first condition (CHOTV NE 'L') is true then the second condition (CHOT NE 'Y') cannot also be true.  */

if (CHOTV NE 'L')  and (CHOT NE 'Y')  then prob02590  = 1 ;
else prob02590  = 0 ;



/* Logic check variable prob02600 :
Check that if the first condition (CHOTV='L') is true then the second condition (CHOT = 'Y') cannot also be true.  */

if (CHOTV='L')  and (CHOT = 'Y')  then prob02600  = 1 ;
else prob02600  = 0 ;



/* Logic check variable prob02610 :
Check that if the first condition (WKTRNI IN ('Y','N')) is true then the second condition (LWNVR NOT IN ('N','L')) cannot also be true.  */

if (WKTRNI IN ('Y','N'))  and (LWNVR NOT IN ('N','L'))  then prob02610  = 1 ;
else prob02610  = 0 ;



/* Logic check variable prob02620 :
Check that if the first condition (WKTRNI='L') is true then the second condition (LWNVR NE 'Y') cannot also be true.  */

if (WKTRNI='L')  and (LWNVR NE 'Y')  then prob02620  = 1 ;
else prob02620  = 0 ;



/* Logic check variable prob02630 :
Check that if the first condition (WTRSKL IN ('Y','N')) is true then the second condition (WKTRNI IN ('N','L')) cannot also be true.  */

if (WTRSKL IN ('Y','N'))  and (WKTRNI IN ('N','L'))  then prob02630  = 1 ;
else prob02630  = 0 ;



/* Logic check variable prob02640 :
Check that if the first condition (WTRSKL='L') is true then the second condition (WKTRNI='Y') cannot also be true.  */

if (WTRSKL='L')  and (WKTRNI='Y')  then prob02640  = 1 ;
else prob02640  = 0 ;



/* Logic check variable prob02650 :
Check that if the first condition (WTROPPS IN ('Y','N')) is true then the second condition (WKTRNI IN ('N','L')) cannot also be true.  */

if (WTROPPS IN ('Y','N'))  and (WKTRNI IN ('N','L'))  then prob02650  = 1 ;
else prob02650  = 0 ;



/* Logic check variable prob02660 :
Check that if the first condition (WTROPPS='L') is true then the second condition (WKTRNI='Y') cannot also be true.  */

if (WTROPPS='L')  and (WKTRNI='Y')  then prob02660  = 1 ;
else prob02660  = 0 ;



/* Logic check variable prob02670 :
Check that if the first condition (WTRLIC IN ('Y','N')) is true then the second condition (WKTRNI IN ('N','L')) cannot also be true.  */

if (WTRLIC IN ('Y','N'))  and (WKTRNI IN ('N','L'))  then prob02670  = 1 ;
else prob02670  = 0 ;



/* Logic check variable prob02680 :
Check that if the first condition (WTRLIC='L') is true then the second condition (WKTRNI='Y') cannot also be true.  */

if (WTRLIC='L')  and (WKTRNI='Y')  then prob02680  = 1 ;
else prob02680  = 0 ;



/* Logic check variable prob02690 :
Check that if the first condition (WTRCHOC IN ('Y','N')) is true then the second condition (WKTRNI IN ('N','L')) cannot also be true.  */

if (WTRCHOC IN ('Y','N'))  and (WKTRNI IN ('N','L'))  then prob02690  = 1 ;
else prob02690  = 0 ;



/* Logic check variable prob02700 :
Check that if the first condition (WTRCHOC='L') is true then the second condition (WKTRNI='Y') cannot also be true.  */

if (WTRCHOC='L')  and (WKTRNI='Y')  then prob02700  = 1 ;
else prob02700  = 0 ;



/* Logic check variable prob02710 :
Check that if the first condition (WTREM IN ('Y','N')) is true then the second condition (WKTRNI IN ('N','L')) cannot also be true.  */

if (WTREM IN ('Y','N'))  and (WKTRNI IN ('N','L'))  then prob02710  = 1 ;
else prob02710  = 0 ;



/* Logic check variable prob02720 :
Check that if the first condition (WTREM='L') is true then the second condition (WKTRNI='Y') cannot also be true.  */

if (WTREM='L')  and (WKTRNI='Y')  then prob02720  = 1 ;
else prob02720  = 0 ;



/* Logic check variable prob02730 :
Check that if the first condition (WTRPERS IN ('Y','N')) is true then the second condition (WKTRNI IN ('N','L')) cannot also be true.  */

if (WTRPERS IN ('Y','N'))  and (WKTRNI IN ('N','L'))  then prob02730  = 1 ;
else prob02730  = 0 ;



/* Logic check variable prob02740 :
Check that if the first condition (WTRPERS='L') is true then the second condition (WKTRNI='Y') cannot also be true.  */

if (WTRPERS='L')  and (WKTRNI='Y')  then prob02740  = 1 ;
else prob02740  = 0 ;



/* Logic check variable prob02750 :
Check that if the first condition (WTROT IN ('Y','N')) is true then the second condition (WKTRNI IN ('N','L')) cannot also be true.  */

if (WTROT IN ('Y','N'))  and (WKTRNI IN ('N','L'))  then prob02750  = 1 ;
else prob02750  = 0 ;



/* Logic check variable prob02760 :
Check that if the first condition (WTROT='L') is true then the second condition (WKTRNI='Y') cannot also be true.  */

if (WTROT='L')  and (WKTRNI='Y')  then prob02760  = 1 ;
else prob02760  = 0 ;



/* Logic check variable prob02770 :
Check that if the first condition (WTROTV NE 'L') is true then the second condition (WTROT NE 'Y') cannot also be true.  */

if (WTROTV NE 'L')  and (WTROT NE 'Y')  then prob02770  = 1 ;
else prob02770  = 0 ;



/* Logic check variable prob02780 :
Check that if the first condition (WTROTV='L') is true then the second condition (WTROT = 'Y') cannot also be true.  */

if (WTROTV='L')  and (WTROT = 'Y')  then prob02780  = 1 ;
else prob02780  = 0 ;



/* Logic check variable prob02790 :
Check that if the first condition (WTREASN IN ('1','2','3','4','5','6','7')) is true then the second condition (WKTRNI IN ('N','L')) cannot also be true.  */

if (WTREASN IN ('1','2','3','4','5','6','7'))  and (WKTRNI IN ('N','L'))  then prob02790  = 1 ;
else prob02790  = 0 ;



/* Logic check variable prob02800 :
Check that if the first condition (WTREASN='L') is true then the second condition (WKTRNI = 'Y') cannot also be true.  */

if (WTREASN='L')  and (WKTRNI = 'Y')  then prob02800  = 1 ;
else prob02800  = 0 ;



/* Logic check variable prob02810 :
Check that WTREASN always meets the condition (WTREASN NE '0').  */

if (WTREASN NE '0')  then prob02810  = 0 ;
else prob02810  = 1 ;



/* Logic check variable prob02820 :
Check that if the first condition (WTREASN = '1') is true then the second condition (WTRSKL NE 'Y') cannot also be true.  */

if (WTREASN = '1')  and (WTRSKL NE 'Y')  then prob02820  = 1 ;
else prob02820  = 0 ;



/* Logic check variable prob02830 :
Check that if the first condition (WTREASN = '2') is true then the second condition (WTROPPS NE 'Y') cannot also be true.  */

if (WTREASN = '2')  and (WTROPPS NE 'Y')  then prob02830  = 1 ;
else prob02830  = 0 ;



/* Logic check variable prob02840 :
Check that if the first condition (WTREASN = '3') is true then the second condition (WTRLIC NE 'Y') cannot also be true.  */

if (WTREASN = '3')  and (WTRLIC NE 'Y')  then prob02840  = 1 ;
else prob02840  = 0 ;



/* Logic check variable prob02850 :
Check that if the first condition (WTREASN = '4') is true then the second condition (WTRCHOC NE 'Y') cannot also be true.  */

if (WTREASN = '4')  and (WTRCHOC NE 'Y')  then prob02850  = 1 ;
else prob02850  = 0 ;



/* Logic check variable prob02860 :
Check that if the first condition (WTREASN = '5') is true then the second condition (WTREM NE 'Y') cannot also be true.  */

if (WTREASN = '5')  and (WTREM NE 'Y')  then prob02860  = 1 ;
else prob02860  = 0 ;



/* Logic check variable prob02870 :
Check that if the first condition (WTREASN = '6') is true then the second condition (WTRPERS NE 'Y') cannot also be true.  */

if (WTREASN = '6')  and (WTRPERS NE 'Y')  then prob02870  = 1 ;
else prob02870  = 0 ;



/* Logic check variable prob02880 :
Check that if the first condition (WTREASN = '7') is true then the second condition (WTROT NE 'Y') cannot also be true.  */

if (WTREASN = '7')  and (WTROT NE 'Y')  then prob02880  = 1 ;
else prob02880  = 0 ;



/* Logic check variable prob02890 :
Check that TCDGCMP always meets the condition (TCDGCMP NE 'L').  */

if (TCDGCMP NE 'L')  then prob02890  = 0 ;
else prob02890  = 1 ;



/* Logic check variable prob02910 :
Check that if the first condition (MRDG IN ('1','2','3','4','5')) is true then the second condition (TCDGCMP = 'N') cannot also be true.  */

if (MRDG IN ('1','2','3','4','5'))  and (TCDGCMP = 'N')  then prob02910  = 1 ;
else prob02910  = 0 ;



/* Logic check variable prob02920 :
Check that if the first condition (MRDG='L') is true then the second condition (TCDGCMP = 'Y') cannot also be true.  */

if (MRDG='L')  and (TCDGCMP = 'Y')  then prob02920  = 1 ;
else prob02920  = 0 ;



/* Logic check variable prob03030 :
Check that if the first condition (MRYR = 9998) is true then the second condition (MRDG NE 'L') cannot also be true.  */

if (MRYR = 9998)  and (MRDG NE 'L')  then prob03030  = 1 ;
else prob03030  = 0 ;



/* Logic check variable prob03040 :
Check that MRYR always meets the condition (2008<=MRYR<=2010 or MRYR = 9998).  */

if (2008<=MRYR<=2010 or MRYR = 9998)  then prob03040  = 0 ;
else prob03040  = 1 ;



/* Logic check variable prob03050 :
Check that if the first condition (2008<=MRYR<=2010) is true then the second condition (MRDG = 'L') cannot also be true.  */

if (2008<=MRYR<=2010)  and (MRDG = 'L')  then prob03050  = 1 ;
else prob03050  = 0 ;



/* Logic check variable prob03060 :
Check that if the first condition (MRYR=2008) is true then the second condition (MRMN IN (1,2,3,4,5,6,7,8,9)) cannot also be true.  */

if (MRYR=2008)  and (MRMN IN (1,2,3,4,5,6,7,8,9))  then prob03060  = 1 ;
else prob03060  = 0 ;



/* Logic check variable prob03070 :
Check that if the first condition (MRYR=2010) is true then the second condition (MRMN IN (10,11,12,98)) cannot also be true.  */

if (MRYR=2010)  and (MRMN IN (10,11,12,98))  then prob03070  = 1 ;
else prob03070  = 0 ;



/* Logic check variable prob03080 :
Check that if the first condition (MROPV NE 'L') is true then the second condition (MRDG NE '4') cannot also be true.  */

if (MROPV NE 'L')  and (MRDG NE '4')  then prob03080  = 1 ;
else prob03080  = 0 ;



/* Logic check variable prob03090 :
Check that if the first condition (MROPV='L') is true then the second condition (MRDG = '4') cannot also be true.  */

if (MROPV='L')  and (MRDG = '4')  then prob03090  = 1 ;
else prob03090  = 0 ;



/* Logic check variable prob03100 :
Check that if the first condition (MROV NE 'L') is true then the second condition (MRDG NE '5') cannot also be true.  */

if (MROV NE 'L')  and (MRDG NE '5')  then prob03100  = 1 ;
else prob03100  = 0 ;



/* Logic check variable prob03110 :
Check that if the first condition (MROV='L') is true then the second condition (MRDG = '5') cannot also be true.  */

if (MROV='L')  and (MRDG = '5')  then prob03110  = 1 ;
else prob03110  = 0 ;



/* Logic check variable prob03120 :
Check that if the first condition (MRMAV NE 'L') is true then the second condition (MRDG = 'L') cannot also be true.  */

if (MRMAV NE 'L')  and (MRDG = 'L')  then prob03120  = 1 ;
else prob03120  = 0 ;



/* Logic check variable prob03130 :
Check that if the first condition (MRMAV='L') is true then the second condition (MRDG NE 'L') cannot also be true.  */

if (MRMAV='L')  and (MRDG NE 'L')  then prob03130  = 1 ;
else prob03130  = 0 ;



/* Logic check variable prob03140 :
Check that if the first condition (('601' <= NMRMED<='995')) is true then the second condition (MRDG = 'L') cannot also be true.  */

if (('601' <= NMRMED<='995'))  and (MRDG = 'L')  then prob03140  = 1 ;
else prob03140  = 0 ;



/* Logic check variable prob03150 :
Check that if the first condition (NMRMED='998') is true then the second condition (MRDG NE 'L') cannot also be true.  */

if (NMRMED='998')  and (MRDG NE 'L')  then prob03150  = 1 ;
else prob03150  = 0 ;



/* Logic check variable prob03170 :
Check that if the first condition (1<=MRMN<=12) is true then the second condition (MRDG = 'L') cannot also be true.  */

if (1<=MRMN<=12)  and (MRDG = 'L')  then prob03170  = 1 ;
else prob03170  = 0 ;



/* Logic check variable prob03180 :
Check that if the first condition (MRMN=98) is true then the second condition (MRDG NE 'L') cannot also be true.  */

if (MRMN=98)  and (MRDG NE 'L')  then prob03180  = 1 ;
else prob03180  = 0 ;



/* Logic check variable prob03200 :
Check that if the first condition (MRDGSCH='L') is true then the second condition (MRDG NE 'L') cannot also be true.  */

if (MRDGSCH='L')  and (MRDG NE 'L')  then prob03200  = 1 ;
else prob03200  = 0 ;



/* Logic check variable prob03210 :
Check that if the first condition (MRDGDEPT='L') is true then the second condition (MRDG NE 'L') cannot also be true.  */

if (MRDGDEPT='L')  and (MRDG NE 'L')  then prob03210  = 1 ;
else prob03210  = 0 ;



/* Logic check variable prob03220 :
Check that if the first condition (MRDGCTY='L') is true then the second condition (MRDG NE 'L') cannot also be true.  */

if (MRDGCTY='L')  and (MRDG NE 'L')  then prob03220  = 1 ;
else prob03220  = 0 ;



/* Logic check variable prob03230 :
Check that if the first condition (MRDGST='L') is true then the second condition (MRDG NE 'L') cannot also be true.  */

if (MRDGST='L')  and (MRDG NE 'L')  then prob03230  = 1 ;
else prob03230  = 0 ;



/* Logic check variable prob03240 :
Check that if the first condition (MRINCD >= '000000' AND MRINCD <= '999996') is true then the second condition (MRDG = 'L') cannot also be true.  */

if (MRINCD >= '000000' AND MRINCD <= '999996')  and (MRDG = 'L')  then prob03240  = 1 ;
else prob03240  = 0 ;



/* Logic check variable prob03250 :
Check that if the first condition (MRINCD = '999998') is true then the second condition (MRDG NE 'L') cannot also be true.  */

if (MRINCD = '999998')  and (MRDG NE 'L')  then prob03250  = 1 ;
else prob03250  = 0 ;



/* Logic check variable prob03260 :
Check that if the first condition (MRPBPR IN ('1','2')) is true then the second condition (MRDG = 'L' OR MRINCD = '777777') cannot also be true.  */

if (MRPBPR IN ('1','2'))  and (MRDG = 'L' OR MRINCD = '777777')  then prob03260  = 1 ;
else prob03260  = 0 ;



/* Logic check variable prob03270 :
Check that if the first condition (MRPBPR='L') is true then the second condition (MRDG NE 'L' AND MRINCD NE '777777') cannot also be true.  */

if (MRPBPR='L')  and (MRDG NE 'L' AND MRINCD NE '777777')  then prob03270  = 1 ;
else prob03270  = 0 ;



/* Logic check variable prob03280 :
Check that if the first condition (MRPBP05C IN ('1','2')) is true then the second condition (MRDG = 'L' OR MRINCD = '777777') cannot also be true.  */

if (MRPBP05C IN ('1','2'))  and (MRDG = 'L' OR MRINCD = '777777')  then prob03280  = 1 ;
else prob03280  = 0 ;



/* Logic check variable prob03290 :
Check that if the first condition (MRPBP05C='L') is true then the second condition (MRDG NE 'L' AND MRINCD NE '777777') cannot also be true.  */

if (MRPBP05C='L')  and (MRDG NE 'L' AND MRINCD NE '777777')  then prob03290  = 1 ;
else prob03290  = 0 ;



/* Logic check variable prob03300 :
Check that if the first condition (MRCARN IN ('11','12','13','14','21','22','31','32','40','51','52','53','54','55','56','57','58','59','60','65')) is true then the second condition (MRDG = 'L' OR MRINCD = '777777') cannot also be true.  */

if (MRCARN IN ('11','12','13','14','21','22','31','32','40','51','52','53','54','55','56','57','58','59','60','65'))  and (MRDG = 'L' OR MRINCD = '777777')  then prob03300  = 1 ;
else prob03300  = 0 ;



/* Logic check variable prob03310 :
Check that if the first condition (MRCARN='L') is true then the second condition (MRDG NE 'L' AND MRINCD NE '777777') cannot also be true.  */

if (MRCARN='L')  and (MRDG NE 'L' AND MRINCD NE '777777')  then prob03310  = 1 ;
else prob03310  = 0 ;



/* Logic check variable prob03320 :
Check that if the first condition (MRCRN05C='L') is true then the second condition (MRDG NE 'L' AND MRINCD NE '777777') cannot also be true.  */

if (MRCRN05C='L')  and (MRDG NE 'L' AND MRINCD NE '777777')  then prob03320  = 1 ;
else prob03320  = 0 ;



/* Logic check variable prob03330 :
Check that if the first condition (MRCRN05C>='00' AND MRCRN05C<='33') is true then the second condition (MRDG = 'L' OR MRINCD = '777777') cannot also be true.  */

if (MRCRN05C>='00' AND MRCRN05C<='33')  and (MRDG = 'L' OR MRINCD = '777777')  then prob03330  = 1 ;
else prob03330  = 0 ;



/* Logic check variable prob03340 :
Check that if the first condition (MRST >= '001' AND MRST <= '500') is true then the second condition (MRDG = 'L') cannot also be true.  */

if (MRST >= '001' AND MRST <= '500')  and (MRDG = 'L')  then prob03340  = 1 ;
else prob03340  = 0 ;



/* Logic check variable prob03350 :
Check that if the first condition (MRST='998') is true then the second condition (MRDG NE 'L') cannot also be true.  */

if (MRST='998')  and (MRDG NE 'L')  then prob03350  = 1 ;
else prob03350  = 0 ;



/* Logic check variable prob03360 :
Check that if the first condition (MRCAR IN ('Y','N')) is true then the second condition (MRDG = 'L') cannot also be true.  */

if (MRCAR IN ('Y','N'))  and (MRDG = 'L')  then prob03360  = 1 ;
else prob03360  = 0 ;



/* Logic check variable prob03370 :
Check that if the first condition (MRCAR='L') is true then the second condition (MRDG NE 'L') cannot also be true.  */

if (MRCAR='L')  and (MRDG NE 'L')  then prob03370  = 1 ;
else prob03370  = 0 ;



/* Logic check variable prob03380 :
Check that if the first condition (MRGRD IN ('Y','N')) is true then the second condition (MRDG = 'L') cannot also be true.  */

if (MRGRD IN ('Y','N'))  and (MRDG = 'L')  then prob03380  = 1 ;
else prob03380  = 0 ;



/* Logic check variable prob03390 :
Check that if the first condition (MRGRD='L') is true then the second condition (MRDG NE 'L') cannot also be true.  */

if (MRGRD='L')  and (MRDG NE 'L')  then prob03390  = 1 ;
else prob03390  = 0 ;



/* Logic check variable prob03400 :
Check that if the first condition (MRCHG IN ('Y','N')) is true then the second condition (MRDG = 'L') cannot also be true.  */

if (MRCHG IN ('Y','N'))  and (MRDG = 'L')  then prob03400  = 1 ;
else prob03400  = 0 ;



/* Logic check variable prob03410 :
Check that if the first condition (MRCHG='L') is true then the second condition (MRDG NE 'L') cannot also be true.  */

if (MRCHG='L')  and (MRDG NE 'L')  then prob03410  = 1 ;
else prob03410  = 0 ;



/* Logic check variable prob03420 :
Check that if the first condition (MRSKL IN ('Y','N')) is true then the second condition (MRDG = 'L') cannot also be true.  */

if (MRSKL IN ('Y','N'))  and (MRDG = 'L')  then prob03420  = 1 ;
else prob03420  = 0 ;



/* Logic check variable prob03430 :
Check that if the first condition (MRSKL='L') is true then the second condition (MRDG NE 'L') cannot also be true.  */

if (MRSKL='L')  and (MRDG NE 'L')  then prob03430  = 1 ;
else prob03430  = 0 ;



/* Logic check variable prob03440 :
Check that if the first condition (MRLIC IN ('Y','N')) is true then the second condition (MRDG = 'L') cannot also be true.  */

if (MRLIC IN ('Y','N'))  and (MRDG = 'L')  then prob03440  = 1 ;
else prob03440  = 0 ;



/* Logic check variable prob03450 :
Check that if the first condition (MRLIC='L') is true then the second condition (MRDG NE 'L') cannot also be true.  */

if (MRLIC='L')  and (MRDG NE 'L')  then prob03450  = 1 ;
else prob03450  = 0 ;



/* Logic check variable prob03460 :
Check that if the first condition (MRADV IN ('Y','N')) is true then the second condition (MRDG = 'L') cannot also be true.  */

if (MRADV IN ('Y','N'))  and (MRDG = 'L')  then prob03460  = 1 ;
else prob03460  = 0 ;



/* Logic check variable prob03470 :
Check that if the first condition (MRADV='L') is true then the second condition (MRDG NE 'L') cannot also be true.  */

if (MRADV='L')  and (MRDG NE 'L')  then prob03470  = 1 ;
else prob03470  = 0 ;



/* Logic check variable prob03480 :
Check that if the first condition (MREM IN ('Y','N')) is true then the second condition (MRDG = 'L') cannot also be true.  */

if (MREM IN ('Y','N'))  and (MRDG = 'L')  then prob03480  = 1 ;
else prob03480  = 0 ;



/* Logic check variable prob03490 :
Check that if the first condition (MREM='L') is true then the second condition (MRDG NE 'L') cannot also be true.  */

if (MREM='L')  and (MRDG NE 'L')  then prob03490  = 1 ;
else prob03490  = 0 ;



/* Logic check variable prob03500 :
Check that if the first condition (MRINT IN ('Y','N')) is true then the second condition (MRDG = 'L') cannot also be true.  */

if (MRINT IN ('Y','N'))  and (MRDG = 'L')  then prob03500  = 1 ;
else prob03500  = 0 ;



/* Logic check variable prob03510 :
Check that if the first condition (MRINT='L') is true then the second condition (MRDG NE 'L') cannot also be true.  */

if (MRINT='L')  and (MRDG NE 'L')  then prob03510  = 1 ;
else prob03510  = 0 ;



/* Logic check variable prob03520 :
Check that if the first condition (MROT IN ('Y','N')) is true then the second condition (MRDG = 'L') cannot also be true.  */

if (MROT IN ('Y','N'))  and (MRDG = 'L')  then prob03520  = 1 ;
else prob03520  = 0 ;



/* Logic check variable prob03530 :
Check that if the first condition (MROT='L') is true then the second condition (MRDG NE 'L') cannot also be true.  */

if (MROT='L')  and (MRDG NE 'L')  then prob03530  = 1 ;
else prob03530  = 0 ;



/* Logic check variable prob03540 :
Check that if the first condition (MROTV='L') is true then the second condition (MROT = 'Y') cannot also be true.  */

if (MROTV='L')  and (MROT = 'Y')  then prob03540  = 1 ;
else prob03540  = 0 ;



/* Logic check variable prob04140 :
Check that MARSTA always meets the condition (MARSTA NE 'L').  */

if (MARSTA NE 'L')  then prob04140  = 0 ;
else prob04140  = 1 ;



/* Logic check variable prob04150 :
Check that if the first condition (SPOWK IN ('1','2','3')) is true then the second condition (MARSTA NOT IN ('1','2')) cannot also be true.  */

if (SPOWK IN ('1','2','3'))  and (MARSTA NOT IN ('1','2'))  then prob04150  = 1 ;
else prob04150  = 0 ;



/* Logic check variable prob04160 :
Check that if the first condition (SPOWK='L') is true then the second condition (MARSTA NOT IN ('3','4','5','6')) cannot also be true.  */

if (SPOWK='L')  and (MARSTA NOT IN ('3','4','5','6'))  then prob04160  = 1 ;
else prob04160  = 0 ;



/* Logic check variable prob04170 :
Check that if the first condition (SPNAT IN ('Y','N')) is true then the second condition (SPOWK NOT IN ('1','2')) cannot also be true.  */

if (SPNAT IN ('Y','N'))  and (SPOWK NOT IN ('1','2'))  then prob04170  = 1 ;
else prob04170  = 0 ;



/* Logic check variable prob04180 :
Check that if the first condition (SPNAT = 'L') is true then the second condition (SPOWK IN ('1','2')) cannot also be true.  */

if (SPNAT = 'L')  and (SPOWK IN ('1','2'))  then prob04180  = 1 ;
else prob04180  = 0 ;



/* Logic check variable prob04190 :
Check that if the first condition (SPSOC IN ('Y','N')) is true then the second condition (SPOWK NOT IN ('1','2')) cannot also be true.  */

if (SPSOC IN ('Y','N'))  and (SPOWK NOT IN ('1','2'))  then prob04190  = 1 ;
else prob04190  = 0 ;



/* Logic check variable prob04200 :
Check that if the first condition (SPSOC = 'L') is true then the second condition (SPOWK IN ('1','2')) cannot also be true.  */

if (SPSOC = 'L')  and (SPOWK IN ('1','2'))  then prob04200  = 1 ;
else prob04200  = 0 ;



/* Logic check variable prob04210 :
Check that if the first condition (SPOT IN ('Y','N')) is true then the second condition (SPOWK NOT IN ('1','2')) cannot also be true.  */

if (SPOT IN ('Y','N'))  and (SPOWK NOT IN ('1','2'))  then prob04210  = 1 ;
else prob04210  = 0 ;



/* Logic check variable prob04220 :
Check that if the first condition (SPOT = 'L') is true then the second condition (SPOWK IN ('1','2')) cannot also be true.  */

if (SPOT = 'L')  and (SPOWK IN ('1','2'))  then prob04220  = 1 ;
else prob04220  = 0 ;



/* Logic check variable prob04230 :
Check that if the first condition (SPOTV NE 'L') is true then the second condition (SPOT NE 'Y') cannot also be true.  */

if (SPOTV NE 'L')  and (SPOT NE 'Y')  then prob04230  = 1 ;
else prob04230  = 0 ;



/* Logic check variable prob04240 :
Check that if the first condition (SPOTV = 'L') is true then the second condition (SPOT = 'Y') cannot also be true.  */

if (SPOTV = 'L')  and (SPOT = 'Y')  then prob04240  = 1 ;
else prob04240  = 0 ;



/* Logic check variable prob04250 :
Check that CHLVIN always meets the condition (CHLVIN NE 'L').  */

if (CHLVIN NE 'L')  then prob04250  = 0 ;
else prob04250  = 1 ;



/* Logic check variable prob04260 :
Check that if the first condition (0 <= CHU2 <=9) is true then the second condition (CHLVIN = 'N') cannot also be true.  */

if (0 <= CHU2 <=9)  and (CHLVIN = 'N')  then prob04260  = 1 ;
else prob04260  = 0 ;



/* Logic check variable prob04270 :
Check that if the first condition (CHU2=98) is true then the second condition (CHLVIN = 'Y') cannot also be true.  */

if (CHU2=98)  and (CHLVIN = 'Y')  then prob04270  = 1 ;
else prob04270  = 0 ;



/* Logic check variable prob04280 :
Check that if the first condition (0 <= CH25 <=9) is true then the second condition (CHLVIN = 'N') cannot also be true.  */

if (0 <= CH25 <=9)  and (CHLVIN = 'N')  then prob04280  = 1 ;
else prob04280  = 0 ;



/* Logic check variable prob04290 :
Check that if the first condition (CH25=98) is true then the second condition (CHLVIN = 'Y') cannot also be true.  */

if (CH25=98)  and (CHLVIN = 'Y')  then prob04290  = 1 ;
else prob04290  = 0 ;



/* Logic check variable prob04300 :
Check that if the first condition (0 <= CH611 <=9) is true then the second condition (CHLVIN = 'N') cannot also be true.  */

if (0 <= CH611 <=9)  and (CHLVIN = 'N')  then prob04300  = 1 ;
else prob04300  = 0 ;



/* Logic check variable prob04310 :
Check that if the first condition (CH611=98) is true then the second condition (CHLVIN = 'Y') cannot also be true.  */

if (CH611=98)  and (CHLVIN = 'Y')  then prob04310  = 1 ;
else prob04310  = 0 ;



/* Logic check variable prob04320 :
Check that if the first condition (0 <= CH1218 <=9) is true then the second condition (CHLVIN = 'N') cannot also be true.  */

if (0 <= CH1218 <=9)  and (CHLVIN = 'N')  then prob04320  = 1 ;
else prob04320  = 0 ;



/* Logic check variable prob04330 :
Check that if the first condition (CH1218=98) is true then the second condition (CHLVIN = 'Y') cannot also be true.  */

if (CH1218=98)  and (CHLVIN = 'Y')  then prob04330  = 1 ;
else prob04330  = 0 ;



/* Logic check variable prob04340 :
Check that if the first condition (0 <= CH19 <=9) is true then the second condition (CHLVIN = 'N') cannot also be true.  */

if (0 <= CH19 <=9)  and (CHLVIN = 'N')  then prob04340  = 1 ;
else prob04340  = 0 ;



/* Logic check variable prob04350 :
Check that if the first condition (CH19=98) is true then the second condition (CHLVIN = 'Y') cannot also be true.  */

if (CH19=98)  and (CHLVIN = 'Y')  then prob04350  = 1 ;
else prob04350  = 0 ;



/* Logic check variable prob04360 :
Check that FNINUS always meets the condition (FNINUS NE 'L').  */

if (FNINUS NE 'L')  then prob04360  = 0 ;
else prob04360  = 1 ;



/* Logic check variable prob04370 :
Check that CTZUSIN always meets the condition (CTZUSIN NE 'L').  */

if (CTZUSIN NE 'L')  then prob04370  = 0 ;
else prob04370  = 1 ;



/* Logic check variable prob04380 :
Check that if the first condition (CTZUS IN ('1','2','3')) is true then the second condition (CTZUSIN = 'N') cannot also be true.  */

if (CTZUS IN ('1','2','3'))  and (CTZUSIN = 'N')  then prob04380  = 1 ;
else prob04380  = 0 ;



/* Logic check variable prob04390 :
Check that if the first condition (CTZUS = 'L') is true then the second condition (CTZUSIN = 'Y') cannot also be true.  */

if (CTZUS = 'L')  and (CTZUSIN = 'Y')  then prob04390  = 1 ;
else prob04390  = 0 ;



/* Logic check variable prob04400 :
Check that if the first condition (CTZFOR IN ('1','2')) is true then the second condition (CTZUSIN = 'Y') cannot also be true.  */

if (CTZFOR IN ('1','2'))  and (CTZUSIN = 'Y')  then prob04400  = 1 ;
else prob04400  = 0 ;



/* Logic check variable prob04410 :
Check that if the first condition (CTZFOR = 'L') is true then the second condition (CTZUSIN = 'N') cannot also be true.  */

if (CTZFOR = 'L')  and (CTZUSIN = 'N')  then prob04410  = 1 ;
else prob04410  = 0 ;



/* Logic check variable prob04420 :
Check that if the first condition (FNCV='L') is true then the second condition (CTZUSIN = 'N') cannot also be true.  */

if (FNCV='L')  and (CTZUSIN = 'N')  then prob04420  = 1 ;
else prob04420  = 0 ;



/* Logic check variable prob04430 :
Check that if the first condition (('100' <=FNCCD<='555')) is true then the second condition (CTZUSIN = 'Y') cannot also be true.  */

if (('100' <=FNCCD<='555'))  and (CTZUSIN = 'Y')  then prob04430  = 1 ;
else prob04430  = 0 ;



/* Logic check variable prob04440 :
Check that if the first condition (FNCCD='998') is true then the second condition (CTZUSIN = 'N') cannot also be true.  */

if (FNCCD='998')  and (CTZUSIN = 'N')  then prob04440  = 1 ;
else prob04440  = 0 ;



/* Logic check variable prob04540 :
Check that BIRDAY always meets the condition (BIRDAY NE 98).  */

if (BIRDAY NE 98)  then prob04540  = 0 ;
else prob04540  = 1 ;



/* Logic check variable prob04550 :
Check that BIRMN always meets the condition (BIRMN NE 98).  */

if (BIRMN NE 98)  then prob04550  = 0 ;
else prob04550  = 1 ;



/* Logic check variable prob04560 :
Check that BIRYR always meets the condition (BIRYR NE 9998).  */

if (BIRYR NE 9998)  then prob04560  = 0 ;
else prob04560  = 1 ;



/* Logic check variable prob04570 :
Check that if the first condition (BIRMN IN (1,3,5,7,8,10,12)) is true then the second condition (BIRDAY < 1 OR BIRDAY > 31) cannot also be true.  */

if (BIRMN IN (1,3,5,7,8,10,12))  and (BIRDAY < 1 OR BIRDAY > 31)  then prob04570  = 1 ;
else prob04570  = 0 ;



/* Logic check variable prob04580 :
Check that if the first condition (BIRMN IN (4,6,9,11)) is true then the second condition (BIRDAY < 1 OR BIRDAY > 30) cannot also be true.  */

if (BIRMN IN (4,6,9,11))  and (BIRDAY < 1 OR BIRDAY > 30)  then prob04580  = 1 ;
else prob04580  = 0 ;



/* Logic check variable prob04590 :
Check that if the first condition (BIRMN=2 AND BIRYR IN (1932,1936,1940,1944,1948,1952,1956,1960,1964,1968,1972,1976,1980,1984,1988,1992,1996,2000,2004,2008)) is true then the second condition (BIRDAY < 1 OR BIRDAY > 29) cannot also be true.  */

if (BIRMN=2 AND BIRYR IN (1932,1936,1940,1944,1948,1952,1956,1960,1964,1968,1972,1976,1980,1984,1988,1992,1996,2000,2004,2008))  and (BIRDAY < 1 OR BIRDAY > 29)  then prob04590  = 1 ;
else prob04590  = 0 ;



/* Logic check variable prob04600 :
Check that if the first condition (BIRMN=2 AND BIRYR NOT IN (1932,1936,1940,1944,1948,1952,1956,1960,1964,1968,1972,1976,1980,1984,1988,1992,1996,2000,2004,2008)) is true then the second condition (BIRDAY < 1 OR BIRDAY > 28) cannot also be true.  */

if (BIRMN=2 AND BIRYR NOT IN (1932,1936,1940,1944,1948,1952,1956,1960,1964,1968,1972,1976,1980,1984,1988,1992,1996,2000,2004,2008))  and (BIRDAY < 1 OR BIRDAY > 28)  then prob04600  = 1 ;
else prob04600  = 0 ;



/* Logic check variable prob04610 :
Check that BIRYR always meets the condition (1932<=BIRYR <=1991).  */

if (1932<=BIRYR <=1991)  then prob04610  = 0 ;
else prob04610  = 1 ;



/* Logic check variable prob04620 :
Check that DIFSEE always meets the condition (DIFSEE NE 'L').  */

if (DIFSEE NE 'L')  then prob04620  = 0 ;
else prob04620  = 1 ;



/* Logic check variable prob04630 :
Check that DIFHEAR always meets the condition (DIFHEAR NE 'L').  */

if (DIFHEAR NE 'L')  then prob04630  = 0 ;
else prob04630  = 1 ;



/* Logic check variable prob04640 :
Check that DIFWALK always meets the condition (DIFWALK NE 'L').  */

if (DIFWALK NE 'L')  then prob04640  = 0 ;
else prob04640  = 1 ;



/* Logic check variable prob04650 :
Check that DIFLIFT always meets the condition (DIFLIFT NE 'L').  */

if (DIFLIFT NE 'L')  then prob04650  = 0 ;
else prob04650  = 1 ;



/* Logic check variable prob04660 :
Check that DIFNO always meets the condition (DIFNO NE 'L').  */

if (DIFNO NE 'L')  then prob04660  = 0 ;
else prob04660  = 1 ;



/* Logic check variable prob04670 :
Check that if the first condition (DIFNO='Y') is true then the second condition (DIFSEE NE '1' OR DIFHEAR NE '1' OR DIFWALK NE '1' OR DIFLIFT NE '1' OR DIFCOGN NE '1') cannot also be true.  */

if (DIFNO='Y')  and (DIFSEE NE '1' OR DIFHEAR NE '1' OR DIFWALK NE '1' OR DIFLIFT NE '1' OR DIFCOGN NE '1')  then prob04670  = 1 ;
else prob04670  = 0 ;



/* Logic check variable prob04680 :
Check that if the first condition (DIFNO='N') is true then the second condition (DIFSEE='1' AND DIFHEAR='1' AND DIFWALK='1' AND DIFLIFT='1' AND DIFCOGN='1') cannot also be true.  */

if (DIFNO='N')  and (DIFSEE='1' AND DIFHEAR='1' AND DIFWALK='1' AND DIFLIFT='1' AND DIFCOGN='1')  then prob04680  = 1 ;
else prob04680  = 0 ;



/* Logic check variable prob04690 :
Check that if the first condition ((1<=DIFAGE<=75)) is true then the second condition (DIFNO = 'Y' OR DIFBIR = 'Y') cannot also be true.  */

if ((1<=DIFAGE<=75))  and (DIFNO = 'Y' OR DIFBIR = 'Y')  then prob04690  = 1 ;
else prob04690  = 0 ;



/* Logic check variable prob04700 :
Check that if the first condition (DIFAGE=98) is true then the second condition (DIFNO = 'N' AND DIFBIR = 'N') cannot also be true.  */

if (DIFAGE=98)  and (DIFNO = 'N' AND DIFBIR = 'N')  then prob04700  = 1 ;
else prob04700  = 0 ;



/* Logic check variable prob04710 :
Check that if the first condition (DIFBIR='Y') is true then the second condition (DIFNO = 'Y' OR DIFAGE NE 98) cannot also be true.  */

if (DIFBIR='Y')  and (DIFNO = 'Y' OR DIFAGE NE 98)  then prob04710  = 1 ;
else prob04710  = 0 ;



/* Logic check variable prob04720 :
Check that if the first condition (DIFBIR='N') is true then the second condition (DIFNO = 'Y' OR NOT (1<=DIFAGE<=75)) cannot also be true.  */

if (DIFBIR='N')  and (DIFNO = 'Y' OR NOT (1<=DIFAGE<=75))  then prob04720  = 1 ;
else prob04720  = 0 ;



/* Logic check variable prob04730 :
Check that if the first condition (DIFBIR='L') is true then the second condition (DIFNO = 'N') cannot also be true.  */

if (DIFBIR='L')  and (DIFNO = 'N')  then prob04730  = 1 ;
else prob04730  = 0 ;



/* Logic check variable prob04740 :
Check that RESPLO3 always meets the condition (RESPLO3 NE '998').  */

if (RESPLO3 NE '998')  then prob04740  = 0 ;
else prob04740  = 1 ;



/* Logic check variable prob04750 :
Check that SURID always meets the condition (SURID EQ '2').  */

if (SURID EQ '2')  then prob04750  = 0 ;
else prob04750  = 1 ;



/* Logic check variable prob04780 :
Check that VALIND always meets the condition (VALIND = 'Y').  */

if (VALIND = 'Y')  then prob04780  = 0 ;
else prob04780  = 1 ;



/* Logic check variable prob04810 :
Check that COHORT always meets the condition (COHORT EQ '2').  */

if (COHORT EQ '2')  then prob04810  = 0 ;
else prob04810  = 1 ;



/* Logic check variable prob04830 :
Check that REFYR always meets the condition (REFYR EQ 2010).  */

if (REFYR EQ 2010)  then prob04830  = 0 ;
else prob04830  = 1 ;



/* Logic check variable prob04840 :
Check that SRVMODE always meets the condition (SRVMODE IN ('1','2','3','4')).  */

if (SRVMODE IN ('1','2','3','4'))  then prob04840  = 0 ;
else prob04840  = 1 ;



/* Logic check variable prob04850 :
Check that SMODEP always meets the condition (SMODEP NE 'L').  */

if (SMODEP NE 'L')  then prob04850  = 0 ;
else prob04850  = 1 ;



/* Logic check variable prob04860 :
Check that if the first condition (ACADNA IN ('N','Y')) is true then the second condition (EMED IN ('N','L') OR EDTP = '1') cannot also be true.  */

if (ACADNA IN ('N','Y'))  and (EMED IN ('N','L') OR EDTP = '1')  then prob04860  = 1 ;
else prob04860  = 0 ;



/* Logic check variable prob04870 :
Check that if the first condition (ACADNA = 'L') is true then the second condition (EMED = 'Y' AND (EDTP ^='1')) cannot also be true.  */

if (ACADNA = 'L')  and (EMED = 'Y' AND (EDTP ^='1'))  then prob04870  = 1 ;
else prob04870  = 0 ;



/* Logic check variable prob04880 :
Check that if the first condition (ACADPRES IN ('N','Y')) is true then the second condition (EMED IN ('N','L') OR EDTP = '1') cannot also be true.  */

if (ACADPRES IN ('N','Y'))  and (EMED IN ('N','L') OR EDTP = '1')  then prob04880  = 1 ;
else prob04880  = 0 ;



/* Logic check variable prob04890 :
Check that if the first condition (ACADPRES = 'L') is true then the second condition (EMED = 'Y' AND (EDTP ^='1')) cannot also be true.  */

if (ACADPRES = 'L')  and (EMED = 'Y' AND (EDTP ^='1'))  then prob04890  = 1 ;
else prob04890  = 0 ;



/* Logic check variable prob04900 :
Check that if the first condition (ACADDEAN IN ('N','Y')) is true then the second condition (EMED IN ('N','L') OR EDTP = '1') cannot also be true.  */

if (ACADDEAN IN ('N','Y'))  and (EMED IN ('N','L') OR EDTP = '1')  then prob04900  = 1 ;
else prob04900  = 0 ;



/* Logic check variable prob04910 :
Check that if the first condition (ACADDEAN = 'L') is true then the second condition (EMED = 'Y' AND (EDTP ^='1')) cannot also be true.  */

if (ACADDEAN = 'L')  and (EMED = 'Y' AND (EDTP ^='1'))  then prob04910  = 1 ;
else prob04910  = 0 ;



/* Logic check variable prob04920 :
Check that if the first condition (ACADRCHF IN ('N','Y')) is true then the second condition (EMED IN ('N','L') OR EDTP = '1') cannot also be true.  */

if (ACADRCHF IN ('N','Y'))  and (EMED IN ('N','L') OR EDTP = '1')  then prob04920  = 1 ;
else prob04920  = 0 ;



/* Logic check variable prob04930 :
Check that if the first condition (ACADRCHF = 'L') is true then the second condition (EMED = 'Y' AND (EDTP ^='1')) cannot also be true.  */

if (ACADRCHF = 'L')  and (EMED = 'Y' AND (EDTP ^='1'))  then prob04930  = 1 ;
else prob04930  = 0 ;



/* Logic check variable prob04940 :
Check that if the first condition (ACADTCHF IN ('N','Y')) is true then the second condition (EMED IN ('N','L') OR EDTP = '1') cannot also be true.  */

if (ACADTCHF IN ('N','Y'))  and (EMED IN ('N','L') OR EDTP = '1')  then prob04940  = 1 ;
else prob04940  = 0 ;



/* Logic check variable prob04950 :
Check that if the first condition (ACADTCHF = 'L') is true then the second condition (EMED = 'Y' AND (EDTP ^='1')) cannot also be true.  */

if (ACADTCHF = 'L')  and (EMED = 'Y' AND (EDTP ^='1'))  then prob04950  = 1 ;
else prob04950  = 0 ;



/* Logic check variable prob04960 :
Check that if the first condition (ACADADJF IN ('N','Y')) is true then the second condition (EMED IN ('N','L') OR EDTP = '1') cannot also be true.  */

if (ACADADJF IN ('N','Y'))  and (EMED IN ('N','L') OR EDTP = '1')  then prob04960  = 1 ;
else prob04960  = 0 ;



/* Logic check variable prob04970 :
Check that if the first condition (ACADADJF = 'L') is true then the second condition (EMED = 'Y' AND (EDTP ^='1')) cannot also be true.  */

if (ACADADJF = 'L')  and (EMED = 'Y' AND (EDTP ^='1'))  then prob04970  = 1 ;
else prob04970  = 0 ;



/* Logic check variable prob04980 :
Check that if the first condition (ACADPDOC IN ('N','Y')) is true then the second condition (EMED IN ('N','L') OR EDTP = '1') cannot also be true.  */

if (ACADPDOC IN ('N','Y'))  and (EMED IN ('N','L') OR EDTP = '1')  then prob04980  = 1 ;
else prob04980  = 0 ;



/* Logic check variable prob04990 :
Check that if the first condition (ACADPDOC = 'L') is true then the second condition (EMED = 'Y' AND (EDTP ^='1')) cannot also be true.  */

if (ACADPDOC = 'L')  and (EMED = 'Y' AND (EDTP ^='1'))  then prob04990  = 1 ;
else prob04990  = 0 ;



/* Logic check variable prob05000 :
Check that if the first condition (ACADRA IN ('N','Y')) is true then the second condition (EMED IN ('N','L') OR EDTP = '1') cannot also be true.  */

if (ACADRA IN ('N','Y'))  and (EMED IN ('N','L') OR EDTP = '1')  then prob05000  = 1 ;
else prob05000  = 0 ;



/* Logic check variable prob05010 :
Check that if the first condition (ACADRA = 'L') is true then the second condition (EMED = 'Y' AND (EDTP ^='1')) cannot also be true.  */

if (ACADRA = 'L')  and (EMED = 'Y' AND (EDTP ^='1'))  then prob05010  = 1 ;
else prob05010  = 0 ;



/* Logic check variable prob05020 :
Check that if the first condition (ACADTA IN ('N','Y')) is true then the second condition (EMED IN ('N','L') OR EDTP = '1') cannot also be true.  */

if (ACADTA IN ('N','Y'))  and (EMED IN ('N','L') OR EDTP = '1')  then prob05020  = 1 ;
else prob05020  = 0 ;



/* Logic check variable prob05030 :
Check that if the first condition (ACADTA = 'L') is true then the second condition (EMED = 'Y' AND (EDTP ^='1')) cannot also be true.  */

if (ACADTA = 'L')  and (EMED = 'Y' AND (EDTP ^='1'))  then prob05030  = 1 ;
else prob05030  = 0 ;



/* Logic check variable prob05040 :
Check that if the first condition (ACADOTHR IN ('N','Y')) is true then the second condition (EMED IN ('N','L') OR EDTP = '1') cannot also be true.  */

if (ACADOTHR IN ('N','Y'))  and (EMED IN ('N','L') OR EDTP = '1')  then prob05040  = 1 ;
else prob05040  = 0 ;



/* Logic check variable prob05050 :
Check that if the first condition (ACADOTHR = 'L') is true then the second condition (EMED = 'Y' AND (EDTP ^='1')) cannot also be true.  */

if (ACADOTHR = 'L')  and (EMED = 'Y' AND (EDTP ^='1'))  then prob05050  = 1 ;
else prob05050  = 0 ;



/* Logic check variable prob05060 :
Check that if the first condition (ACADOTV='L') is true then the second condition (ACADOTHR = 'Y') cannot also be true.  */

if (ACADOTV='L')  and (ACADOTHR = 'Y')  then prob05060  = 1 ;
else prob05060  = 0 ;



/* Logic check variable prob05070 :
Check that if the first condition (FACRANK IN ('1','2','3','4','5','6','7','8')) is true then the second condition (EMED IN ('N','L') OR EDTP = '1') cannot also be true.  */

if (FACRANK IN ('1','2','3','4','5','6','7','8'))  and (EMED IN ('N','L') OR EDTP = '1')  then prob05070  = 1 ;
else prob05070  = 0 ;



/* Logic check variable prob05080 :
Check that if the first condition (FACRANK='L') is true then the second condition (EMED = 'Y' AND (EDTP ^='1')) cannot also be true.  */

if (FACRANK='L')  and (EMED = 'Y' AND (EDTP ^='1'))  then prob05080  = 1 ;
else prob05080  = 0 ;



/* Logic check variable prob05090 :
Check that if the first condition (FACC NE 'L') is true then the second condition (FACRANK NE '8') cannot also be true.  */

if (FACC NE 'L')  and (FACRANK NE '8')  then prob05090  = 1 ;
else prob05090  = 0 ;



/* Logic check variable prob05100 :
Check that if the first condition (FACC='L') is true then the second condition (FACRANK = '8') cannot also be true.  */

if (FACC='L')  and (FACRANK = '8')  then prob05100  = 1 ;
else prob05100  = 0 ;



/* Logic check variable prob05110 :
Check that if the first condition (TENSTA IN ('1','2','3','4','5')) is true then the second condition (EMED IN ('N','L') OR EDTP = '1') cannot also be true.  */

if (TENSTA IN ('1','2','3','4','5'))  and (EMED IN ('N','L') OR EDTP = '1')  then prob05110  = 1 ;
else prob05110  = 0 ;



/* Logic check variable prob05120 :
Check that if the first condition (TENSTA='L') is true then the second condition (EMED = 'Y' AND (EDTP ^='1')) cannot also be true.  */

if (TENSTA='L')  and (EMED = 'Y' AND (EDTP ^='1'))  then prob05120  = 1 ;
else prob05120  = 0 ;



/* Logic check variable prob05130 :
Check that if the first condition (PDIX IN ('Y','N')) is true then the second condition (WRKG='N') cannot also be true.  */

if (PDIX IN ('Y','N'))  and (WRKG='N')  then prob05130  = 1 ;
else prob05130  = 0 ;



/* Logic check variable prob05140 :
Check that if the first condition (PDIX = 'L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (PDIX = 'L')  and (WRKG='Y')  then prob05140  = 1 ;
else prob05140  = 0 ;



/* Logic check variable prob05150 :
Check that if the first condition (PDTRAIN IN ('Y','N')) is true then the second condition (PDIX IN ('N','L')) cannot also be true.  */

if (PDTRAIN IN ('Y','N'))  and (PDIX IN ('N','L'))  then prob05150  = 1 ;
else prob05150  = 0 ;



/* Logic check variable prob05160 :
Check that if the first condition (PDTRAIN = 'L') is true then the second condition (PDIX = 'Y') cannot also be true.  */

if (PDTRAIN = 'L')  and (PDIX = 'Y')  then prob05160  = 1 ;
else prob05160  = 0 ;



/* Logic check variable prob05170 :
Check that if the first condition (PDTROUT IN ('Y','N')) is true then the second condition (PDIX IN ('N','L')) cannot also be true.  */

if (PDTROUT IN ('Y','N'))  and (PDIX IN ('N','L'))  then prob05170  = 1 ;
else prob05170  = 0 ;



/* Logic check variable prob05180 :
Check that if the first condition (PDTROUT = 'L') is true then the second condition (PDIX = 'Y') cannot also be true.  */

if (PDTROUT = 'L')  and (PDIX = 'Y')  then prob05180  = 1 ;
else prob05180  = 0 ;



/* Logic check variable prob05190 :
Check that if the first condition (PDPERPL IN ('Y','N')) is true then the second condition (PDIX IN ('N','L')) cannot also be true.  */

if (PDPERPL IN ('Y','N'))  and (PDIX IN ('N','L'))  then prob05190  = 1 ;
else prob05190  = 0 ;



/* Logic check variable prob05200 :
Check that if the first condition (PDPERPL = 'L') is true then the second condition (PDIX = 'Y') cannot also be true.  */

if (PDPERPL = 'L')  and (PDIX = 'Y')  then prob05200  = 1 ;
else prob05200  = 0 ;



/* Logic check variable prob05210 :
Check that if the first condition (PDEMPL IN ('Y','N')) is true then the second condition (PDIX IN ('N','L')) cannot also be true.  */

if (PDEMPL IN ('Y','N'))  and (PDIX IN ('N','L'))  then prob05210  = 1 ;
else prob05210  = 0 ;



/* Logic check variable prob05220 :
Check that if the first condition (PDEMPL = 'L') is true then the second condition (PDIX = 'Y') cannot also be true.  */

if (PDEMPL = 'L')  and (PDIX = 'Y')  then prob05220  = 1 ;
else prob05220  = 0 ;



/* Logic check variable prob05230 :
Check that if the first condition (PDEXPECT IN ('Y','N')) is true then the second condition (PDIX IN ('N','L')) cannot also be true.  */

if (PDEXPECT IN ('Y','N'))  and (PDIX IN ('N','L'))  then prob05230  = 1 ;
else prob05230  = 0 ;



/* Logic check variable prob05240 :
Check that if the first condition (PDEXPECT = 'L') is true then the second condition (PDIX = 'Y') cannot also be true.  */

if (PDEXPECT = 'L')  and (PDIX = 'Y')  then prob05240  = 1 ;
else prob05240  = 0 ;



/* Logic check variable prob05250 :
Check that if the first condition (PDOTHER IN ('Y','N')) is true then the second condition (PDIX IN ('N','L')) cannot also be true.  */

if (PDOTHER IN ('Y','N'))  and (PDIX IN ('N','L'))  then prob05250  = 1 ;
else prob05250  = 0 ;



/* Logic check variable prob05260 :
Check that if the first condition (PDOTHER = 'L') is true then the second condition (PDIX = 'Y') cannot also be true.  */

if (PDOTHER = 'L')  and (PDIX = 'Y')  then prob05260  = 1 ;
else prob05260  = 0 ;



/* Logic check variable prob05270 :
Check that if the first condition (PDROTV NE 'L') is true then the second condition (PDOTHER NE 'Y') cannot also be true.  */

if (PDROTV NE 'L')  and (PDOTHER NE 'Y')  then prob05270  = 1 ;
else prob05270  = 0 ;



/* Logic check variable prob05280 :
Check that if the first condition (PDROTV = 'L') is true then the second condition (PDOTHER = 'Y') cannot also be true.  */

if (PDROTV = 'L')  and (PDOTHER = 'Y')  then prob05280  = 1 ;
else prob05280  = 0 ;



/* Logic check variable prob05290 :
Check that if the first condition (PDPRI IN ('1','2','3','4','5','6')) is true then the second condition (PDIX NE 'Y') cannot also be true.  */

if (PDPRI IN ('1','2','3','4','5','6'))  and (PDIX NE 'Y')  then prob05290  = 1 ;
else prob05290  = 0 ;



/* Logic check variable prob05300 :
Check that if the first condition (PDPRI = 'L') is true then the second condition (PDIX IN ('Y')) cannot also be true.  */

if (PDPRI = 'L')  and (PDIX IN ('Y'))  then prob05300  = 1 ;
else prob05300  = 0 ;



/* Logic check variable prob05310 :
Check that PDPRI always meets the condition (PDPRI NE '0').  */

if (PDPRI NE '0')  then prob05310  = 0 ;
else prob05310  = 1 ;



/* Logic check variable prob05320 :
Check that if the first condition (PDPRI = '1') is true then the second condition (PDTRAIN NE 'Y') cannot also be true.  */

if (PDPRI = '1')  and (PDTRAIN NE 'Y')  then prob05320  = 1 ;
else prob05320  = 0 ;



/* Logic check variable prob05330 :
Check that if the first condition (PDPRI = '2') is true then the second condition (PDTROUT NE 'Y') cannot also be true.  */

if (PDPRI = '2')  and (PDTROUT NE 'Y')  then prob05330  = 1 ;
else prob05330  = 0 ;



/* Logic check variable prob05340 :
Check that if the first condition (PDPRI = '3') is true then the second condition (PDPERPL NE 'Y') cannot also be true.  */

if (PDPRI = '3')  and (PDPERPL NE 'Y')  then prob05340  = 1 ;
else prob05340  = 0 ;



/* Logic check variable prob05350 :
Check that if the first condition (PDPRI = '4') is true then the second condition (PDEMPL NE 'Y') cannot also be true.  */

if (PDPRI = '4')  and (PDEMPL NE 'Y')  then prob05350  = 1 ;
else prob05350  = 0 ;



/* Logic check variable prob05360 :
Check that if the first condition (PDPRI = '5') is true then the second condition (PDEXPECT NE 'Y') cannot also be true.  */

if (PDPRI = '5')  and (PDEXPECT NE 'Y')  then prob05360  = 1 ;
else prob05360  = 0 ;



/* Logic check variable prob05370 :
Check that if the first condition (PDPRI = '6') is true then the second condition (PDOTHER NE 'Y') cannot also be true.  */

if (PDPRI = '6')  and (PDOTHER NE 'Y')  then prob05370  = 1 ;
else prob05370  = 0 ;



/* Logic check variable prob05380 :
Check that if the first condition (PDSEC IN ('0','1','2','3','4','5','6')) is true then the second condition (PDIX NE 'Y') cannot also be true.  */

if (PDSEC IN ('0','1','2','3','4','5','6'))  and (PDIX NE 'Y')  then prob05380  = 1 ;
else prob05380  = 0 ;



/* Logic check variable prob05390 :
Check that if the first condition (PDSEC = 'L') is true then the second condition (PDIX IN ('Y')) cannot also be true.  */

if (PDSEC = 'L')  and (PDIX IN ('Y'))  then prob05390  = 1 ;
else prob05390  = 0 ;



/* Logic check variable prob05400 :
Check that if the first condition (PDSEC = '1') is true then the second condition (PDTRAIN NE 'Y') cannot also be true.  */

if (PDSEC = '1')  and (PDTRAIN NE 'Y')  then prob05400  = 1 ;
else prob05400  = 0 ;



/* Logic check variable prob05410 :
Check that if the first condition (PDSEC = '2') is true then the second condition (PDTROUT NE 'Y') cannot also be true.  */

if (PDSEC = '2')  and (PDTROUT NE 'Y')  then prob05410  = 1 ;
else prob05410  = 0 ;



/* Logic check variable prob05420 :
Check that if the first condition (PDSEC = '3') is true then the second condition (PDPERPL NE 'Y') cannot also be true.  */

if (PDSEC = '3')  and (PDPERPL NE 'Y')  then prob05420  = 1 ;
else prob05420  = 0 ;



/* Logic check variable prob05430 :
Check that if the first condition (PDSEC = '4') is true then the second condition (PDEMPL NE 'Y') cannot also be true.  */

if (PDSEC = '4')  and (PDEMPL NE 'Y')  then prob05430  = 1 ;
else prob05430  = 0 ;



/* Logic check variable prob05440 :
Check that if the first condition (PDSEC = '5') is true then the second condition (PDEXPECT NE 'Y') cannot also be true.  */

if (PDSEC = '5')  and (PDEXPECT NE 'Y')  then prob05440  = 1 ;
else prob05440  = 0 ;



/* Logic check variable prob05450 :
Check that if the first condition (PDSEC = '6') is true then the second condition (PDOTHER NE 'Y') cannot also be true.  */

if (PDSEC = '6')  and (PDOTHER NE 'Y')  then prob05450  = 1 ;
else prob05450  = 0 ;



/* Logic check variable prob06570 :
Check that ACSIN always meets the condition (ACSIN NE 'L').  */

if (ACSIN NE 'L')  then prob06570  = 0 ;
else prob06570  = 1 ;



/* Logic check variable prob06580 :
Check that if the first condition (ACFPT IN ('1','2','3')) is true then the second condition (ACSIN = 'N') cannot also be true.  */

if (ACFPT IN ('1','2','3'))  and (ACSIN = 'N')  then prob06580  = 1 ;
else prob06580  = 0 ;



/* Logic check variable prob06590 :
Check that if the first condition (ACFPT = 'L') is true then the second condition (ACSIN = 'Y') cannot also be true.  */

if (ACFPT = 'L')  and (ACSIN = 'Y')  then prob06590  = 1 ;
else prob06590  = 0 ;



/* Logic check variable prob06600 :
Check that if the first condition (ACDRG IN ('0','1','2','3','4','5')) is true then the second condition (ACSIN = 'N') cannot also be true.  */

if (ACDRG IN ('0','1','2','3','4','5'))  and (ACSIN = 'N')  then prob06600  = 1 ;
else prob06600  = 0 ;



/* Logic check variable prob06610 :
Check that if the first condition (ACDRG='L') is true then the second condition (ACSIN = 'Y') cannot also be true.  */

if (ACDRG='L')  and (ACSIN = 'Y')  then prob06610  = 1 ;
else prob06610  = 0 ;



/* Logic check variable prob06620 :
Check that if the first condition (ACDGOPV='L') is true then the second condition (ACDRG = '4') cannot also be true.  */

if (ACDGOPV='L')  and (ACDRG = '4')  then prob06620  = 1 ;
else prob06620  = 0 ;



/* Logic check variable prob06630 :
Check that if the first condition (ACDGOV='L') is true then the second condition (ACDRG = '5') cannot also be true.  */

if (ACDGOV='L')  and (ACDRG = '5')  then prob06630  = 1 ;
else prob06630  = 0 ;



/* Logic check variable prob06640 :
Check that if the first condition (ACEDV='L') is true then the second condition (ACDRG NOT IN ('0','L')) cannot also be true.  */

if (ACEDV='L')  and (ACDRG NOT IN ('0','L'))  then prob06640  = 1 ;
else prob06640  = 0 ;



/* Logic check variable prob06650 :
Check that if the first condition (('010'<=NACED<='500') OR NACED='999') is true then the second condition (ACSIN NE 'Y' OR ACDRG = '0') cannot also be true.  */

if (('010'<=NACED<='500') OR NACED='999')  and (ACSIN NE 'Y' OR ACDRG = '0')  then prob06650  = 1 ;
else prob06650  = 0 ;



/* Logic check variable prob06660 :
Check that if the first condition (NACED='998') is true then the second condition (ACSIN NE 'N' AND ACDRG NE '0') cannot also be true.  */

if (NACED='998')  and (ACSIN NE 'N' AND ACDRG NE '0')  then prob06660  = 1 ;
else prob06660  = 0 ;



/* Logic check variable prob06670 :
Check that if the first condition (ACCAR IN ('Y','N')) is true then the second condition (ACSIN = 'N') cannot also be true.  */

if (ACCAR IN ('Y','N'))  and (ACSIN = 'N')  then prob06670  = 1 ;
else prob06670  = 0 ;



/* Logic check variable prob06680 :
Check that if the first condition (ACCAR='L') is true then the second condition (ACSIN = 'Y') cannot also be true.  */

if (ACCAR='L')  and (ACSIN = 'Y')  then prob06680  = 1 ;
else prob06680  = 0 ;



/* Logic check variable prob06690 :
Check that if the first condition (ACGRD IN ('Y','N')) is true then the second condition (ACSIN = 'N') cannot also be true.  */

if (ACGRD IN ('Y','N'))  and (ACSIN = 'N')  then prob06690  = 1 ;
else prob06690  = 0 ;



/* Logic check variable prob06700 :
Check that if the first condition (ACGRD='L') is true then the second condition (ACSIN = 'Y') cannot also be true.  */

if (ACGRD='L')  and (ACSIN = 'Y')  then prob06700  = 1 ;
else prob06700  = 0 ;



/* Logic check variable prob06710 :
Check that if the first condition (ACCHG IN ('Y','N')) is true then the second condition (ACSIN = 'N') cannot also be true.  */

if (ACCHG IN ('Y','N'))  and (ACSIN = 'N')  then prob06710  = 1 ;
else prob06710  = 0 ;



/* Logic check variable prob06720 :
Check that if the first condition (ACCHG='L') is true then the second condition (ACSIN = 'Y') cannot also be true.  */

if (ACCHG='L')  and (ACSIN = 'Y')  then prob06720  = 1 ;
else prob06720  = 0 ;



/* Logic check variable prob06730 :
Check that if the first condition (ACSKL IN ('Y','N')) is true then the second condition (ACSIN = 'N') cannot also be true.  */

if (ACSKL IN ('Y','N'))  and (ACSIN = 'N')  then prob06730  = 1 ;
else prob06730  = 0 ;



/* Logic check variable prob06740 :
Check that if the first condition (ACSKL='L') is true then the second condition (ACSIN = 'Y') cannot also be true.  */

if (ACSKL='L')  and (ACSIN = 'Y')  then prob06740  = 1 ;
else prob06740  = 0 ;



/* Logic check variable prob06750 :
Check that if the first condition (ACLIC IN ('Y','N')) is true then the second condition (ACSIN = 'N') cannot also be true.  */

if (ACLIC IN ('Y','N'))  and (ACSIN = 'N')  then prob06750  = 1 ;
else prob06750  = 0 ;



/* Logic check variable prob06760 :
Check that if the first condition (ACLIC='L') is true then the second condition (ACSIN = 'Y') cannot also be true.  */

if (ACLIC='L')  and (ACSIN = 'Y')  then prob06760  = 1 ;
else prob06760  = 0 ;



/* Logic check variable prob06770 :
Check that if the first condition (ACADV IN ('Y','N')) is true then the second condition (ACSIN = 'N') cannot also be true.  */

if (ACADV IN ('Y','N'))  and (ACSIN = 'N')  then prob06770  = 1 ;
else prob06770  = 0 ;



/* Logic check variable prob06780 :
Check that if the first condition (ACADV='L') is true then the second condition (ACSIN = 'Y') cannot also be true.  */

if (ACADV='L')  and (ACSIN = 'Y')  then prob06780  = 1 ;
else prob06780  = 0 ;



/* Logic check variable prob06790 :
Check that if the first condition (ACEM IN ('Y','N')) is true then the second condition (ACSIN = 'N') cannot also be true.  */

if (ACEM IN ('Y','N'))  and (ACSIN = 'N')  then prob06790  = 1 ;
else prob06790  = 0 ;



/* Logic check variable prob06800 :
Check that if the first condition (ACEM='L') is true then the second condition (ACSIN = 'Y') cannot also be true.  */

if (ACEM='L')  and (ACSIN = 'Y')  then prob06800  = 1 ;
else prob06800  = 0 ;



/* Logic check variable prob06810 :
Check that if the first condition (ACINT IN ('Y','N')) is true then the second condition (ACSIN = 'N') cannot also be true.  */

if (ACINT IN ('Y','N'))  and (ACSIN = 'N')  then prob06810  = 1 ;
else prob06810  = 0 ;



/* Logic check variable prob06820 :
Check that if the first condition (ACINT='L') is true then the second condition (ACSIN = 'Y') cannot also be true.  */

if (ACINT='L')  and (ACSIN = 'Y')  then prob06820  = 1 ;
else prob06820  = 0 ;



/* Logic check variable prob06830 :
Check that if the first condition (ACOT IN ('Y','N')) is true then the second condition (ACSIN = 'N') cannot also be true.  */

if (ACOT IN ('Y','N'))  and (ACSIN = 'N')  then prob06830  = 1 ;
else prob06830  = 0 ;



/* Logic check variable prob06840 :
Check that if the first condition (ACOT='L') is true then the second condition (ACSIN = 'Y') cannot also be true.  */

if (ACOT='L')  and (ACSIN = 'Y')  then prob06840  = 1 ;
else prob06840  = 0 ;



/* Logic check variable prob06850 :
Check that if the first condition (ACOTV='L') is true then the second condition (ACOT = 'Y') cannot also be true.  */

if (ACOTV='L')  and (ACOT = 'Y')  then prob06850  = 1 ;
else prob06850  = 0 ;



/* Logic check variable prob06860 :
Check that if the first condition (ACOTV NE 'L') is true then the second condition (ACOT NE 'Y') cannot also be true.  */

if (ACOTV NE 'L')  and (ACOT NE 'Y')  then prob06860  = 1 ;
else prob06860  = 0 ;



/* Logic check variable prob07470 :
Check that BAIND always meets the condition (BAIND ne 'L').  */

if (BAIND ne 'L')  then prob07470  = 0 ;
else prob07470  = 1 ;



/* Logic check variable prob07480 :
Check that if the first condition (FTPRET IN ('Y','N')) is true then the second condition (WRKG='N') cannot also be true.  */

if (FTPRET IN ('Y','N'))  and (WRKG='N')  then prob07480  = 1 ;
else prob07480  = 0 ;



/* Logic check variable prob07490 :
Check that if the first condition (FTPRET='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (FTPRET='L')  and (WRKG='Y')  then prob07490  = 1 ;
else prob07490  = 0 ;



/* Logic check variable prob07500 :
Check that if the first condition ((1949<=FTPRTYR<=2010) OR FTPRTYR=9999) is true then the second condition (FTPRET IN ('N','L')) cannot also be true.  */

if ((1949<=FTPRTYR<=2010) OR FTPRTYR=9999)  and (FTPRET IN ('N','L'))  then prob07500  = 1 ;
else prob07500  = 0 ;



/* Logic check variable prob07510 :
Check that if the first condition (FTPRTYR=9998) is true then the second condition (FTPRET = 'Y') cannot also be true.  */

if (FTPRTYR=9998)  and (FTPRET = 'Y')  then prob07510  = 1 ;
else prob07510  = 0 ;



/* Logic check variable prob07520 :
Check that if the first condition (NEWBUS='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (NEWBUS='L')  and (WRKG='Y')  then prob07520  = 1 ;
else prob07520  = 0 ;



/* Logic check variable prob07530 :
Check that if the first condition (NEWBUS IN ('Y','N')) is true then the second condition (WRKG='N') cannot also be true.  */

if (NEWBUS IN ('Y','N'))  and (WRKG='N')  then prob07530  = 1 ;
else prob07530  = 0 ;



/* Logic check variable prob07740 :
Check that if the first condition ((1949<=TENYR<=2010) OR TENYR=9999) is true then the second condition (TENSTA NE  '3') cannot also be true.  */

if ((1949<=TENYR<=2010) OR TENYR=9999)  and (TENSTA NE  '3')  then prob07740  = 1 ;
else prob07740  = 0 ;



/* Logic check variable prob07750 :
Check that if the first condition (TENYR=9998) is true then the second condition (TENSTA= '3') cannot also be true.  */

if (TENYR=9998)  and (TENSTA= '3')  then prob07750  = 1 ;
else prob07750  = 0 ;



/* Logic check variable prob07760 :
Check that if the first condition (SATSAL='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (SATSAL='L')  and (WRKG='Y')  then prob07760  = 1 ;
else prob07760  = 0 ;



/* Logic check variable prob07770 :
Check that if the first condition (SATSAL IN ('1','2','3','4')) is true then the second condition (WRKG='N') cannot also be true.  */

if (SATSAL IN ('1','2','3','4'))  and (WRKG='N')  then prob07770  = 1 ;
else prob07770  = 0 ;



/* Logic check variable prob07780 :
Check that if the first condition (SATBEN='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (SATBEN='L')  and (WRKG='Y')  then prob07780  = 1 ;
else prob07780  = 0 ;



/* Logic check variable prob07790 :
Check that if the first condition (SATBEN IN ('1','2','3','4')) is true then the second condition (WRKG='N') cannot also be true.  */

if (SATBEN IN ('1','2','3','4'))  and (WRKG='N')  then prob07790  = 1 ;
else prob07790  = 0 ;



/* Logic check variable prob07800 :
Check that if the first condition (SATSEC='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (SATSEC='L')  and (WRKG='Y')  then prob07800  = 1 ;
else prob07800  = 0 ;



/* Logic check variable prob07810 :
Check that if the first condition (SATSEC IN ('1','2','3','4')) is true then the second condition (WRKG='N') cannot also be true.  */

if (SATSEC IN ('1','2','3','4'))  and (WRKG='N')  then prob07810  = 1 ;
else prob07810  = 0 ;



/* Logic check variable prob07820 :
Check that if the first condition (SATLOC='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (SATLOC='L')  and (WRKG='Y')  then prob07820  = 1 ;
else prob07820  = 0 ;



/* Logic check variable prob07830 :
Check that if the first condition (SATLOC IN ('1','2','3','4')) is true then the second condition (WRKG='N') cannot also be true.  */

if (SATLOC IN ('1','2','3','4'))  and (WRKG='N')  then prob07830  = 1 ;
else prob07830  = 0 ;



/* Logic check variable prob07840 :
Check that if the first condition (SATADV='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (SATADV='L')  and (WRKG='Y')  then prob07840  = 1 ;
else prob07840  = 0 ;



/* Logic check variable prob07850 :
Check that if the first condition (SATADV IN ('1','2','3','4')) is true then the second condition (WRKG='N') cannot also be true.  */

if (SATADV IN ('1','2','3','4'))  and (WRKG='N')  then prob07850  = 1 ;
else prob07850  = 0 ;



/* Logic check variable prob07860 :
Check that if the first condition (SATCHAL='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (SATCHAL='L')  and (WRKG='Y')  then prob07860  = 1 ;
else prob07860  = 0 ;



/* Logic check variable prob07870 :
Check that if the first condition (SATCHAL IN ('1','2','3','4')) is true then the second condition (WRKG='N') cannot also be true.  */

if (SATCHAL IN ('1','2','3','4'))  and (WRKG='N')  then prob07870  = 1 ;
else prob07870  = 0 ;



/* Logic check variable prob07880 :
Check that if the first condition (SATRESP='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (SATRESP='L')  and (WRKG='Y')  then prob07880  = 1 ;
else prob07880  = 0 ;



/* Logic check variable prob07890 :
Check that if the first condition (SATRESP IN ('1','2','3','4')) is true then the second condition (WRKG='N') cannot also be true.  */

if (SATRESP IN ('1','2','3','4'))  and (WRKG='N')  then prob07890  = 1 ;
else prob07890  = 0 ;



/* Logic check variable prob07900 :
Check that if the first condition (SATIND='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (SATIND='L')  and (WRKG='Y')  then prob07900  = 1 ;
else prob07900  = 0 ;



/* Logic check variable prob07910 :
Check that if the first condition (SATIND IN ('1','2','3','4')) is true then the second condition (WRKG='N') cannot also be true.  */

if (SATIND IN ('1','2','3','4'))  and (WRKG='N')  then prob07910  = 1 ;
else prob07910  = 0 ;



/* Logic check variable prob07920 :
Check that if the first condition (SATSOC='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (SATSOC='L')  and (WRKG='Y')  then prob07920  = 1 ;
else prob07920  = 0 ;



/* Logic check variable prob07930 :
Check that if the first condition (SATSOC IN ('1','2','3','4')) is true then the second condition (WRKG='N') cannot also be true.  */

if (SATSOC IN ('1','2','3','4'))  and (WRKG='N')  then prob07930  = 1 ;
else prob07930  = 0 ;



/* Logic check variable prob07940 :
Check that if the first condition (PJOCNA IN ('Y','N')) is true then the second condition (WRKG = 'N' OR HRSWK>=35) cannot also be true.  */

if (PJOCNA IN ('Y','N'))  and (WRKG = 'N' OR HRSWK>=35)  then prob07940  = 1 ;
else prob07940  = 0 ;



/* Logic check variable prob07950 :
Check that if the first condition (PJOCNA='L') is true then the second condition (WRKG = 'Y' AND HRSWK<35) cannot also be true.  */

if (PJOCNA='L')  and (WRKG = 'Y' AND HRSWK<35)  then prob07950  = 1 ;
else prob07950  = 0 ;



/* Logic check variable prob07960 :
Check that if the first condition (PJHAJ IN ('Y','N')) is true then the second condition (WRKG = 'N' OR HRSWK>=35) cannot also be true.  */

if (PJHAJ IN ('Y','N'))  and (WRKG = 'N' OR HRSWK>=35)  then prob07960  = 1 ;
else prob07960  = 0 ;



/* Logic check variable prob07970 :
Check that if the first condition (PJHAJ='L') is true then the second condition (WRKG = 'Y' AND HRSWK<35) cannot also be true.  */

if (PJHAJ='L')  and (WRKG = 'Y' AND HRSWK<35)  then prob07970  = 1 ;
else prob07970  = 0 ;



/* Logic check variable prob07980 :
Check that if the first condition (JOBINS='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (JOBINS='L')  and (WRKG='Y')  then prob07980  = 1 ;
else prob07980  = 0 ;



/* Logic check variable prob07990 :
Check that if the first condition (JOBINS IN ('Y','N')) is true then the second condition (WRKG='N') cannot also be true.  */

if (JOBINS IN ('Y','N'))  and (WRKG='N')  then prob07990  = 1 ;
else prob07990  = 0 ;



/* Logic check variable prob08000 :
Check that if the first condition (JOBPENS='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (JOBPENS='L')  and (WRKG='Y')  then prob08000  = 1 ;
else prob08000  = 0 ;



/* Logic check variable prob08010 :
Check that if the first condition (JOBPENS IN ('Y','N')) is true then the second condition (WRKG='N') cannot also be true.  */

if (JOBPENS IN ('Y','N'))  and (WRKG='N')  then prob08010  = 1 ;
else prob08010  = 0 ;



/* Logic check variable prob08020 :
Check that if the first condition (JOBPROFT='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (JOBPROFT='L')  and (WRKG='Y')  then prob08020  = 1 ;
else prob08020  = 0 ;



/* Logic check variable prob08030 :
Check that if the first condition (JOBPROFT IN ('Y','N')) is true then the second condition (WRKG='N') cannot also be true.  */

if (JOBPROFT IN ('Y','N'))  and (WRKG='N')  then prob08030  = 1 ;
else prob08030  = 0 ;



/* Logic check variable prob08040 :
Check that if the first condition (JOBVAC='L') is true then the second condition (WRKG='Y') cannot also be true.  */

if (JOBVAC='L')  and (WRKG='Y')  then prob08040  = 1 ;
else prob08040  = 0 ;



/* Logic check variable prob08050 :
Check that if the first condition (JOBVAC IN ('Y','N')) is true then the second condition (WRKG='N') cannot also be true.  */

if (JOBVAC IN ('Y','N'))  and (WRKG='N')  then prob08050  = 1 ;
else prob08050  = 0 ;



/* Logic check variable prob08060 :
Check that if the first condition (FSDOD='Y') is true then the second condition (GOVSUP NE '1') cannot also be true.  */

if (FSDOD='Y')  and (GOVSUP NE '1')  then prob08060  = 1 ;
else prob08060  = 0 ;



/* Logic check variable prob08070 :
Check that if the first condition (FSDOD='L') is true then the second condition (GOVSUP='1') cannot also be true.  */

if (FSDOD='L')  and (GOVSUP='1')  then prob08070  = 1 ;
else prob08070  = 0 ;



/* Logic check variable prob08080 :
Check that if the first condition (FSDED='Y') is true then the second condition (GOVSUP NE '1') cannot also be true.  */

if (FSDED='Y')  and (GOVSUP NE '1')  then prob08080  = 1 ;
else prob08080  = 0 ;



/* Logic check variable prob08090 :
Check that if the first condition (FSDED='L') is true then the second condition (GOVSUP='1') cannot also be true.  */

if (FSDED='L')  and (GOVSUP='1')  then prob08090  = 1 ;
else prob08090  = 0 ;



/* Logic check variable prob08100 :
Check that if the first condition (FSDOE='Y') is true then the second condition (GOVSUP NE '1') cannot also be true.  */

if (FSDOE='Y')  and (GOVSUP NE '1')  then prob08100  = 1 ;
else prob08100  = 0 ;



/* Logic check variable prob08110 :
Check that if the first condition (FSDOE='L') is true then the second condition (GOVSUP='1') cannot also be true.  */

if (FSDOE='L')  and (GOVSUP='1')  then prob08110  = 1 ;
else prob08110  = 0 ;



/* Logic check variable prob08120 :
Check that if the first condition (FSNIH='Y') is true then the second condition (GOVSUP NE '1') cannot also be true.  */

if (FSNIH='Y')  and (GOVSUP NE '1')  then prob08120  = 1 ;
else prob08120  = 0 ;



/* Logic check variable prob08130 :
Check that if the first condition (FSNIH='L') is true then the second condition (GOVSUP='1') cannot also be true.  */

if (FSNIH='L')  and (GOVSUP='1')  then prob08130  = 1 ;
else prob08130  = 0 ;



/* Logic check variable prob08140 :
Check that if the first condition (FSHHS='Y') is true then the second condition (GOVSUP NE '1') cannot also be true.  */

if (FSHHS='Y')  and (GOVSUP NE '1')  then prob08140  = 1 ;
else prob08140  = 0 ;



/* Logic check variable prob08150 :
Check that if the first condition (FSHHS='L') is true then the second condition (GOVSUP='1') cannot also be true.  */

if (FSHHS='L')  and (GOVSUP='1')  then prob08150  = 1 ;
else prob08150  = 0 ;



/* Logic check variable prob08160 :
Check that if the first condition (FSNSF='Y') is true then the second condition (GOVSUP NE '1') cannot also be true.  */

if (FSNSF='Y')  and (GOVSUP NE '1')  then prob08160  = 1 ;
else prob08160  = 0 ;



/* Logic check variable prob08170 :
Check that if the first condition (FSNSF='L') is true then the second condition (GOVSUP='1') cannot also be true.  */

if (FSNSF='L')  and (GOVSUP='1')  then prob08170  = 1 ;
else prob08170  = 0 ;



/* Logic check variable prob08180 :
Check that if the first condition (FSOT='Y') is true then the second condition (GOVSUP NE '1') cannot also be true.  */

if (FSOT='Y')  and (GOVSUP NE '1')  then prob08180  = 1 ;
else prob08180  = 0 ;



/* Logic check variable prob08190 :
Check that if the first condition (FSOT='L') is true then the second condition (GOVSUP='1') cannot also be true.  */

if (FSOT='L')  and (GOVSUP='1')  then prob08190  = 1 ;
else prob08190  = 0 ;



/* Logic check variable prob08200 :
Check that if the first condition (FSOTV='L') is true then the second condition (FSOT= 'Y') cannot also be true.  */

if (FSOTV='L')  and (FSOT= 'Y')  then prob08200  = 1 ;
else prob08200  = 0 ;



/* Logic check variable prob08210 :
Check that if the first condition (FSOTV NE 'L') is true then the second condition (FSOT NE 'Y') cannot also be true.  */

if (FSOTV NE 'L')  and (FSOT NE 'Y')  then prob08210  = 1 ;
else prob08210  = 0 ;



/* Logic check variable prob08220 :
Check that if the first condition (FSDK='Y') is true then the second condition (GOVSUP NE '1') cannot also be true.  */

if (FSDK='Y')  and (GOVSUP NE '1')  then prob08220  = 1 ;
else prob08220  = 0 ;



/* Logic check variable prob08230 :
Check that if the first condition (FSDK='L') is true then the second condition (GOVSUP='1') cannot also be true.  */

if (FSDK='L')  and (GOVSUP='1')  then prob08230  = 1 ;
else prob08230  = 0 ;



/* Logic check variable prob08240 :
Check that if the first condition (PROMTGI IN ('Y','N')) is true then the second condition (LWNVR = 'Y') cannot also be true.  */

if (PROMTGI IN ('Y','N'))  and (LWNVR = 'Y')  then prob08240  = 1 ;
else prob08240  = 0 ;



/* Logic check variable prob08250 :
Check that if the first condition (PROMTGI='L') is true then the second condition (LWNVR NE 'Y') cannot also be true.  */

if (PROMTGI='L')  and (LWNVR NE 'Y')  then prob08250  = 1 ;
else prob08250  = 0 ;



/* Logic check variable prob08260 :
Check that if the first condition (0<=PRMBR<=97 OR PRMBR = 99) is true then the second condition (LWNVR = 'Y') cannot also be true.  */

if (0<=PRMBR<=97 OR PRMBR = 99)  and (LWNVR = 'Y')  then prob08260  = 1 ;
else prob08260  = 0 ;



/* Logic check variable prob08270 :
Check that if the first condition (PRMBR='98') is true then the second condition (LWNVR NE 'Y') cannot also be true.  */

if (PRMBR='98')  and (LWNVR NE 'Y')  then prob08270  = 1 ;
else prob08270  = 0 ;



/* Logic check variable prob08280 :
Check that if the first condition (FACSAL IN ('1','2','3','4')) is true then the second condition (LWNVR = 'Y') cannot also be true.  */

if (FACSAL IN ('1','2','3','4'))  and (LWNVR = 'Y')  then prob08280  = 1 ;
else prob08280  = 0 ;



/* Logic check variable prob08290 :
Check that if the first condition (FACSAL='L') is true then the second condition (LWNVR NE 'Y') cannot also be true.  */

if (FACSAL='L')  and (LWNVR NE 'Y')  then prob08290  = 1 ;
else prob08290  = 0 ;



/* Logic check variable prob08300 :
Check that if the first condition (FACBEN IN ('1','2','3','4')) is true then the second condition (LWNVR = 'Y') cannot also be true.  */

if (FACBEN IN ('1','2','3','4'))  and (LWNVR = 'Y')  then prob08300  = 1 ;
else prob08300  = 0 ;



/* Logic check variable prob08310 :
Check that if the first condition (FACBEN='L') is true then the second condition (LWNVR NE 'Y') cannot also be true.  */

if (FACBEN='L')  and (LWNVR NE 'Y')  then prob08310  = 1 ;
else prob08310  = 0 ;



/* Logic check variable prob08320 :
Check that if the first condition (FACSEC IN ('1','2','3','4')) is true then the second condition (LWNVR = 'Y') cannot also be true.  */

if (FACSEC IN ('1','2','3','4'))  and (LWNVR = 'Y')  then prob08320  = 1 ;
else prob08320  = 0 ;



/* Logic check variable prob08330 :
Check that if the first condition (FACSEC='L') is true then the second condition (LWNVR NE 'Y') cannot also be true.  */

if (FACSEC='L')  and (LWNVR NE 'Y')  then prob08330  = 1 ;
else prob08330  = 0 ;



/* Logic check variable prob08340 :
Check that if the first condition (FACLOC IN ('1','2','3','4')) is true then the second condition (LWNVR = 'Y') cannot also be true.  */

if (FACLOC IN ('1','2','3','4'))  and (LWNVR = 'Y')  then prob08340  = 1 ;
else prob08340  = 0 ;



/* Logic check variable prob08350 :
Check that if the first condition (FACLOC='L') is true then the second condition (LWNVR NE 'Y') cannot also be true.  */

if (FACLOC='L')  and (LWNVR NE 'Y')  then prob08350  = 1 ;
else prob08350  = 0 ;



/* Logic check variable prob08360 :
Check that if the first condition (FACADV IN ('1','2','3','4')) is true then the second condition (LWNVR = 'Y') cannot also be true.  */

if (FACADV IN ('1','2','3','4'))  and (LWNVR = 'Y')  then prob08360  = 1 ;
else prob08360  = 0 ;



/* Logic check variable prob08370 :
Check that if the first condition (FACADV='L') is true then the second condition (LWNVR NE 'Y') cannot also be true.  */

if (FACADV='L')  and (LWNVR NE 'Y')  then prob08370  = 1 ;
else prob08370  = 0 ;



/* Logic check variable prob08380 :
Check that if the first condition (FACCHAL IN ('1','2','3','4')) is true then the second condition (LWNVR = 'Y') cannot also be true.  */

if (FACCHAL IN ('1','2','3','4'))  and (LWNVR = 'Y')  then prob08380  = 1 ;
else prob08380  = 0 ;



/* Logic check variable prob08390 :
Check that if the first condition (FACCHAL='L') is true then the second condition (LWNVR NE 'Y') cannot also be true.  */

if (FACCHAL='L')  and (LWNVR NE 'Y')  then prob08390  = 1 ;
else prob08390  = 0 ;



/* Logic check variable prob08400 :
Check that if the first condition (FACRESP IN ('1','2','3','4')) is true then the second condition (LWNVR = 'Y') cannot also be true.  */

if (FACRESP IN ('1','2','3','4'))  and (LWNVR = 'Y')  then prob08400  = 1 ;
else prob08400  = 0 ;



/* Logic check variable prob08410 :
Check that if the first condition (FACRESP='L') is true then the second condition (LWNVR NE 'Y') cannot also be true.  */

if (FACRESP='L')  and (LWNVR NE 'Y')  then prob08410  = 1 ;
else prob08410  = 0 ;



/* Logic check variable prob08420 :
Check that if the first condition (FACIND IN ('1','2','3','4')) is true then the second condition (LWNVR = 'Y') cannot also be true.  */

if (FACIND IN ('1','2','3','4'))  and (LWNVR = 'Y')  then prob08420  = 1 ;
else prob08420  = 0 ;



/* Logic check variable prob08430 :
Check that if the first condition (FACIND='L') is true then the second condition (LWNVR NE 'Y') cannot also be true.  */

if (FACIND='L')  and (LWNVR NE 'Y')  then prob08430  = 1 ;
else prob08430  = 0 ;



/* Logic check variable prob08440 :
Check that if the first condition (FACSOC IN ('1','2','3','4')) is true then the second condition (LWNVR = 'Y') cannot also be true.  */

if (FACSOC IN ('1','2','3','4'))  and (LWNVR = 'Y')  then prob08440  = 1 ;
else prob08440  = 0 ;



/* Logic check variable prob08450 :
Check that if the first condition (FACSOC='L') is true then the second condition (LWNVR NE 'Y') cannot also be true.  */

if (FACSOC='L')  and (LWNVR NE 'Y')  then prob08450  = 1 ;
else prob08450  = 0 ;



/* Logic check variable prob08460 :
Check that if the first condition (ACCCEP IN ('Y','N')) is true then the second condition (ACSIN NE 'Y') cannot also be true.  */

if (ACCCEP IN ('Y','N'))  and (ACSIN NE 'Y')  then prob08460  = 1 ;
else prob08460  = 0 ;



/* Logic check variable prob08470 :
Check that if the first condition (ACCCEP='L') is true then the second condition (ACSIN NE 'N') cannot also be true.  */

if (ACCCEP='L')  and (ACSIN NE 'N')  then prob08470  = 1 ;
else prob08470  = 0 ;



/* Logic check variable prob08480 :
Check that DIFCOGN always meets the condition (DIFCOGN NE 'L').  */

if (DIFCOGN NE 'L')  then prob08480  = 0 ;
else prob08480  = 1 ;



