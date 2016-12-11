proc import datafile= '"\\client\C$\Users\anobs\Documents\GitHub\MSDS_6372_Project_3_Adverbs\Data\booksDF2.csv' out = adverbs 
dbms=csv replace;
guessingrows = 214 ;
getnames = yes;;
run;

proc print data = adverbs; run;

data adverbs2; set adverbs;
Per_Small = Per_Small /100;
Per_Medium = Per_Medium / 100;
Per_Large  = Per_Large / 100;
run;

proc print data = adverbs2; run;


proc means data = adverbs2 mean nway ;
class author;
var Per_Small Per_Medium Per_Large little without other nothing again before these least about those though after through together where under never right first always great there others still;
output out = adverbmeans mean=mean;
run;


proc print data = adverbmeans; run;


proc logistic data = adverbs3;
*class author (ref = '1');
model authorID = Per_Small Per_Medium Per_Large little without other nothing again before these least about those though after through together where under never right first always great there others still / link = glogit;
run;

proc cluster data = adverbs2 simple method = centroid rmsstd rsquare out = tree;
id author;
var Per_Small Per_Medium Per_Large little without other nothing again before these least about those though after through together where under never right first always great there others still;
run;

proc tree ;

run;
