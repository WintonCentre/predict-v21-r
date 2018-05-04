#!/usr/bin/env Rscript --default-packages=jsonlite
json_args <- commandArgs(TRUE);

# extract args from the JSON key:value pairs and create globalenv vars named
# by the key with the paired value.
cargs <- fromJSON(json_args);
for(n in names(cargs)) {
  assign(n, eval(as.symbol(n), cargs));
}

#
# NB: iMac R wants "~/Library/R/3.3/library"
#     MBP R wants "/Library/Frameworks/R.framework/Versions/3.3/Resources/library");
#

#library("tidyverse", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library");
library("tidyverse", lib.loc="~/Library/R/3.4/library")

#
# NB: iMac wants /Library path first.
#
#.libPaths(new = "/Library/Frameworks/R.framework/Versions/3.4/Resources/library");
#.libPaths(new = "/Users/gmp26/Library/R/3.4/library");
library("utils");
library("tidyverse");

# this should force 14 figures in result. but doesn't work with data.frame inside list :(
options(digits = 14);

# execute Paul Pharoah's script
source("Predict-v2.1-2018-04-07.R");

# return only benefits2.1
print(toJSON(digits=14,
             list(benefits2.1 = data.frame(benefits2.1),
                  inputs = data.frame(age.start,
                                      screen,
                                      size,
                                      grade,
                                      nodes,
                                      er,
                                      her2,
                                      ki67,
                                      generation,
                                      horm,
                                      traz,
                                      bis,
                                      r.enabled))))
