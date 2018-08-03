#!/bin/bash

# find rdata files used in Rmds
find .. -name '*.Rmd' -exec grep -i rdata {} \; | sort | sed 's/ [ ]*$//g' | uniq
