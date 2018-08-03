#!/bin/bash

for f in *
do
    echo "looking for figure: $f"
    file=`find .. -name '*.Rmd' -exec grep $f {} \;`
    if [[ -z $file ]]
    then
	echo "COULDN'T FIND ANYTHING"
    else
	echo $file
    fi

    echo
done
