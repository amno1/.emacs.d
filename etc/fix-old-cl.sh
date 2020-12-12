#!/bin/env bash

do_subst () {
    sed -i -- "s/(incf /(cl-incf /g" "$1"
    sed -i -- "s/(decf /(cl-decf /g" "$1"
    sed -i -- "s/(flet /(cl-flet /g" "$1"
    sed -i -- "s/(delete-if/(cl-delete-if /g" "$1"
    sed -i -- "s/(lexical-let/(let /g" "$1"
    sed -i -- "s/(lexical-let*/(let*/g" "$1"
    sed -i -- "s/(labels/(cl-labels /g" "$1"
    sed -i -- "s/(loop for /(cl-loop for /g" "$1"
    sed -i -- "s/(loop with /(cl-loop with /g" "$1"
    sed -i -- "s/(defstruct /(cl-defstruct /g" "$1"
    sed -i -- "s/require 'cl)/require 'cl-lib)/g" "$1"
    sed -i -- "s/(destructuring-bind/(cl-destructuring-bind/g" "$1"
}

do_files() {
    
    echo "$PWD"
    for file in *.el
    do
        echo "$file"
        do_subst "$file"
    done
}

for file in *
do
    echo "$file"
    [ -d "$file" ] && {
        cd "$file"
        do_files 
        cd ..
    }
done

