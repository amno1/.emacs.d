#!/bin/bash

echo starting in $PWD

function clean-eln {

    #echo working in $PWD

    #ls *.eln 2>/dev/null
    rm *.eln 2>/dev/null
    rm -fr eln-* 2>/dev/null
    
    for file in *
    do
        [[ -d "$file" ]] && {
            cd "$file"
            clean-eln
            cd ..
        }
    done
}

clean-eln
echo done in $PWD
