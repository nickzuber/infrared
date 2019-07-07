#!/bin/bash

rgxFindFn="and ([a-zA-Z|_]+)"
rgxFindUnfinishedMatchStmt="\s+| ([a-zA-Z]+) _ ->"
rgxFindFinishedMatchStmt="\s+| ([a-zA-Z]+)"
rgxIsValidBlock="^[A-Z]"

begin=0
pass=0
fail=0

filePrinter="./InfraredParser/Printer.ml"

fnPrinterCoverage () {
  while read -r line
  do
    if [[ begin -eq 1 && -z $line ]]; then
      echo "pass $pass fail $fail"
      pass=0
      fail=0
    elif [[ $line =~ $rgxFindFn ]]; then
      echo "\033[1m\n${BASH_REMATCH[1]}\033[0m"
      begin=1
    elif [[ begin -eq 1 && $line =~ $rgxFindUnfinishedMatchStmt ]]; then
      local name=${BASH_REMATCH[1]}
      if [[ -n $name ]]; then
        echo " \033[31m×\033[39;0m $name"
        ((fail++))
      else
        [[ $line =~ $rgxFindFinishedMatchStmt ]]
        local name=${BASH_REMATCH[1]}
        if [[ -n $name && $name =~ $rgxIsValidBlock ]]; then
          echo " \033[32;1m✓\033[39;0m $name"
          ((pass++))
        fi
      fi
    fi
  done < "$filePrinter"
  begin=0
}

fnPrinterCoverage
