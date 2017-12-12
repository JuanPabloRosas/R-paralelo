#!/bin/bash
for f in domains/*.pddl ### Outer for loop ###
do
   for g in problems/*.pddl ### Inner for loop ###
   do
        n=${f##*domain1m-} # Dominio
        m=${g##*problem1m-}  # Problema
        #test $m = $n && echo Equal || echo $n $m
        if [ $n = $m ]; 
        then
	    timeout 180 ./optic-clp $f $g -> planes/U/$n".txt"
            #echo "domain1m-"$n
            #echo "problem1m-"$m
        fi
  done
done
