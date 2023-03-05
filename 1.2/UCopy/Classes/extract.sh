#!/bin/sh

for html in $(ls *.html); do
  printf "Processing $html..."
  base=$(basename $html .html)
    sed -n "s/U+\([[:alnum:]]\+\).*/\1/p" $html \
  | (start=-2; old=$start
     while read hex; do
       i=$(echo "obase=10;ibase=16;$hex" | bc)
       if test $((old+1)) -ne $i
       then if test $start -eq $old
            then if test $old -ne -2
                 then printf "| 0x%x\n" $old
                 fi
            else printf "| [0x%x-0x%x]\n" $start $old;
            fi
            start=$i
       fi
       old=$i
     done
     if test $start -eq $old
     then printf "| 0x%x\n" $old
     else printf "| [0x%x-0x%x]\n" $start $old
     fi) >| $base.txt
  echo " end."
done
