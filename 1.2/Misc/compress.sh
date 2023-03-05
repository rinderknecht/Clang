#!/bin/sh

tr '|' '\n' | sed -e 's/[^0-Fx]//g' | grep -v '^$' | (old=0; start=0; o=6; while read ab; do i=$(($ab)); if [ $((old+1)) != $i ]; then if [ $start == $old ]; then echo -n '| '; [ $old -lt 65536 ] && echo -n ' '; printf "0x%x " $old; o=$(((o+1)%7)); else [ $o -gt 5 ] && echo && o=0; echo -n '| '; [ $start -lt 65536 ] && echo -n ' '; printf "[0x%x-0x%x] " $start $old; [ $old -lt 65536 ] && echo -n ' '; o=$(((o+2)%7)); fi; [ $o == 0 ] && echo; start=$i; fi; old=$i; done) | tr "[:lower:]" "[:upper:]"; echo
