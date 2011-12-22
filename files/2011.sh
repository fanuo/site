root=http://ngm.nationalgeographic.com
loc=/ngm/photo-contest/2011/entries/gallery/
type=(nature places people)

for kind in "${type[@]}"
do
	for week in {1..12}
	do
		i=1
		curl $root$loc$kind"-week-"$week"/#/" | 
		awk 'BEGIN {FS="\""} {if ($2=="wallpaper_monitor") print $4}' |
		while read line
		do
			curl -o $kind"-"$week"-"$i".jpg" "${root}${line}"
			let i=i+1
		done
	done
done