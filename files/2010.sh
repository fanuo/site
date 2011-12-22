root=http://ngm.nationalgeographic.com
root2=http://www.nationalgeographic.com
loc=/ngm/photo-contest/2010/entries/wallpaper/

for week in {1..11}
do
	i=1
	curl $root$loc"week-"$week"/" | 
	awk 'BEGIN {FS="\""} {if ($2=="wallpaper_link") print $4}' |
	while read line
	do
    curl "${root2}${line}" |
    awk 'BEGIN {FS="\""} {if ($2=="download_link") print $4}' |
    xargs -I % curl -o $week"-"$i".jpg" %
		let i=i+1
	done
done
