# First you'll have to concat a search term:
echo "What terms?"; read terms; terms="${terms// /+}"
# Insert in link like:
# "http://images.google.com/images?q=${terms}&svnum=30&start=0"
# Bash has no concept of "pages", so basically you'll iterate through the "start=$n" part until you're done 
# or until someone comes up with a less crude way:
for n in $(seq 0 10); do links -dump -source \
 "http://images.google.com/images?q=${terms}&svnum=30&start=${n}" \
 | grep "href=/imgres?imgurl=" | tr "<>" " " | while read l; do for i in $l; do case "${i}" in href=/imgres*) \
 i="${i//&imgrefurl=/ }"; echo "${i//href=\/imgres?imgurl=/ }"|awk '{print $1}'|grep -i "\.jp";; esac; done; done
done
# Now you should have a listing of images on stdout.
# Pipe output to a temporary file and wget from that file.



term=savanah

wget "https://www.google.com/search?tbm=isch&q=sunset&vnum=10&amp"

count=10
imagelink=$(wget --user-agent 'Mozilla/5.0' -qO - "www.google.uk/search?q=sunset\&tbm=isch" | sed 's/</\n</g' | grep '<img' | head -n"$count" | tail -n1 | sed 's/.*src="\([^"]*\)".*/\1/')
wget $imagelink 


wget --user-agent 'Mozilla/5.0' -qO - "www.google.uk/search?q=sunset\&tbm=isch" | sed 's/</\n</g'| grep '<img'| head -n"$count" | tail -n1 | sed 's/.*src="\([^"]*\)".*/\1/'