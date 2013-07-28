v/id="NurikabeTable"/d
s/<\/\?table[^>]*>//g
%s/<td >\(\d\)<\/td>/| \1 |/g
%s/<td >\(\d\d\)<\/td>/| \1|/g
%s/<td><img[^>]*><\/td>/|   |/g
%s/||/|/g
%s/<\/tr><tr>/\r/g
%s/<[^>]>//g
%s/<[^>]*>//g
%s/^.*$/@\0\r\0/g
%s/|\s*$/|/
g/@/s/[^@]/-/g
%s/@//
1co$
w
