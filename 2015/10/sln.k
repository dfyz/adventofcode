in:"1321131112"

grp_ne:{(0,(1+&~~':x)) _ x}
grp:{:[(#x)>1;grp_ne[x];x]}
nxt:{,/{($#:x),(*x)}'grp[x]}

# (0$_a[2]) nxt/in
