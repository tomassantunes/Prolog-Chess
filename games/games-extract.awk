/\[[0-9]+\]/ { gsub ("[[\\]]", ""); CGAME=$1; }
/^[0-9]+\. / {
    if (GAME==CGAME) {
	s = ""
	for (i = 2; i <= NF; ++i) {
	    printf "%s%s", s, $i;
	    s = " ";
	}
	printf "\n";
    }
}
