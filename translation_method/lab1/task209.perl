while (<>) {
	s/\([^\)]*\)/()/g;
	print;
}