$start = 1;
$blank = 0;

while (<>) {
	s/<.*?>//g;
	if (/\S/) {
		$start = 0;
	}
	
	if ($start == 0) {
		if ($blank == 1 && /^\s+$/)
		{
			print "";
		} else {
			if(/^\s+$/) {
				$blank = 1;
			} else {
				if ($blank == 1) {
					print "\n";
				}
				$blank = 0;
				s/^(\s+)//g;
				s/(\s+)$//g;
				s/(\s+)/ /g;
				print;
				print "\n";
			}
		}
	}
}