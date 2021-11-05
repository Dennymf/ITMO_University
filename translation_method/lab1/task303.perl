use List::MoreUtils qw/ uniq /;

@sites = ();

my $line;
while (defined($line = <>)) {
	if ($line =~ /(.*?)(<a href=")(.*?)(")(.*?)(>)/) {
	    #print $line;
		$line =~ s/(.*?)(")(.*?)(")(.*?)(>)(.*?)/$3/g;
		#print $line;
		$line =~ s/(.*?)(\/\/)//g;
		#print $line;
		$line =~ s/(\/|:|$)(.*)(\n)?//g;
		#print $line;
		push(@sites, $line);
	}
}

my @sites = uniq @sites;
my @sites = sort @sites;
print join("\n", @sites);