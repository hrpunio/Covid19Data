use Locale::Codes::Country;

##country_code2code('PL', 'alpha-2', 'alpha-3');
## Life ex
open (LE, "le2019.csv");

while (<LE>) { chomp();
  ($tmp1, $iso3, $tmp2, $le) = split /,/, $_;
  $LE{$iso3} = $le; $CC{$iso3} = 1; }

close (LE);
############
open (GP, "gdp2016.csv");

while (<GP>) { chomp();
  ($tmp1, $iso3, $tmp2, $gp) = split /,/, $_;
  $GP{$iso3} = $gp; $CC{$iso3} = 1; }

close (GP);
############
open (CM, "cm2017.csv");

while (<CM>) { chomp();
  ($tmp1, $iso3, $tmp2, $cm) = split /,/, $_;
  $CM{$iso3} = $cm; $CC{$iso3} = 1; }

close (CM);
############
open (PP, "pop2018.csv");

while (<PP>) { chomp();
  ($iso3, $pp) = split /;/, $_;
  $PP{$iso3} = $pp; $CC{$iso3} = 1; }

close (PP);
###########
##########
open (COVID, "covid19_C.csv");

while (<COVID>) { chomp();
($date, $iso2, $country, $newc, $newd, $totalc, $totald) = split /;/, $_;

   $iso3 = uc ( country_code2code($iso2, 'alpha-2', 'alpha-3'));

   ###print STDERR "$iso3 $iso2 $totalc $totald\n";

   $Cases{$iso3} = $totalc;
   $Deaths{$iso3} = $totald;

   $CList{$iso3}=$country;

}
close (COVID);

###
print "iso3;lex2019;gdp2016;cm2017;pop2018;cases;deaths\n";

for $c ( sort keys %CList ) {
  $le = $LE{$c};
  $gp = $GP{$c};
  $cm = $CM{$c};
  $pp = $PP{$c};
  $cases = $Cases{$c};
  $deaths = $Deaths{$c};

  $country = $CList{$c};

  unless ( $le > 0 ) { $le = 'NA'} else { $le = sprintf "%.2f", $le; }
  unless ( $gp > 0 ) { $gp = 'NA'} else { $gp = sprintf "%i", $gp;};
  unless ( $cm > 0 ) { $cm = 'NA'} else { $cm = sprintf "%.2f", $cm;};
  unless ( $pp > 0 ) { $pp = 'NA'} else { $pp = sprintf "%i", $pp;};
  
  unless ( $c eq '') {
    printf "%s;%s;%s;%s;%s;%s;%i;%i\n", $c, $country, $le, $gp, $cm, $pp, $cases, $deaths;
  }
}
