#!/usr/bin/perl 
#
while (<>) {
   chomp();

   if ($_ =~ /DateRep/) { next }

   ## DateRep;CountryExp;NewConfCases;NewDeaths;GeoId;Gaul1Nuts1;EU
   #($Day, $Country, $NewCC, $NewDD, $GeoId, $Gaul1Nuts1, $EU) = split /;/, $_;
   # zmiana 17-03
   # DateRep;Day;Month;Year;Cases;Deaths;Countries and territories;GeoId
   ($Day, $DDay, $DMonth, $Year, $NewCC, $NewDD, $CandT, $GeoId ) = split /;/, $_;

   if ($GeoId eq 'EL') { $GeoId = 'GR' } ## Greece
   if ($GeoId eq 'UK') { $GeoId = 'GB' } ## UK

   ##($d, $m, $y) = split /\./, $Day;
   ($y, $m, $d) = split /[\.\-]/, $Day;

   $Day = sprintf "%4i-%02i-%02i", $y, $m, $d;

   $C{"$Day"}{"$GeoId"} = $NewCC; # cases
   $D{"$Day"}{"$GeoId"} = $NewDD; # deaths

   $Days{"$Day"}=1;
   $Countries{$GeoId}=1;
   ##$Id2Name{$GeoId}=$Country;
   $Id2Name{$GeoId}=$CandT;
}

##  Init
#for $c (sort keys  %Countries ) { $vCCur{$c}='NA'; $vDCur{$c} ='NA'}
for $c (sort keys  %Countries ) { $vCCur{$c}=0; $vDCur{$c} =0}

print "date;id;country;newc;newd;totalc;totald\n";
##
for $d (sort keys  %Days ) {
    for $c (sort keys  %Countries ) {
      if (exists $C{"$d"}{"$c"}) { 
            $vC = $C{$d}{$c};
            $vCCur{$c} += $vC;
        } else { 
            #$vC = 'NA'
            $vC = 0;
      }

      if (exists $D{"$d"}{"$c"}) { 
            $vD = $D{$d}{$c}; 
            $vDCur{$c} += $vD; } 
      else { 
            ##$vD = 'NA'
            $vD = 0;
      }
      ####
      #if ($vCCur{$c} == 0 && $vDCur{$c} == 0) { $vCCur{$c} = 'NA'; $vDCur{$c} = 'NA' }
      #########
      #if ($vCCur{$c} > 0 && $vC eq 'NA') { $vC = 0}
      #if ($vCCur{$c} > 0 && $vD eq 'NA') { $vD = 0}
      #########
      print "$d;$c;$Id2Name{$c};$vC;$vD;$vCCur{$c};$vDCur{$c}\n";
   }
}
