#!/usr/bin/perl
# https://www.google.com/covid19/mobility/
use LWP::UserAgent;
use POSIX 'strftime';

my $sleepTime = 11;

%OECD = ('Australia' => 'AU',
'New Zealand' => 'NZ',
'Austria' => 'AT',
'Norway' => 'NO',
'Belgium' => 'BE',
'Poland' => 'PL',
'Canada' => 'CA',
'Portugal' => 'PT',
'Chile' => 'CL',
'Slovak Republic' => 'SK',
'Czech Republic' => 'CZ', 
'Slovenia' => 'SI',
'Denmark' => 'DK',
'Spain' => 'ES',
'Estonia' => 'EE',
'Sweden' => 'SE',
'Finland' => 'FI',
'Switzerland' => 'CH',
'France' => 'FR',
'Turkey' => 'TR',
'Germany' => 'DE',
'United Kingdom' => 'GB',
'Greece' => 'GR',
'Greece' => 'EL',
'United States' => 'US',
'Hungary' => 'HU',
'Iceland' => 'IS',
'Ireland' => 'IE',
'Argentina' => 'AR',
'Israel' => 'IL',
'Brazil' => 'BR',
'Italy' => 'IT',
'China' => 'CN',
'Japan' => 'JP',
'India' => 'IN',
'Korea' => 'KR',
'Indonesia' => 'ID',
'Luxembourg' => 'LU',
'Russian Federation' => 'RU',
'Mexico' => 'MX',
'Saudi Arabia' => 'SA',
'Netherlands' => 'NL',
'South Africa' => 'ZA', );

@oecd = values %OECD;

my $ua = LWP::UserAgent->new(agent => 'Mozilla/5.0', cookie_jar =>{});

my $date = "2020-03-29";

foreach $c (sort @oecd) {
   $PP="https://www.gstatic.com/covid19/mobility/${date}_${c}_Mobility_Report_en.pdf";
   print STDERR "==> $c\n";
   my $req = HTTP::Request->new(GET => $PP);
   my $res = $ua->request($req, "${date}_${c}_Mobility_Report_en.pdf");
   if ($res->is_success) { print $res->as_string; }
   else { print "Failed: ", $res->status_line, "\n"; }
   print STDERR "Sleeping $sleepTime\n";
   sleep $sleepTime;
}
