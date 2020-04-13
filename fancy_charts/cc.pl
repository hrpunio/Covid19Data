use Locale::Codes::Country;

## XK = XKX (Kosowo)
## EL = GR
while (<>) {
  chomp();
  ($c, $i2, $o) = split /;/, $_;
  $i3 = uc ( country_code2code($i2, 'alpha-2', 'alpha-3'));    
  print "$c;$i2;$i3;$o\n";
  
}
